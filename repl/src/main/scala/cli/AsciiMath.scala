package it.grypho.scala.leonardo
package cli

import parser.Parser

/** Reads MathLive's AsciiMath into Leonardo's own grammar, or refuses (issue F_0017 slice 2).
 *
 *  **Why AsciiMath and not LaTeX.**  A `<math-field>` holds LaTeX, and reading that would mean
 *  writing a parser for a *typesetting* language, where `\frac{d}{dx}` is layout rather than
 *  meaning and `f(x)` is ambiguous between application and multiplication.  But the vendored
 *  MathLive build already exports `convertLatexToAsciiMath`, and **AsciiMath's arithmetic core
 *  IS this grammar**: a LaTeX fraction arrives as `(a+b)/(c+d)`, `x^{2}` as `x^2`, `2 x` as
 *  `2x`, `\theta` as `theta`.  What remains is a bounded rewrite table, which is this file.
 *
 *  **It refuses on its own authority, and that is the load-bearing half.**  The grammar has no
 *  unknown-identifier error: an unrecognised name is a `_Variable` and juxtaposition is
 *  multiplication, so `sqrt(x)` handed to `Parser.parse` yields *the product of a free variable
 *  named `sqrt` with `x`* — an answer to a different question rather than a complaint.  Nothing
 *  here may pass text through in the hope that the parser will object.  The whitelist is
 *  `Parser.ReservedWords`, the same surface F_0030's REPL diagnostic uses, so the browser
 *  reader and the prompt cannot disagree about what counts as a function.
 *
 *  **What is not converted is refused by name.**  The binders are declined with the grammar's
 *  own spelling rather than pattern-matched out of an undocumented intermediate form;
 *  supporting them is a bounded follow-up, not a gap in the design.
 *
 *  It lives in `repl` rather than in `web` because it concerns the input *language*, not the
 *  browser: it emits grammar text and validates names exactly as `Session` does, so the two
 *  rules stay neighbours, and a terminal user pasting AsciiMath gets the same treatment.
 */
object AsciiMath:

  /** Names whose grammar spelling differs from the mathematical one.
   *
   *  `ToLatex` renders `asin` as `\arcsin` for the same reason in the other direction, so this
   *  table is that decision read backwards and the pair must stay in step.
   */
  private val Renamed: Map[String, String] =
    Map("arcsin" -> "asin", "arccos" -> "acos", "arctan" -> "atan")

  /** Infix words AsciiMath writes between bracket groups, as the grammar's call (F_0037).
   *
   *  `\binom{4}{k}` arrives as `(4) choose (k)`, which MathLive emits inside a summation
   *  often enough that refusing it would make the commonest `Σ` unusable.
   */
  private val Infix: Map[String, String] = Map("choose" -> "binom")

  /** Words that name something this reader does not build, mapped to the grammar spelling
   *  its refusal should suggest.
   *
   *  Smaller than it was: F_0033 taught the reader `int`, the `d/dx` fraction and `lim`, and
   *  F_0037 the two reductions — so each of those is consumed by a structured case in
   *  [[render]] before this map is ever reached, and what is left here is the **shape-less**
   *  spelling of each: a `lim` without its `(x->a)`, a `sum` without its `(k=lo)^hi`.  Only
   *  `oint` is refused outright, since a contour integral has no Leonardo counterpart and
   *  reading it as a plain integral would be a confident answer to a different question.
   */
  private val Binders: Map[String, String] =
    Map(
      "oint" -> "integral(f, x)",
      "lim"  -> "limit(f, x, a)",   "sum"  -> "sum(f, k, lo, hi)",
      "prod" -> "product(f, k, lo, hi)")

  /** The operator glyphs MathLive emits, in the grammar's ASCII.
   *
   *  `∣` is `\lvert`'s spelling of the bar, folded onto `|` so the abs pairing below sees
   *  one glyph; `∥` (`\|`, a norm) is deliberately NOT here — it is refused by name in
   *  [[leaf]], since nothing in the grammar means it.
   */
  private val Glyphs: Map[Char, String] =
    Map('≠' -> "!=", '≤' -> "<=", '≥' -> ">=", '×' -> "*", '÷' -> "/", '∣' -> "|")

  /** One lexical unit: a name, a number, or a single punctuation character. */
  private enum Tok:
    case Name(text: String)
    case Num(text: String)
    case Punct(text: String)

  /** A comma-separated bracket group, or a leaf.
   *
   *  The tree exists because three of the rewrites are *structural* — a radical needs its
   *  argument, a matrix its rows — and a regular expression over nested brackets is the wrong
   *  instrument for all three.
   */
  private enum Node:
    case Leaf(tok: Tok)
    case Group(items: List[List[Node]])

  /** Splits the source into names, numbers and punctuation, translating the Unicode glyphs.
   *
   *  Whitespace is dropped entirely: AsciiMath's spacing is presentational (` int  x d x`,
   *  `sin (x)`), and the grammar's own is not — `sin (x)` does not parse, because `"sin("` is
   *  one literal in the parser.
   */
  private def tokenise(source: String): List[Tok] =
    def loop(cs: List[Char], acc: List[Tok]): List[Tok] = cs match
      case Nil                        => acc.reverse
      case c :: rest if c.isWhitespace => loop(rest, acc)
      case c :: rest if c.isLetter     =>
        val (name, tail) = (c :: rest).span(ch => ch.isLetterOrDigit)
        loop(tail, Tok.Name(name.mkString) :: acc)
      case c :: rest if c.isDigit      =>
        val (num, tail) = (c :: rest).span(ch => ch.isDigit || ch == '.')
        loop(tail, Tok.Num(num.mkString) :: acc)
      case c :: rest =>
        Glyphs.get(c) match
          case Some(ascii) => loop(rest, Tok.Punct(ascii) :: acc)
          // `<=`, `>=`, `!=` and `->` arrive as two characters; keep them together so the
          // emitter never splits a relation, and so `->` can be recognised and refused.
          case None =>
            rest.headOption.filter(n => (c == '<' || c == '>' || c == '!' || c == '-') && (n == '=' || n == '>')) match
              case Some(n) => loop(rest.tail, Tok.Punct(s"$c$n") :: acc)
              case None    => loop(rest, Tok.Punct(c.toString) :: acc)
    loop(source.toList, Nil)

  /** Builds the bracket tree, or reports the bracket that has no partner. */
  private def parseGroups(toks: List[Tok]): Either[String, List[Node]] =
    def items(ts: List[Tok], item: List[Node], done: List[List[Node]])
        : Either[String, (List[List[Node]], List[Tok])] = ts match
      case Nil                      => Left("unbalanced '(' in the formula")
      case Tok.Punct(")") :: rest   => Right(((item.reverse :: done).reverse, rest))
      case Tok.Punct(",") :: rest   => items(rest, Nil, item.reverse :: done)
      case Tok.Punct("(") :: rest   =>
        items(rest, Nil, Nil).flatMap { (inner, tail) => items(tail, Node.Group(inner) :: item, done) }
      case t :: rest                => items(rest, Node.Leaf(t) :: item, done)

    def top(ts: List[Tok], acc: List[Node]): Either[String, List[Node]] = ts match
      case Nil                    => Right(acc.reverse)
      case Tok.Punct(")") :: _    => Left("unbalanced ')' in the formula")
      case Tok.Punct("(") :: rest =>
        items(rest, Nil, Nil).flatMap { (inner, tail) => top(tail, Node.Group(inner) :: acc) }
      case t :: rest              => top(rest, Node.Leaf(t) :: acc)

    top(toks, Nil)

  /** Whether a group is a matrix rather than ordinary nesting.
   *
   *  Every item must itself be a lone group — `((1,2),(3,4))` — and the shape must be
   *  *unambiguous*: `((a+b))` is one item holding one item and is plain double bracketing,
   *  while `((1,2))` is a one-row matrix because the inner group carries a comma.
   */
  private def isMatrix(items: List[List[Node]]): Boolean =
    items.forall { case List(Node.Group(_)) => true; case _ => false } &&
      (items.sizeIs > 1 || items.exists { case List(Node.Group(inner)) => inner.sizeIs > 1; case _ => false })

  /** Renders a node list, rewriting the shapes whose spelling differs, or refuses. */
  private def render(nodes: List[Node]): Either[String, String] = nodes match
    case Nil => Right("")

    //  int [_lo^hi] <integrand> d <v>  ->  integral(integrand, v[, lo, hi])  (F_0033).
    //  The differential is the delimiter, so unlike `lim` there is nothing to guess: the
    //  integrand is exactly what sits between the bounds and the matching `d`-pair.
    case Node.Leaf(Tok.Name("int")) :: rest =>
      renderIntegral(rest)

    //  sum|prod _(k=lo)^hi <body>  ->  sum|product(body, k, lo, hi)  (issue F_0037).  The
    //  spec is a group, exactly as `lim`'s is, so the two parse the same shape; the upper
    //  bound is a bare token or a group, as the integral's bounds are.
    case Node.Leaf(Tok.Name(w)) :: Node.Leaf(Tok.Punct("_")) :: Node.Group(List(spec))
        :: Node.Leaf(Tok.Punct("^")) :: hi :: rest if Reductions.contains(w) =>
      renderReduction(Reductions(w), spec, hi, rest)

    //  lim _(v->point[^dir]) <body>  ->  limit(body, v, point[, dir])  (F_0033).
    //  THE BODY IS THE REST OF THE CURRENT RUN, brackets being how a reader limits it -- the
    //  standard reading of the notation, and the same extent rule the derivative below uses,
    //  so there is one rule to learn rather than two.
    case Node.Leaf(Tok.Name("lim")) :: Node.Leaf(Tok.Punct("_")) :: Node.Group(List(spec)) :: rest =>
      renderLimit(spec, rest)

    //  (d)/(d v) f  ->  derive(f, v), and (d^n)/(d v^n) nests n times, since the grammar's
    //  `derive` takes one variable (F_0033).  The shape is unmistakable -- no ordinary
    //  quotient spells its numerator `d` and its denominator `d <name>` -- so the one loser
    //  is a user with a variable literally named `d` dividing it by `d*v`, and standard
    //  notation wins that collision.  MUST precede the generic Group case, which would
    //  otherwise render it as the fraction it is not.
    case Node.Group(List(num)) :: Node.Leaf(Tok.Punct("/")) :: Node.Group(List(den)) :: rest =>
      derivativeOf(num, den) match
        case Some((v, order)) =>
          if rest.isEmpty then Left("d/dx needs an expression to differentiate; write derive(f, x)")
          else render(rest).map(body => (1 to order).foldLeft(body)((b, _) => s"derive($b, $v)"))
        case None =>
          // An ordinary fraction of two bracketed groups: render the numerator and hand the
          // rest back, so the generic cases below keep owning what a fraction looks like.
          for n <- groupText(List(num))
              r <- render(Node.Leaf(Tok.Punct("/")) :: Node.Group(List(den)) :: rest)
          yield n + r

    //  |…| -> abs(…)  (issue F_0036).  The bar is its own closer, so pairing is
    //  SEQUENTIAL: the first bar opens and the next bar at this level closes it.  That reads
    //  every unnested spelling correctly — `|a|-|b|` pairs as abs(a)-abs(b), `|x+(|y|)|` is
    //  fine because a bracketed group is one opaque node with its own level — and makes a
    //  NESTED one refuse itself: `||a||` pairs its two opening bars around nothing, and empty
    //  content is refused by name rather than guessed at.
    case Node.Leaf(Tok.Punct("|")) :: rest =>
      val (content, after) = rest.span { case Node.Leaf(Tok.Punct("|")) => false; case _ => true }
      after match
        case Node.Leaf(Tok.Punct("|")) :: tail =>
          if content.isEmpty then Left("nested or empty '|...|' is ambiguous here; write abs(x)")
          else for c <- render(content); r <- render(tail) yield s"abs($c)$r"
        case _ => Left("unbalanced '|' in the formula; the absolute value is written abs(x)")

    //  (A) choose (B) -> binom(A, B)  (F_0037): an INFIX word between two groups, which is
    //  how AsciiMath spells `\binom`.  Matched before the generic group case, which would
    //  otherwise render the left group and then meet `choose` as an unknown name.
    case Node.Group(a) :: Node.Leaf(Tok.Name(w)) :: Node.Group(b) :: rest if Infix.contains(w) =>
      for x <- renderItems(a); y <- renderItems(b); r <- render(rest)
      yield s"${Infix(w)}($x, $y)$r"

    // sqrt(A) -> (A)^(1/2).  The grammar has no radical: a root is a fractional exponent, and
    // `ToLatex` reads all three spellings of one back as \sqrt.
    case Node.Leaf(Tok.Name("sqrt")) :: Node.Group(arg) :: rest =>
      for a <- renderItems(arg); r <- render(rest) yield s"($a)^(1/2)$r"

    // root(N)(A) -> (A)^(1/N)
    case Node.Leaf(Tok.Name("root")) :: Node.Group(deg) :: Node.Group(arg) :: rest =>
      for d <- renderItems(deg); a <- renderItems(arg); r <- render(rest) yield s"($a)^(1/$d)$r"

    // log _B(A) -> log(A, B).  AsciiMath writes the base as a subscript; the grammar writes it
    // as the second argument, which is the only place `_` is legitimate here.
    case Node.Leaf(Tok.Name("log")) :: Node.Leaf(Tok.Punct("_")) :: baseAndRest =>
      baseAndRest match
        case b :: Node.Group(arg) :: rest =>
          for base <- render(List(b)); a <- renderItems(arg); r <- render(rest)
            yield s"log($a, $base)$r"
        case _ => Left("a subscript is only understood as the base of 'log'; write log(x, b)")

    case Node.Leaf(Tok.Name(n)) :: Node.Group(arg) :: rest =>
      val name = Renamed.getOrElse(n, n)
      if !Parser.ReservedWords.contains(name) then
        Left(s"'$n' is not a function this grammar knows; '$n(...)' would read as a product with the variable '$n'")
      else for a <- renderItems(arg); r <- render(rest) yield s"$name($a)$r"

    //  `x y` is a PRODUCT, never the identifier `xy` (F_0033, but a pre-existing fault).
    //  Whitespace is presentational everywhere EXCEPT between two names: a multi-character
    //  variable arrives as one token, so adjacent Name tokens can only be juxtaposed
    //  multiplication -- and joining them bare would hand the grammar a different variable,
    //  the confidently-wrong answer this reader exists to refuse.  A following number is
    //  included (`x 2` would fuse into `x2`); a LEADING number is not, since a digit cannot
    //  continue an identifier and `2x` must stay the `2x` the grammar already reads.
    case Node.Leaf(t @ Tok.Name(_)) :: (rest @ Node.Leaf(Tok.Name(_) | Tok.Num(_)) :: _) =>
      for head <- leaf(t); tail <- render(rest) yield s"$head*$tail"

    case Node.Leaf(tok) :: rest =>
      for head <- leaf(tok); tail <- render(rest) yield head + tail

    case Node.Group(items) :: rest =>
      for i <- groupText(items); r <- render(rest) yield i + r

  /** A bracket group's own text: a matrix when the shape says so, plain nesting otherwise. */
  private def groupText(items: List[List[Node]]): Either[String, String] =
    if isMatrix(items) then
      items.traverseJoin(row => row match
          case List(Node.Group(cells)) => renderItems(cells).map(c => s"[$c]")
          case _                       => Left("a matrix row must be bracketed"))
        .map(rows => s"[${rows.mkString(", ")}]")
    else renderItems(items).map(s => s"($s)")

  /** Reads `[_lo^hi] <integrand> d <v>` after an `int`, or refuses (issue F_0033).
   *
   *  The bounds are single tokens or groups, glued straight onto the integrand exactly as
   *  MathLive writes them (`_0^1x^2 d x`); a group bound is inlined WITHOUT brackets, which is
   *  safe because a comma already delimits the slot.  Until F_0040 it was also *necessary* —
   *  the grammar's integral limits were signed atoms, so a bracketed bound would not have
   *  parsed; they are ordinary expressions now, and `-100*pi` reaches the parser intact.
   */
  private def renderIntegral(nodes: List[Node]): Either[String, String] =
    val (bounds, body) = nodes match
      case Node.Leaf(Tok.Punct("_")) :: lo :: Node.Leaf(Tok.Punct("^")) :: hi :: tail =>
        (Some((lo, hi)), tail)
      case other => (None, other)
    splitDifferential(body).flatMap { (integrand, v, tail) =>
      for
        f <- render(integrand)
        r <- render(tail)
        text <- bounds match
          case None => Right(s"integral($f, $v)")
          case Some((lo, hi)) =>
            for l <- boundText(lo); h <- boundText(hi) yield s"integral($f, $v, $l, $h)"
      yield text + r
    }

  /** One bound: a bare token, or a group whose content is inlined bracket-free.
   *
   *  A comma-carrying group is refused rather than inlined: it is a matrix or an argument
   *  list, and either would silently become a second bound.
   */
  private def boundText(node: Node): Either[String, String] = node match
    case Node.Leaf(tok)               => leaf(tok)
    case Node.Group(List(items))      => render(items)
    case Node.Group(_)                => Left("an integral bound cannot carry a comma")

  /** Splits an integrand from its closing `d <v>` pair, matching iterated integrals.
   *
   *  A nested bare `int` claims the FIRST pair that follows it, so ` int   int  x y d x d y`
   *  closes inside-out; the depth counter is what says whose pair is whose.  A group is one
   *  opaque node here — an `int` inside brackets settles its differential when the group's own
   *  content is rendered.
   */
  private def splitDifferential(nodes: List[Node]): Either[String, (List[Node], String, List[Node])] =
    @annotation.tailrec
    def loop(ns: List[Node], depth: Int, acc: List[Node]): Either[String, (List[Node], String, List[Node])] =
      ns match
        case Node.Leaf(Tok.Name("d")) :: Node.Leaf(Tok.Name(v)) :: tail if depth == 0 =>
          Right((acc.reverse, v, tail))
        case (d @ Node.Leaf(Tok.Name("d"))) :: (n @ Node.Leaf(Tok.Name(_))) :: tail =>
          loop(tail, depth - 1, n :: d :: acc)
        case (i @ Node.Leaf(Tok.Name("int"))) :: tail =>
          loop(tail, depth + 1, i :: acc)
        case n :: tail => loop(tail, depth, n :: acc)
        case Nil       => Left("an integral needs its differential; write it as ∫ f dx, or integral(f, x)")
    loop(nodes, 0, Nil)

  /** The reduction keywords MathLive emits, mapped to the grammar's spelling (F_0037). */
  private val Reductions: Map[String, String] = Map("sum" -> "sum", "prod" -> "product")

  /** Reads `(k=lo)`, the upper bound and the body into `sum(...)`/`product(...)` (F_0037).
   *
   *  The body extends to the end of the bracket group, the same one extent rule `lim` and
   *  `d/dx` follow — the standard reading, with brackets as the way to confine it.
   */
  private def renderReduction(name: String, spec: List[Node], hi: Node,
                              rest: List[Node]): Either[String, String] =
    spec match
      case Node.Leaf(Tok.Name(v)) :: Node.Leaf(Tok.Punct("=")) :: lo if lo.nonEmpty =>
        if rest.isEmpty then Left(s"a $name needs a term to combine; write $name(f, k, lo, hi)")
        else
          for l <- render(lo); h <- boundText(hi); body <- render(rest)
          yield s"$name($body, $v, $l, $h)"
      case _ => Left(s"a $name's index is written (k=lo); or write $name(f, k, lo, hi)")

  /** Reads `(v->point[^+|-])` and the body that follows into `limit(...)` (issue F_0033). */
  private def renderLimit(spec: List[Node], rest: List[Node]): Either[String, String] =
    spec match
      case Node.Leaf(Tok.Name(v)) :: Node.Leaf(Tok.Punct("->")) :: point if point.nonEmpty =>
        val (run, dir) = point.takeRight(2) match
          case List(Node.Leaf(Tok.Punct("^")), Node.Leaf(Tok.Punct(d))) if d == "+" || d == "-" =>
            (point.dropRight(2), Some(d))
          case _ => (point, None)
        if run.isEmpty then Left("a limit needs its point; write limit(f, x, a)")
        else if rest.isEmpty then Left("a limit needs an expression to apply to; write limit(f, x, a)")
        else
          for p <- render(run); body <- render(rest)
          yield dir.fold(s"limit($body, $v, $p)")(d => s"limit($body, $v, $p, $d)")
      case _ => Left("a limit's subscript is written (x->a); or write limit(f, x, a)")

  /** The `(d^n)/(d v^n)` shape as `(variable, order)`, or `None` for an ordinary fraction.
   *
   *  The marker is `d` or the partial glyph, the same on both sides; the orders must agree
   *  and stay small — past [[MaxDeriveOrder]] a tower of `derive(...)` is more likely a
   *  transcription accident than a request.
   */
  private def derivativeOf(num: List[Node], den: List[Node]): Option[(String, Int)] =
    def marker(n: Node): Option[String] = n match
      case Node.Leaf(Tok.Name("d"))    => Some("d")
      case Node.Leaf(Tok.Punct("∂")) => Some("∂")
      case _                           => None
    def order(ns: List[Node]): Option[Int] = ns match
      case Nil                                                => Some(1)
      case List(Node.Leaf(Tok.Punct("^")), Node.Leaf(Tok.Num(n))) => n.toIntOption
      case _                                                  => None
    (num, den) match
      case (m :: numTail, m2 :: Node.Leaf(Tok.Name(v)) :: denTail) =>
        for
          k  <- marker(m)
          k2 <- marker(m2) if k == k2
          n  <- order(numTail)
          n2 <- order(denTail) if n == n2 && n >= 1 && n <= MaxDeriveOrder
        yield (v, n)
      case _ => None

  /** Highest `d^n/dx^n` read as nested `derive` calls; above it the shape is refused. */
  private val MaxDeriveOrder = 9

  /** Renders comma-separated items, keeping the separator the grammar uses for arguments. */
  private def renderItems(items: List[List[Node]]): Either[String, String] =
    items.traverseJoin(render).map(_.mkString(", "))

  /** One token, refusing the words that name something this reader does not build. */
  private def leaf(tok: Tok): Either[String, String] = tok match
    case Tok.Num(text)   => Right(text)
    // AsciiMath's infinity, which the grammar spells `inf` (issue F_0033).
    case Tok.Name("oo")  => Right("inf")
    case Tok.Punct("_")  => Left("a subscript is only understood as the base of 'log'; write log(x, b)")
    case Tok.Punct("{") | Tok.Punct("}") =>
      Left("braces are not part of this grammar; a transform is written laplace(f, t, s)")
    case Tok.Punct("->") => Left("'->' is not part of this grammar; a limit is written limit(f, x, a)")
    // The norm bars (`\|x\|`): nothing in the grammar means a norm, and reading them as abs
    // would be a confident answer to a different question.
    case Tok.Punct("∥")  => Left("'∥...∥' is a norm, which this grammar does not have; abs(x) is the absolute value")
    case Tok.Punct(text) => Right(text)
    case Tok.Name(text)  =>
      Binders.get(text) match
        case Some(spelling) => Left(s"'$text' is not accepted here; write $spelling instead")
        case None           => Right(text)

  /** Converts MathLive's AsciiMath into Leonardo grammar text.
   *
   *  @param source the AsciiMath, as `convertLatexToAsciiMath` produced it
   *  @return the grammar text, or the reason it was refused
   */
  def toGrammar(source: String): Either[String, String] =
    for
      nodes <- parseGroups(tokenise(source))
      text  <- render(nodes)
    yield text

  extension [A](items: List[A])
    /** `traverse` over `Either`, short-circuiting on the first refusal.
     *
     *  Written out because the first refusal is the one worth reporting, and a `map` + `collect`
     *  would either lose it or report the last.
     */
    private def traverseJoin(f: A => Either[String, String]): Either[String, List[String]] =
      items.foldLeft[Either[String, List[String]]](Right(Nil)) { (acc, a) =>
        for done <- acc; next <- f(a) yield next :: done
      }.map(_.reverse)
