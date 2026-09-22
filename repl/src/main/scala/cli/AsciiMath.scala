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

  /** Words that name something the grammar spells differently, mapped to that spelling.
   *
   *  Refused rather than converted: each is a *binder*, and recovering one from AsciiMath means
   *  locating an integrand and a `d`-variable inside an unbracketed token run.  Declining is
   *  the honest answer while that is true (the `Asin` convention, applied to notation).
   */
  private val Binders: Map[String, String] =
    Map(
      "int"  -> "integral(f, x)",   "oint" -> "integral(f, x)",
      "lim"  -> "limit(f, x, a)",   "sum"  -> "tabulate(f, k, lo, hi)",
      "prod" -> "tabulate(f, k, lo, hi)")

  /** The operator glyphs MathLive emits, in the grammar's ASCII. */
  private val Glyphs: Map[Char, String] =
    Map('≠' -> "!=", '≤' -> "<=", '≥' -> ">=", '×' -> "*", '÷' -> "/")

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

    case Node.Leaf(tok) :: rest =>
      for head <- leaf(tok); tail <- render(rest) yield head + tail

    case Node.Group(items) :: rest =>
      val inner =
        if isMatrix(items) then
          items.traverseJoin(row => row match
            case List(Node.Group(cells)) => renderItems(cells).map(c => s"[$c]")
            case _                       => Left("a matrix row must be bracketed"))
            .map(rows => s"[${rows.mkString(", ")}]")
        else renderItems(items).map(s => s"($s)")
      for i <- inner; r <- render(rest) yield i + r

  /** Renders comma-separated items, keeping the separator the grammar uses for arguments. */
  private def renderItems(items: List[List[Node]]): Either[String, String] =
    items.traverseJoin(render).map(_.mkString(", "))

  /** One token, refusing the words that name something this reader does not build. */
  private def leaf(tok: Tok): Either[String, String] = tok match
    case Tok.Num(text)   => Right(text)
    case Tok.Punct("_")  => Left("a subscript is only understood as the base of 'log'; write log(x, b)")
    case Tok.Punct("{") | Tok.Punct("}") =>
      Left("braces are not part of this grammar; a transform is written laplace(f, t, s)")
    case Tok.Punct("->") => Left("'->' is not part of this grammar; a limit is written limit(f, x, a)")
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
