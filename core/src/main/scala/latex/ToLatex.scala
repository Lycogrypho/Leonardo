package it.grypho.scala.leonardo
package latex

import core.*
import scalar.*
import transform.*

/** Renders an expression as LaTeX source (issue F_0016).
 *
 *  **A second renderer, not a replacement.**  `toString` emits the *input grammar* and must
 *  keep doing so — it is what `:save` writes and what the round-trip invariant re-parses.
 *  LaTeX is display only, and the arrow runs one way: nothing ever reads this back.
 *
 *  **Why the package sits here.**  A renderer that must match on `scalar.Sin` and
 *  `matrix._Matrix` cannot live in `core`, which depends on no domain; it sits *above* every
 *  package, exactly as `parser` does.  `toString` is no counterexample — it is an override on
 *  each node, not a function anywhere.
 *
 *  **Coverage is a curated set plus a fallback** (F_0016 Decision 2): a node without a rule
 *  renders through [[ToLatex.fallback]] as `\mathrm{…}` of its `toString` — *correct and
 *  plain* rather than wrong — which is what lets the emitter improve node by node without
 *  ever being half-broken.  Step 1 covers the scalar operator core; matrices, binders and the
 *  named-function macros are steps 2–4.
 */
object ToLatex:

  // ── precedence ──────────────────────────────────────────────────────────────────
  //
  // The whole point of the pass.  `toString` parenthesises defensively because it must
  // re-parse; LaTeX's \frac and ^{...} carry their own grouping, so transcribing those
  // parentheses would be correct and unreadable.  Every node reports the precedence it
  // renders AT; every context asks for the minimum that needs no brackets.

  /** A slot LaTeX already delimits — a `\frac` argument, a braced exponent, a function
   *  argument list.  Nothing is ever bracketed here, and that is what makes `(a+b)/(c+d)`
   *  come out as `\frac{a + b}{c + d}` with **both** pairs of parentheses gone.
   */
  private val Grouped   = 0
  private val AtSum     = 1
  private val AtProduct = 2
  private val AtPower   = 3
  private val Atomic    = 4

  /** Renders `e` for a slot needing at least `min`, bracketing only when it falls short. */
  private def at(e: _Expression, min: Int): String =
    val (text, prec) = render(e)
    if prec < min then s"\\left($text\\right)" else text

  /** Renders an expression as LaTeX.
   *
   *  @param e the expression
   *  @return LaTeX source with no surrounding math delimiters — the caller chooses between
   *          `$…$`, `\[…\]` or a renderer's own API
   */
  def apply(e: _Expression): String = render(e)._1

  /** The traversal: each node returns its LaTeX and the precedence it rendered at. */
  private def render(e: _Expression): (String, Int) = e match

    // A division is a \frac whose two slots are GROUPED -- the case the pass exists for.  It
    // reports AtProduct rather than Atomic so a power still brackets it: \frac{a}{b}^2 reads
    // as b carrying the exponent, and \left(\frac{a}{b}\right)^2 is the honest form.
    case Ratio(a, b) => (s"\\frac{${at(a, Grouped)}}{${at(b, Grouped)}}", AtProduct)

    // Subtraction has no node: `a - b` parses to Sum(a, (-1)*b), and `a - 3k` folds the sign
    // into the coefficient as Sum(a, (-3)*k).  Recognising both here is not cosmetic --
    // every subtraction anyone types arrives in one of these shapes, and rendering them
    // naively gives `a + -1 \cdot b`.  The subtrahend slot asks for AtProduct, so a compound
    // one is bracketed: `a - \left(b + c\right)`.
    case Sum(a, Negated(b)) => (s"${at(a, AtSum)} - ${at(b, AtProduct)}", AtSum)
    case Sum(a, b)          => (s"${at(a, AtSum)} + ${at(b, AtSum)}", AtSum)

    case Negated(b) => (s"-${at(b, AtProduct)}", AtSum)

    // A numeric coefficient juxtaposes (`2 x`); everything else takes an explicit \cdot.
    // Bare juxtaposition of two variables is rejected on purpose: in THIS grammar `xy` is a
    // single identifier, so `x y` in output would suggest an input that means something else.
    case Product(n: _Number, b) => (s"${at(n, Atomic)} ${at(b, AtProduct)}", AtProduct)
    case Product(a, b)          => (s"${at(a, AtProduct)} \\cdot ${at(b, AtProduct)}", AtProduct)

    // A root is a power written the other way round.  The radicand is Grouped because \sqrt
    // delimits for itself -- the same argument \frac makes -- so `(a+b)^0.5` comes out as
    // `\sqrt{a + b}` rather than `\sqrt{\left(a + b\right)}`.
    case Power(a, Root(2))      => (s"\\sqrt{${at(a, Grouped)}}", Atomic)
    case Power(a, Root(degree)) => (s"\\sqrt[$degree]{${at(a, Grouped)}}", Atomic)

    // The exponent is braced, hence Grouped and never parenthesised; the base must be Atomic,
    // which brackets `(a+b)^2`, `(2x)^2` and `(a/b)^2` while leaving `x^2` alone.  A power
    // reports BELOW Atomic on purpose: as the base of another power it must be re-bracketed,
    // because `x^{2}^{3}` is not a smaller rendering but a LaTeX double-superscript ERROR.
    case Power(a, b) => (s"${at(a, Atomic)}^{${at(b, Grouped)}}", AtPower)

    // ── the binders ──────────────────────────────────────────────────────────
    //
    // The nine nodes `NamedFunction` excludes.  Each prints a variable its `children` omit,
    // which is exactly why a shared `name(children)` rendering cannot serve them: it would
    // emit the integrand and lose the `dx`.  Each also has notation no generic rule reaches.

    // The measure terminates the integrand, but only against a product -- `\int a + b \,dx`
    // genuinely reads as `(\int a) + b\,dx`, so a sum must be bracketed and nothing else need be.
    case _Integral(e, v) => (s"\\int ${at(e, AtProduct)} \\,d${v.variable}", AtSum)
    case _DefIntegral(e, v, lo, hi) =>
      (s"\\int_{${at(lo, Grouped)}}^{${at(hi, Grouped)}} ${at(e, AtProduct)} \\,d${v.variable}",
       AtSum)

    // The operator form `\frac{d}{dx}(…)` rather than `\frac{de}{dx}`: the operand is an
    // arbitrary expression, and brackets around it are conventional and never ambiguous.
    case _Derivative(e, v) =>
      (s"\\frac{d}{d${v.variable}}\\left(${at(e, Grouped)}\\right)", Atomic)

    // The direction rides the POINT, not the operator: `x \to 0^{+}` is where a reader looks
    // for it, and it is the only place it can go without inventing notation.
    case _Limit(e, v, point, dir) =>
      val approach = dir match
        case LimitDir.Both      => at(point, Grouped)
        case LimitDir.FromRight => s"${at(point, Atomic)}^{+}"
        case LimitDir.FromLeft  => s"${at(point, Atomic)}^{-}"
      (s"\\lim_{${v.variable} \\to $approach} ${at(e, AtProduct)}", AtSum)

    case Transformed(t) => (t.latex, Atomic)

    // ── the named functions with notation of their own ──────────────────────────────
    //
    // These three are not `name(args)` at all, so they precede the generic NamedFunction arm.
    // The operand is Atomic because `a + b!` would otherwise claim only the b.
    case Factorial(e)  => (s"${at(e, Atomic)}!", Atomic)
    case Binom(n, k)   => (s"\\binom{${at(n, Grouped)}}{${at(k, Grouped)}}", Atomic)

    case v: _Variable => (greek(v.variable), Atomic)

    // An exact rational stays a FRACTION.  Showing 1/3 as 0.33333 would discard precisely
    // what the exact tier exists to preserve, and `display` is allowed to fall back to a
    // decimal for a long one -- acceptable in a terminal, not in typeset mathematics.
    // Reduced first, because the Lazy gcd policy permits an unreduced pair to reach here.
    case r: _Rational =>
      val g       = r.num.gcd(r.den)
      val (n, d)  = (r.num / g, r.den / g)
      // The sign belongs outside the fraction: -\frac{1}{2}, never \frac{-1}{2}.  That makes
      // the text sum-level, so a product slot brackets it rather than emitting `2 \cdot -...`.
      val body    = if d == 1 then n.abs.toString else s"\\frac{${n.abs}}{$d}"
      if n.signum < 0 then (s"-$body", AtSum) else (body, if d == 1 then Atomic else AtProduct)

    // A matrix is its own delimiter, so it is Atomic, and every cell sits in an alignment
    // slot that groups for itself -- hence Grouped and never bracketed.
    case m: _MatrixShaped => (pmatrix(m.rows, m.cols, i => at(m.children(i), Grouped)), Atomic)
    case m: _MatrixValue  =>
      (pmatrix(m.rows, m.cols, i => render(_Number(m.toVector(i)))._1), Atomic)

    // Concrete values render as the REPL shows them.  A leading minus makes the text a sum-
    // level term, so `2 \cdot -3.0` can never be emitted -- the slot brackets it instead.
    case n: _Number => (n.toString, if n.d < 0 then AtSum else Atomic)
    case v: _Value =>
      val text = escape(v.toString)
      (text, if text.startsWith("-") then AtSum else Atomic)

    // Every other named function: LaTeX's own operator where one exists, upright otherwise.
    case f: NamedFunction =>
      val args = f.children.map(at(_, Grouped)).mkString(", ")
      (s"${operator(f.name)}\\left($args\\right)", Atomic)

    case other => (fallback(other), Atomic)

  /** The LaTeX operator for a function name, upright otherwise.
   *
   *  **Not cosmetic.**  `\sin` carries operator spacing that `\mathrm{sin}` does not, which is
   *  how a reader tells a function from a product of three variables.  Three groups:
   *
   *   -  names LaTeX spells identically (`sin`, `ln`, `cosh`, …) — the macro is the name;
   *   -  names where **the grammar and mathematics disagree**: this library says `asin`,
   *      mathematics says `arcsin`, and LaTeX provides `\arcsin`.  Rendering `\mathrm{asin}`
   *      would be faithful to the input and wrong as notation;
   *   -  `Gamma`, which is the Greek letter itself.
   *
   *  Everything else stays `\mathrm{…}` **deliberately**: `sech` and the inverse hyperbolics
   *  have no built-in macro and no single agreed spelling (`arsinh`? `arcsinh`? `sinh^{-1}`?),
   *  so upright is correct and neutral while inventing a macro would not be.
   */
  private def operator(name: String): String = Operators.getOrElse(name, s"\\mathrm{${escape(name)}}")

  private val Operators: Map[String, String] =
    val identical = List("sin", "cos", "tan", "sec", "csc", "cot",
                         "sinh", "cosh", "tanh", "coth", "exp", "ln", "log")
    identical.map(n => n -> s"\\$n").toMap ++ Map(
      "asin"  -> "\\arcsin", "acos" -> "\\arccos", "atan" -> "\\arctan",
      "Gamma" -> "\\Gamma"
    )

  /** A variable's name as a Greek letter when it *is* one, unchanged otherwise.
   *
   *  **Whole-name match only.**  A prefix match would rename `alphabet` to `\alpha bet`, and
   *  silently renaming a user's variable is worse than leaving it plain.  `omicron` is absent
   *  because LaTeX has no macro for it — it is an ordinary `o`.
   *
   *  Note `pi` and `e` cannot reach here: the parser folds both to numbers at parse time, so
   *  `\pi` is not producible from a parsed expression.  That is a property of the grammar,
   *  not a gap in this table.
   */
  private def greek(name: String): String = Greek.getOrElse(name, name)

  private val Greek: Map[String, String] =
    val lower = List("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
                     "iota", "kappa", "lambda", "mu", "nu", "xi", "rho", "sigma", "tau",
                     "upsilon", "phi", "chi", "psi", "omega")
    val upper = List("Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon",
                     "Phi", "Psi", "Omega")
    (lower ++ upper).map(n => n -> s"\\$n").toMap

  /** Lays out `rows × cols` cells as a `pmatrix`, reading each by its row-major index.
   *
   *  Shared by the symbolic and dense carriers so the two cannot drift: they differ only in
   *  what a cell *is* — an `_Expression` against a `Double` — which is exactly what the
   *  `cell` function absorbs.
   *
   *  @param rows the row count
   *  @param cols the column count
   *  @param cell the already-rendered cell at a row-major index
   */
  private def pmatrix(rows: Int, cols: Int, cell: Int => String): String =
    val body = (0 until rows)
      .map(i => (0 until cols).map(j => cell(i * cols + j)).mkString(" & "))
      .mkString(" \\\\ ")
    s"\\begin{pmatrix} $body \\end{pmatrix}"

  /** Everything without a rule yet: its `toString`, escaped, upright.
   *
   *  **Correct and plain rather than wrong** — a node with no rule reads as itself instead of
   *  producing malformed LaTeX or vanishing.  `\mathrm` also stops the renderer italicising
   *  what is really source text.
   */
  private def fallback(e: _Expression): String = s"\\mathrm{${escape(e.toString)}}"

  /** Escapes every character LaTeX reads as markup.
   *
   *  **Load-bearing precisely because of the fallback**: a `toString` is full of `^`, `_` and
   *  parentheses-of-meaning, and one unescaped `^` turns the document into an error — or,
   *  worse, silently changes what is displayed.
   */
  private[latex] def escape(s: String): String =
    val sb = new StringBuilder
    s.foreach {
      case '\\'                       => sb ++= "\\textbackslash{}"
      case '^'                        => sb ++= "\\textasciicircum{}"
      case '~'                        => sb ++= "\\textasciitilde{}"
      case c if "#$%&_{}".contains(c) => sb += '\\'; sb += c
      case c                          => sb += c
    }
    sb.toString

  /** Matches the five integral transforms, rendering each in its own calligraphic operator.
   *
   *  One extractor rather than five cases because they differ only in a symbol and whether
   *  the operator is inverted — the *shape* `\mathcal{X}\{f\}(result)` is identical, and
   *  writing it five times would be five chances for the spacing to drift.
   *
   *  **Which variable appears is the subtle part.**  Each node carries two: a binder the
   *  transform consumes and a variable the answer is a function of.  `children` holds only
   *  the operand, so both names would be lost by any generic rendering — and it is the
   *  *second* that belongs in the output, since `\mathcal{L}\{t\}(s)` is a function of `s`.
   */
  private object Transformed:
    def unapply(e: _Expression): Option[Transformed] = e match
      case _Laplace(f, _, s)           => Some(Transformed("L", inverse = false, f, s))
      case _InverseLaplace(f, _, t)    => Some(Transformed("L", inverse = true,  f, t))
      case _Fourier(f, _, w)           => Some(Transformed("F", inverse = false, f, w))
      case _ZTransform(f, _, z)        => Some(Transformed("Z", inverse = false, f, z))
      case _InverseZTransform(f, _, n) => Some(Transformed("Z", inverse = true,  f, n))
      case _                           => None

  /** One transform's rendering data: the operator letter, whether it is inverted, the operand
   *  and the variable the result is a function of.
   */
  private case class Transformed(symbol: String, inverse: Boolean,
                                 operand: _Expression, result: _Variable):
    /** `\mathcal{L}\{f\}(s)` — the braces are escaped, since bare ones would group silently. */
    def latex: String =
      val op = if inverse then s"\\mathcal{$symbol}^{-1}" else s"\\mathcal{$symbol}"
      s"$op\\left\\{${at(operand, Grouped)}\\right\\}(${result.variable})"

  /** Matches an exponent that denotes an `n`-th root, answering `n`.
   *
   *  **Three shapes, one meaning.**  `x^0.5` arrives as a `Double` literal, `x^(1/2)` as a
   *  `Ratio` of two literals that nothing folds at parse time, and exact mode produces a
   *  `_Rational` — all three are the same square root to a reader, so all three must find
   *  the radical or the rendering would depend on how the user happened to type it.
   *
   *  Only a **unit** fraction qualifies: `x^(2/3)` is a genuine power and stays one, since
   *  `\sqrt[3]{x^2}` is a different (if equal) statement and not what was written.  The
   *  degree is capped at a readable size — past that `\sqrt[97]{}` is worse than the power.
   */
  private object Root:
    private val MaxDegree = 9

    def unapply(e: _Expression): Option[Int] = e match
      // Matches a _Rational of one half too: `_Number` is a widening extractor, so exact mode
      // finds the square root here without a case of its own.
      case _Number(0.5)                    => Some(2)
      case r: _Rational if r.num == 1      => degreeOf(BigDecimal(r.den))
      case Ratio(_Number(1.0), _Number(d)) => degreeOf(BigDecimal(d))
      case _                               => None

    /** A denominator is a root degree only when it is a WHOLE number in `2..MaxDegree`.
     *
     *  The wholeness test is load-bearing: truncating would read `x^(1/3.5)` as a cube root,
     *  which is a different number rendered with total confidence.
     */
    private def degreeOf(den: BigDecimal): Option[Int] =
      if den.isWhole && den >= 2 && den <= MaxDegree then Some(den.toInt) else None

  /** Matches the AST's spellings of a negated term, answering its positive counterpart.
   *
   *  Display-only arithmetic: the rebuilt positive node exists just long enough to be
   *  rendered, never stored.  An **exact** negative coefficient is deliberately not matched
   *  beyond the bare `-1` (which needs no rebuild): negating it here would demote it to a
   *  `Double` for display, and an exact value must never be shown through a demotion.
   */
  private object Negated:
    def unapply(e: _Expression): Option[_Expression] = e match
      case Product(_Number(-1.0), b)       => Some(b)
      case Product(n: _Number, b) if n.d < 0 => Some(Product(_Number(-n.d), b))
      case n: _Number if n.d < 0           => Some(_Number(-n.d))
      case _                               => None
