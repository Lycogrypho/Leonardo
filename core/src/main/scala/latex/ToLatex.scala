package it.grypho.scala.leonardo
package latex

import core.*
import scalar.*

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

    // The exponent is braced, hence Grouped and never parenthesised; the base must be Atomic,
    // which brackets `(a+b)^2`, `(2x)^2` and `(a/b)^2` while leaving `x^2` alone.  A power
    // reports BELOW Atomic on purpose: as the base of another power it must be re-bracketed,
    // because `x^{2}^{3}` is not a smaller rendering but a LaTeX double-superscript ERROR.
    case Power(a, b) => (s"${at(a, Atomic)}^{${at(b, Grouped)}}", AtPower)

    case v: _Variable => (v.variable, Atomic)

    // Concrete values render as the REPL shows them.  A leading minus makes the text a sum-
    // level term, so `2 \cdot -3.0` can never be emitted -- the slot brackets it instead.
    case n: _Number => (n.toString, if n.d < 0 then AtSum else Atomic)
    case v: _Value =>
      val text = escape(v.toString)
      (text, if text.startsWith("-") then AtSum else Atomic)

    // A named function: `\mathrm{name}` for now; step 4 maps the operators LaTeX knows
    // (`\sin`, `\ln`, `\exp`, ...) onto their proper macros.
    case f: NamedFunction =>
      val args = f.children.map(at(_, Grouped)).mkString(", ")
      (s"\\mathrm{${escape(f.name)}}\\left($args\\right)", Atomic)

    case other => (fallback(other), Atomic)

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
