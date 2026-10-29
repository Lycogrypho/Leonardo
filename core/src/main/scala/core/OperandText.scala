package it.grypho.scala.leonardo
package core

/** How a child's printed text is written in a **tight operand slot** of `toString` (F_0241): the
 *  divisor of a quotient, and the base or exponent of a power.
 *
 *  Every operation node brackets *itself*, so a composite child is always safe there.  A **leaf**
 *  is not: its text is whatever its value prints, and three spellings are not one factor to the
 *  grammar.  Printed bare, each re-parses to a *different value*, the confident wrong answer this
 *  library refuses everywhere else:
 *
 *  - a fraction, `3/2` or `-1/2` (an exact `_Rational`): `y / 3/2` is `(y/3)/2`, `x ^ -1/2` is
 *    `(x^-1)/2`, `3/2 ^ x` is `3/(2^x)`;
 *  - an imaginary, `2.0i`: `y / 2.0i` is `(y/2)*i`, since a digit followed by a name is an implicit
 *    product;
 *  - a negative number as a power **base**: `-2 ^ x` is `-(2^x)`, the leading sign belonging to the
 *    first term (F_0070).  After an operator the sign is one token, so `x ^ -2.0` and `x / -2.0`
 *    keep their form.
 *
 *  Such a leaf is a *computed* value -- a solver's coefficient, a simplified exponent -- since the
 *  parser keeps a typed `3/2` as a `Ratio` of two numbers.  **The decision reads the text, not the
 *  type**, because the REPL prints a tree whose leaves it has replaced by display-only nodes holding
 *  their text at the session's precision; a test on the type would miss every one of them.
 *  `:save` and the conventional display (`pretty on`) bracket leaves by their own, stricter rules
 *  (F_0046, F_0224), which bracket every leaf that is not a bare token, in any slot.
 */
private[leonardo] object OperandText:

  /** `text`, bracketed when the grammar would regroup it in a tight slot.
   *  @param text   a child's printed text
   *  @param asBase whether the slot is a power's base, where a leading minus also regroups
   *  @return `text`, or `(text)`
   */
  def tight(text: String, asBase: Boolean): String =
    if (asBase && text.startsWith("-")) || Imaginary.matches(text) || hasTopLevelSlash(text) || impliedRoot(text)
    then s"($text)"
    else text

  /** Whether `text` holds a `√` with something before it other than a leading minus: `2√13` is
   *  an implicit product (F_0231), so `2√13 ^ x` would read as `2·(√13 ^ x)`.  A bare `√13` is
   *  one factor and needs nothing. */
  private def impliedRoot(text: String): Boolean =
    val i = text.indexOf('√')
    i > 0 && !(i == 1 && text.head == '-')

  /** A pure imaginary with a written magnitude, as `_Complex.display` prints one (`2.0i`, `-1.5E-7i`);
   *  a bare `i` is the constant, one token. */
  private val Imaginary = """-?\d+(\.\d+)?([eE][-+]?\d+)?i""".r

  /** Whether a `/` appears outside every bracket of `text`. */
  private def hasTopLevelSlash(text: String): Boolean =
    text.foldLeft((0, false)) { case ((depth, found), c) =>
      c match
        case '(' | '[' => (depth + 1, found)
        case ')' | ']' => (depth - 1, found)
        case '/'       => (depth, found || depth == 0)
        case _         => (depth, found)
    }._2
