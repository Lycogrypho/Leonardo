package it.grypho.scala.leonardo
package core


/** Companion for the graded truth value [[_Truth]]; holds the smart factory and the
 *  named `unknown` constant.
 *
 *  [[_Truth.of]] collapses the crisp endpoints `0.0` / `1.0` back to
 *  `_Bool(false)` / `_Bool(true)`, so every existing `_Bool(b)` pattern match across the
 *  codebase keeps firing on crisp results — only a genuinely intermediate degree ever
 *  becomes a `_Truth`.  This mirrors [[_Complex.of]]'s collapse of a zero imaginary part
 *  back to [[_Number]].
 */
object _Truth:

  /** The third Kleene truth value, `unknown` — neither true nor false.
   *
   *  Under the min–max rule table this is the fixpoint `0.5`: `not unknown = unknown`
   *  and `unknown and unknown = unknown`.
   */
  val Unknown: _Truth = new _Truth(0.5)

  /** Smart factory: clamps `d` into `[0, 1]` and collapses the crisp endpoints to
   *  [[_Bool]], preserving the boolean fast path and all `_Bool(b)` pattern matches.
   *
   *  @param d the truth degree; values outside `[0, 1]` are clamped, `NaN` becomes
   *           [[Unknown]] (unreachable from the min–max kernels, which are closed
   *           over `[0, 1]`)
   *  @return `_Bool(false)` for `0.0`, `_Bool(true)` for `1.0`, a `_Truth` otherwise
   */
  def of(d: Double): _Value =
    if d.isNaN then Unknown
    else
      val clamped = if d < 0.0 then 0.0 else if d > 1.0 then 1.0 else d
      if clamped == 0.0 then _Bool(false)
      else if clamped == 1.0 then _Bool(true)
      else new _Truth(clamped)

  /** Reads a *symmetric* ternary digit as a truth value.
   *
   *  Symmetric ternary spells the three truth values `{-1, 0, 1}` where the default
   *  alphabet uses `{0, 1/2, 1}`; the two are related by the affine map
   *  `t = (s + 1) / 2`, of which [[_Truth.symmetric]] is the inverse.  This is a change
   *  of digits only — the same rule table drives both spellings.
   *
   *  @param s the symmetric digit; `-1` is false, `0` is unknown, `1` is true
   *  @return the corresponding truth value, via [[_Truth.of]] (so `±1` collapse to `_Bool`)
   */
  def fromSymmetric(s: Double): _Value = of((s + 1.0) / 2.0)

  /** Reads any truth-valued result as a symmetric ternary digit.
   *
   *  The counterpart of [[fromSymmetric]] for values that may be `_Bool` rather than
   *  `_Truth` (the crisp endpoints, which `of` collapses).
   *
   *  @param v the value to read
   *  @return the symmetric digit in `[-1, 1]`, or `None` when `v` is not truth-valued
   */
  def toSymmetric(v: _Value): Option[Double] = v match
    case _Bool(b)  => Some(if b then 1.0 else -1.0)
    case t: _Truth => Some(t.symmetric)
    case _         => None


/** A graded truth value: a degree strictly between false and true.
 *
 *  The degree is never exactly `0.0` or `1.0` — the [[_Truth.of]] factory guarantees this
 *  invariant by collapsing to [[_Bool]] in those cases, so `_Truth` and `_Bool` together
 *  carry the full `[0, 1]` interval without overlapping.  Construction through `of` is the
 *  only public route; pattern matching `_Truth(d)` remains available.
 *
 *  In three-valued (Kleene) logic the only reachable degree is `0.5`, the named constant
 *  [[_Truth.Unknown]], which prints and parses as `unknown`.  The `[0, 1]` carrier is
 *  already in place for the fuzzy tier; degrees other than `0.5` print as plain numbers
 *  and do not round-trip through the parser yet (there is no grammar literal for them).
 *
 *  Like [[_Number]], a `_Truth` never rounds in `eval`; rounding is a display concern
 *  handled by `toString` / `display(p)`.
 *
 *  @param d the truth degree, guaranteed to lie strictly inside `(0, 1)` by [[_Truth.of]]
 */
case class _Truth private (d: Double) extends _Value:
  /** Returns `Right(this)` — a concrete truth value needs no further reduction. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  /** Renders this truth value at [[Environment.DefaultPrecision]] decimal places;
   *  the Kleene midpoint prints as `"unknown"`.
   */
  override def toString: String = display(Environment.DefaultPrecision)

  /** Renders this truth value at `precision` decimal places for REPL display.
   *  @param precision number of decimal places to show
   *  @return `"unknown"` when the rounded degree is `0.5`, the rounded number otherwise
   */
  def display(precision: Int): String =
    val r = _Number.round(d, precision)
    if r == 0.5 then "unknown" else r.toString

  /** This degree written as a symmetric ternary digit: `2 * d - 1`, so `0.5` is `0`.
   *  The inverse of [[_Truth.fromSymmetric]].
   */
  def symmetric: Double = 2.0 * d - 1.0
