package it.grypho.scala.leonardo
package core

import scala.math.{exp, log, sin, cos, sinh, cosh, atan2, hypot}


/** Companion for the concrete complex value [[_Complex]]; holds the smart factory and all
 *  arithmetic on [[_Value]] operands.
 *
 *  Arithmetic returns `Option[_Value]` — `None` signals a non-finite or undefined result
 *  (e.g. `log 0`, `0 ^ negative`) so the caller stays symbolic, mirroring the "domain
 *  errors stay symbolic" contract of the real path.
 *
 *  [[_Complex.of]] collapses a zero imaginary part back to a plain [[_Number]], so every
 *  existing `_Number(x)` pattern match across the codebase keeps firing on real results —
 *  only a genuinely non-real value ever becomes a `_Complex`.  Binary operations promote
 *  `_Number` operands to `(x, 0)` via [[_Complex.parts]].
 */
object _Complex:

  /** Smart factory: collapses a zero imaginary part to [[_Number]], preserving the real
   *  fast-path and all `_Number(x)` pattern matches.
   *  @param re real part
   *  @param im imaginary part; if `0.0` the result is `_Number(re)`
   */
  def of(re: Double, im: Double): _Value =
    if im == 0.0 then _Number(re) else new _Complex(re, im)

  /** Extracts the `(re, im)` parts from any numeric value.
   *  `None` for non-numeric values such as [[_Bool]] or [[_MatrixValue]].
   */
  def parts(v: _Value): Option[(Double, Double)] = v match
    case _Number(x)   => Some((x, 0.0))
    case c: _Complex  => Some((c.re, c.im))
    // A rational read as a Double IS the promotion lattice's float contagion (issue 4.L).
    // Putting it here rather than in each operation means every complex kernel already
    // handles a mixed rational/inexact pair, and handles it the one documented way: the
    // exact operand degrades, never the reverse.  The exact rational-with-rational cases
    // are matched earlier, in `scalar._Operation`, so they never reach this reader.
    case r: _Rational => Some((r.toDouble, 0.0))
    case _            => None

  private def finiteVal(re: Double, im: Double): Option[_Value] =
    if re.isNaN || re.isInfinite || im.isNaN || im.isInfinite then None else Some(of(re, im))

  /** Complex addition `a + b`; `None` when either operand is non-numeric or the result
   *  is non-finite.
   */
  def add(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- finiteVal(ar + br, ai + bi) yield r

  /** Complex subtraction `a - b`; `None` when either operand is non-numeric or the result
   *  is non-finite.
   */
  def sub(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- finiteVal(ar - br, ai - bi) yield r

  /** Complex multiplication `a * b`; `None` when either operand is non-numeric or the
   *  result is non-finite.
   */
  def mul(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b)
        r <- finiteVal(ar * br - ai * bi, ar * bi + ai * br) yield r

  /** Complex division `a / b`; `None` when either operand is non-numeric, the denominator
   *  is zero, or the result is non-finite.
   */
  def div(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- divParts(ar, ai, br, bi) yield r

  private def divParts(ar: Double, ai: Double, br: Double, bi: Double): Option[_Value] =
    val denom = br * br + bi * bi
    if denom == 0.0 then None    // division by zero stays symbolic, as in the real path
    else finiteVal((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)

  /** Principal complex power `z ^ w = exp(w · log z)`.  `0 ^ w` is `0` for a positive
   *  real `w` and `None` otherwise, matching the real path's "0 ^ negative stays symbolic".
   *  @param a base
   *  @param b exponent
   */
  def pow(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- powParts(ar, ai, br, bi) yield r

  private def powParts(ar: Double, ai: Double, br: Double, bi: Double): Option[_Value] =
    if ar == 0.0 && ai == 0.0 then
      if br > 0.0 && bi == 0.0 then Some(_Number(0.0)) else None
    else
      // log z = ln|z| + i·arg z ; then w·log z ; then exp of that.
      val lr = log(hypot(ar, ai))
      val li = atan2(ai, ar)
      val er = br * lr - bi * li
      val ei = br * li + bi * lr
      val mag = exp(er)
      finiteVal(mag * cos(ei), mag * sin(ei))

  /** Complex exponential `exp(v) = e^a(cos b + i·sin b)` where `v = a + bi`.
   *  `None` for non-numeric input or non-finite result.
   */
  def expc(v: _Value): Option[_Value] =
    for (a, b) <- parts(v); r <- finiteVal(exp(a) * cos(b), exp(a) * sin(b)) yield r

  /** Principal complex logarithm `log(v) = ln|v| + i·arg(v)`.
   *  `None` when `v` is zero, non-numeric, or the result is non-finite.
   */
  def logc(v: _Value): Option[_Value] =
    parts(v).flatMap { (a, b) =>
      val mod = hypot(a, b)
      if mod == 0.0 then None else finiteVal(log(mod), atan2(b, a))
    }

  /** Complex sine `sin(a + bi) = sin(a)cosh(b) + i·cos(a)sinh(b)`.
   *  `None` for non-numeric input or non-finite result.
   */
  def sinc(v: _Value): Option[_Value] =
    for (a, b) <- parts(v); r <- finiteVal(sin(a) * cosh(b), cos(a) * sinh(b)) yield r

  /** Complex cosine `cos(a + bi) = cos(a)cosh(b) − i·sin(a)sinh(b)`.
   *  `None` for non-numeric input or non-finite result.
   */
  def cosc(v: _Value): Option[_Value] =
    for (a, b) <- parts(v); r <- finiteVal(cos(a) * cosh(b), -(sin(a) * sinh(b))) yield r

  /** Complex tangent `tan(v) = sin(v) / cos(v)`.
   *  `None` when the cosine is zero, the input is non-numeric, or the result is non-finite.
   */
  def tanc(v: _Value): Option[_Value] =
    for s <- sinc(v); c <- cosc(v); r <- div(s, c) yield r


/** Concrete complex value `re + im·i` where `im ≠ 0`.
 *
 *  The imaginary part is never exactly `0` — the [[_Complex.of]] factory guarantees this
 *  invariant by collapsing to [[_Number]] in that case.  Construction through `of` is the
 *  only public route; pattern matching `_Complex(re, im)` remains available.
 *
 *  Like [[_Number]], a `_Complex` never rounds in `eval`; rounding is a display concern
 *  handled by `toString` / `display(p)`.  A rounded-away imaginary part causes the value
 *  to print as a plain real (e.g. `exp(i·π)` → `"-1.0"`); a pure imaginary value prints
 *  as `"bi"` / `"i"` / `"-i"`; the full form is `"(a + bi)"`.
 *
 *  @param re real part
 *  @param im imaginary part; guaranteed non-zero by the [[_Complex.of]] factory
 */
case class _Complex private (re: Double, im: Double) extends _Value:
  /** Returns `Right(this)` — a concrete complex value needs no further reduction. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  /** Renders this complex number at [[Environment.DefaultPrecision]] decimal places.
   *  A rounded-away imaginary part yields a plain real string; a zero real part yields
   *  `"bi"` / `"i"` / `"-i"`; otherwise `"(a ± bi)"`.
   */
  override def toString: String = display(Environment.DefaultPrecision)

  /** Renders this complex number at `precision` decimal places for REPL display.
   *  @param precision number of decimal places to show
   */
  def display(precision: Int): String =
    val r = _Number.round(re, precision)
    val m = _Number.round(im, precision)
    def imTerm(x: Double): String = x match
      case 1.0  => "i"
      case -1.0 => "-i"
      case _    => s"${x}i"
    if m == 0.0 then r.toString
    else if r == 0.0 then imTerm(m)
    else
      val (sign, mag) = if m < 0.0 then ("-", -m) else ("+", m)
      val magStr = if mag == 1.0 then "i" else s"${mag}i"
      s"($r $sign $magStr)"
