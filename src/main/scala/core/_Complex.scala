package it.grypho.scala.leonardo
package core

import scala.math.{exp, log, sin, cos, sinh, cosh, atan2, hypot}


// Concrete complex value: a fully-reduced (re, im) pair, sibling to _Number rather
// than a widening of it. This is the deliberate design that keeps the change cheap:
// every existing `_Number(x)` pattern match across the codebase keeps firing on real
// results, because the smart factory `_Complex.of` collapses a zero imaginary part
// back to a plain `_Number`. Only a genuinely non-real value ever becomes a _Complex.
//
// Like _Number, a _Complex never rounds in eval — the stored Doubles are propagated
// as-is and rounding is a display concern handled in toString / display(p). So
// exp(i·π) reduces to _Complex(-1.0, 1.22e-16) (sin(π) is floating-point noise) and
// prints as "-1.0" once the imaginary part rounds away; the equation tolerance treats
// it as real, exactly as it already does for real noise like sin(π) ≈ 0.
//
// Arithmetic lives in the companion as functions over _Value operands (a _Number is
// viewed as (x, 0)), returning Option[_Value]: None signals a non-finite or undefined
// result (log 0, 0^0, a non-numeric operand) so the caller stays symbolic, mirroring
// the "domain errors stay symbolic" contract of the real path.
object _Complex:

  /** Smart factory: a zero imaginary part collapses to the real _Number, so the real
   *  fast-path and every _Number pattern match keep working. Only a genuinely non-real
   *  value becomes a _Complex. */
  def of(re: Double, im: Double): _Value =
    if im == 0.0 then _Number(re) else new _Complex(re, im)

  /** (re, im) view of any numeric value; None for non-numeric values (_Bool, matrices). */
  def parts(v: _Value): Option[(Double, Double)] = v match
    case _Number(x)  => Some((x, 0.0))
    case c: _Complex => Some((c.re, c.im))
    case _           => None

  private def finiteVal(re: Double, im: Double): Option[_Value] =
    if re.isNaN || re.isInfinite || im.isNaN || im.isInfinite then None else Some(of(re, im))

  // Binary field operations. Each promotes _Number operands to (x, 0) via `parts`,
  // returns None if either operand is non-numeric or the result is non-finite.
  def add(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- finiteVal(ar + br, ai + bi) yield r

  def sub(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- finiteVal(ar - br, ai - bi) yield r

  def mul(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b)
        r <- finiteVal(ar * br - ai * bi, ar * bi + ai * br) yield r

  def div(a: _Value, b: _Value): Option[_Value] =
    for (ar, ai) <- parts(a); (br, bi) <- parts(b); r <- divParts(ar, ai, br, bi) yield r

  private def divParts(ar: Double, ai: Double, br: Double, bi: Double): Option[_Value] =
    val denom = br * br + bi * bi
    if denom == 0.0 then None    // division by zero stays symbolic, as in the real path
    else finiteVal((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)

  /** Principal complex power z^w = exp(w · log z). 0^w is 0 for a positive real w and
   *  undefined (None) otherwise, matching the real path's "0^negative stays symbolic". */
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

  // Elementary functions on a complex argument (standard identities).
  def expc(v: _Value): Option[_Value] =
    for (a, b) <- parts(v); r <- finiteVal(exp(a) * cos(b), exp(a) * sin(b)) yield r

  def logc(v: _Value): Option[_Value] =
    parts(v).flatMap { (a, b) =>
      val mod = hypot(a, b)
      if mod == 0.0 then None else finiteVal(log(mod), atan2(b, a))
    }

  def sinc(v: _Value): Option[_Value] =
    for (a, b) <- parts(v); r <- finiteVal(sin(a) * cosh(b), cos(a) * sinh(b)) yield r

  def cosc(v: _Value): Option[_Value] =
    for (a, b) <- parts(v); r <- finiteVal(cos(a) * cosh(b), -(sin(a) * sinh(b))) yield r

  def tanc(v: _Value): Option[_Value] =
    for s <- sinc(v); c <- cosc(v); r <- div(s, c) yield r


// The imaginary part is never exactly 0 (the `of` factory guarantees it), so a
// _Complex is always genuinely non-real. Constructor is private: all construction
// routes through `of` to preserve that invariant; pattern matching `_Complex(re, im)`
// stays available to the whole codebase.
case class _Complex private (re: Double, im: Double) extends _Value:
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  // toString rounds for display only (DefaultPrecision), mirroring _Number: a real
  // residual in the imaginary part rounds away (exp(i·π) prints "-1.0"), a purely
  // imaginary value prints as "<b>i" ("i" / "-i" for ±1), and the full form is the
  // re-parsable "(a + bi)" / "(a - bi)".
  override def toString: String = display(Environment.DefaultPrecision)

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
