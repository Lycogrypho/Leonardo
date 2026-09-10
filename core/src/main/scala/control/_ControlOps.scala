package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import transform.*


/** AST nodes for the control operators (issue 6.29).
 *
 *  Each holds its **frequency variable explicitly**, since a transfer function is a plain
 *  `Ratio` with no type to dispatch on.  The variables are excluded from `children` and
 *  carried through `rebuild`, the `_Laplace` convention, so `substitute` cannot rewrite the
 *  variable an answer is phrased in.
 */

/** Series connection: `series(G, H, v)`. */
case class _Series(g: _Expression, h: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"series($g, $h, $v)"
  override def children: List[_Expression] = List(g, h)
  override def rebuild(c: List[_Expression]): _Expression = _Series(c.head, c(1), v)
  override def eval(env: Environment): Either[_Expression, _Value] =
    series(g, h, v).eval(env)

/** Parallel connection: `parallel(G, H, v)`. */
case class _Parallel(g: _Expression, h: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"parallel($g, $h, $v)"
  override def children: List[_Expression] = List(g, h)
  override def rebuild(c: List[_Expression]): _Expression = _Parallel(c.head, c(1), v)
  override def eval(env: Environment): Either[_Expression, _Value] =
    parallel(g, h, v).eval(env)

/** Closed loop with negative feedback: `feedback(G, H, v)` = `G/(1 + G*H)`. */
case class _Feedback(g: _Expression, h: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"feedback($g, $h, $v)"
  override def children: List[_Expression] = List(g, h)
  override def rebuild(c: List[_Expression]): _Expression = _Feedback(c.head, c(1), v)
  override def eval(env: Environment): Either[_Expression, _Value] =
    feedback(g, h, v).eval(env)

/** Unit-step response: `step(G, s, t)` = `L^-1{G(s)/s}`. */
case class _StepResponse(g: _Expression, s: _Variable, t: _Variable) extends _Expression:
  override def toString: String = s"step($g, $s, $t)"
  override def children: List[_Expression] = List(g)
  override def rebuild(c: List[_Expression]): _Expression = _StepResponse(c.head, s, t)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = stepResponse(g, s, t)
    if result == this then Left(this) else result.eval(env)

/** Impulse response: `impulse(G, s, t)` = `L^-1{G(s)}`. */
case class _ImpulseResponse(g: _Expression, s: _Variable, t: _Variable) extends _Expression:
  override def toString: String = s"impulse($g, $s, $t)"
  override def children: List[_Expression] = List(g)
  override def rebuild(c: List[_Expression]): _Expression = _ImpulseResponse(c.head, s, t)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = impulseResponse(g, s, t)
    if result == this then Left(this) else result.eval(env)

/** Unit-step response `L^-1{G(s)/s}` — almost entirely a re-spelling of issue 3.17.
 *
 *  **Declines for an improper `G`** (numerator degree above denominator degree).  Such a
 *  response contains an impulsive term at `t = 0` that the inverse-transform tier does not
 *  represent, so returning only the smooth part would be a confidently incomplete answer
 *  rather than a refusal.
 *
 *  @param g the transfer function
 *  @param s the frequency variable
 *  @param t the time variable, free in the result
 *  @return the response, or [[_StepResponse]] unchanged when no rule applies
 */
def stepResponse(g: _Expression, s: _Variable, t: _Variable): _Expression =
  if improper(g, s) then _StepResponse(g, s, t)
  else
    // `normaliseTf`, not `simplifyFully`: G/s is a Ratio whose numerator is itself a Ratio, and
    // simplification does not fold nested fractions -- the inverse-transform table matches on
    // shape, so it would decline the whole of the most ordinary case there is.
    val result = inverseLaplaceOf(normaliseTf(Ratio(g, s), s), s, t)
    if result.isInstanceOf[_InverseLaplace] then _StepResponse(g, s, t) else result

/** Impulse response `L^-1{G(s)}`.  Declines for an improper `G`, as [[stepResponse]] does.
 *
 *  @param g the transfer function
 *  @param s the frequency variable
 *  @param t the time variable, free in the result
 *  @return the response, or [[_ImpulseResponse]] unchanged when no rule applies
 */
def impulseResponse(g: _Expression, s: _Variable, t: _Variable): _Expression =
  if improper(g, s) then _ImpulseResponse(g, s, t)
  else
    val result = inverseLaplaceOf(g, s, t)
    if result.isInstanceOf[_InverseLaplace] then _ImpulseResponse(g, s, t) else result

/** True when `G`'s numerator degree exceeds its denominator degree. */
private def improper(g: _Expression, v: _Variable): Boolean =
  rationalCoeffs(g, v).exists((num, den) => polyDegree(polyTrim(num)) > polyDegree(polyTrim(den)))
