package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix._Matrix


/** The analysis and conversion functions at the prompt (F_0092): `tfpoles`, `tfzeros`,
 *  `isStable`, `isStableDiscrete` and `c2d`, each a thin node over the library function of the
 *  same meaning, so the prompt and Scala cannot disagree. `stateSpace(A, B, C, D)` needs no node:
 *  the parser builds the `1×4` row directly, which *is* the model.
 *
 *  **The prompt names differ from the Scala ones where a collision forced it** (Decision A):
 *  `zeros(r, c)` is the zero-matrix constructor, so the pair is `tfpoles`/`tfzeros` — the `tf`
 *  prefix the grammar uses as a disambiguator, MATLAB's family prefix. Renaming the published
 *  Scala `poles`/`zeros` would be a binary break for a spelling, so they stay.
 *
 *  Each node reads `G` with the session's bound parameters substituted **except the frequency
 *  variable itself**: `a := 2` makes `tfpoles(1/(s + a), s)` answer `-2`, while a stray
 *  `s := 3` must not turn `G` into a number.
 */

/** `G` with every bound name but `v` replaced by its value, then evaluated with no bindings at
 *  all — so a nested node such as `arx2tf(…)` or `feedback(…)` folds to its rational, while
 *  `v` stays free even when the session happens to bind it. */
private def resolved(g: _Expression, v: _Variable, env: Environment): _Expression =
  val bound = substitute(g, (g.freeVars - v.variable).flatMap(n => env.get(n).map(value => n -> (value: _Expression))).toMap)
  bound.eval(new Environment(env.precision, Map(), env.symmetricLogic, env.semantics, env.workingPrecision)).toExpression

/** Whether `G` provably has no poles (`denominator`) or no zeros (`!denominator`): that side of
 *  the fraction is a constant in lowest terms. Read here, not from [[poles]], whose `None` for a
 *  constant means "no roots to find" — and changing that would make [[isStable]] call an
 *  improper polynomial `G` vacuously stable. */
private def provablyNone(g: _Expression, v: _Variable, denominator: Boolean): Boolean =
  coeffsOf(g, v).exists((num, den) => polyDegree(polyTrim(if denominator then den else num)) <= 0)

/** Why the roots of `G` in `v` are not available: not a rational function with numeric
 *  coefficients, or roots the root finder could not resolve. */
private def rootsDecline(g: _Expression, v: _Variable): String =
  if rationalCoeffs(g, v).isEmpty then
    s"the argument is not a rational function of ${v.variable} with numeric coefficients"
  else s"its roots in ${v.variable} could not be resolved"

/** Roots as the answer: a `1×n` row sorted by real then imaginary part (so the printed answer is
 *  reproducible whatever order the root finder returns), or `false` for provably none — the
 *  `stationary`/`singularities` convention, since a matrix cannot have zero columns. */
private def rootsRow(rs: Vector[_Value]): Either[_Expression, _Value] =
  def key(v: _Value): (Double, Double) = v match
    case c: _Complex => _Complex.parts(c).getOrElse((Double.NaN, Double.NaN))
    case _Number(d)  => (d, 0.0)
    case _           => (Double.NaN, Double.NaN)
  if rs.isEmpty then Right(_Bool(false))
  else Left(_Matrix(1, rs.size, rs.sortBy(key)))


/** `tfpoles(G, s)` — the poles of `G`, [[poles]] at the prompt (F_0092 Decision A).
 *  @param g the transfer function
 *  @param v the frequency variable, excluded from `children` (the `_Series` convention) */
case class _TfPoles(g: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"tfpoles($g, $v)"
  override def children: List[_Expression] = List(g)
  override def rebuild(c: List[_Expression]): _Expression = _TfPoles(c.head, v)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val b = resolved(g, v, env)
    if provablyNone(b, v, denominator = true) then Right(_Bool(false))
    else poles(b, v).map(rootsRow).getOrElse(Left(this))

  /** Why this call stays unevaluated, or `None` when it answers.
   *  @param env the evaluation environment
   *  @return the reason */
  def declineReason(env: Environment): Option[String] =
    Option.when(eval(env) == Left(this))(rootsDecline(resolved(g, v, env), v))


/** `tfzeros(G, s)` — the zeros of `G`, [[zeros]] at the prompt; `tfzeros` because `zeros(r, c)`
 *  is the zero-matrix constructor (F_0092 Decision A).
 *  @param g the transfer function
 *  @param v the frequency variable */
case class _TfZeros(g: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"tfzeros($g, $v)"
  override def children: List[_Expression] = List(g)
  override def rebuild(c: List[_Expression]): _Expression = _TfZeros(c.head, v)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val b = resolved(g, v, env)
    if provablyNone(b, v, denominator = false) then Right(_Bool(false))
    else zeros(b, v).map(rootsRow).getOrElse(Left(this))

  /** Why this call stays unevaluated, or `None` when it answers.
   *  @param env the evaluation environment
   *  @return the reason */
  def declineReason(env: Environment): Option[String] =
    Option.when(eval(env) == Left(this))(rootsDecline(resolved(g, v, env), v))


/** `isStable(G, s)` or `isStableDiscrete(G, z)` — [[isStable]] / [[isStableDiscrete]] at the
 *  prompt (F_0092 Decision B: the Scala names, a predicate named as a question). Strict in both:
 *  a pole on the imaginary axis, or on the unit circle, is not stable.
 *  @param g        the transfer function
 *  @param v        the frequency variable
 *  @param discrete whether to test the unit circle rather than the left half-plane */
case class _IsStable(g: _Expression, v: _Variable, discrete: Boolean = false) extends _Expression:
  override def toString: String = s"${if discrete then "isStableDiscrete" else "isStable"}($g, $v)"
  override def children: List[_Expression] = List(g)
  override def rebuild(c: List[_Expression]): _Expression = _IsStable(c.head, v, discrete)

  private def verdict(env: Environment): Option[Boolean] =
    val b = resolved(g, v, env)
    if discrete then isStableDiscrete(b, v) else isStable(b, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    verdict(env).map(ok => Right(_Bool(ok))).getOrElse(Left(this))

  /** Why this call stays unevaluated, or `None` when it answers.
   *  @param env the evaluation environment
   *  @return the reason */
  def declineReason(env: Environment): Option[String] =
    Option.when(verdict(env).isEmpty)(rootsDecline(resolved(g, v, env), v))


/** `c2d(G, s, z, Ts, zoh | tustin)` — [[c2d]] at the prompt (F_0092). The method is a slot word,
 *  always written and **not reserved** (the `rk45`/`pbfgs` precedent), because the same plant
 *  discretised the two ways has different discrete poles.
 *  @param g      the continuous transfer function
 *  @param s      the continuous frequency variable
 *  @param z      the discrete variable of the result
 *  @param ts     the sample period, a positive number
 *  @param method [[Zoh]] or [[Tustin]] */
case class _C2d(g: _Expression, s: _Variable, z: _Variable, ts: _Expression, method: DiscretisationMethod) extends _Expression:
  override def toString: String =
    s"c2d($g, $s, $z, $ts, ${if method == Zoh then "zoh" else "tustin"})"
  override def children: List[_Expression] = List(g, ts)
  override def rebuild(c: List[_Expression]): _Expression = _C2d(c.head, s, z, c(1), method)

  /** The sample period, when it is a positive number. */
  private def period(env: Environment): Option[Double] =
    ts.eval(env) match
      case Right(_Number(d)) if d > 0 && d.isFinite => Some(d)
      case _                                        => None

  override def eval(env: Environment): Either[_Expression, _Value] =
    period(env).flatMap(t => c2d(resolved(g, s, env), s, z, t, method)) match
      case Some(v: _Value) => Right(v)
      case Some(e)         => Left(e)
      case None            => Left(this)

  /** Why this call stays unevaluated, or `None` when it answers.
   *  @param env the evaluation environment
   *  @return the reason */
  def declineReason(env: Environment): Option[String] =
    period(env) match
      case None => Some("the sample period must be a positive number")
      case Some(t) =>
        Option.when(c2d(resolved(g, s, env), s, z, t, method).isEmpty)(
          if method == Zoh then "the zero-order-hold equivalent did not close: it needs the inverse Laplace and z-transform tiers to handle the plant"
          else "the Tustin substitution did not fold to a rational function of " + z.variable)
