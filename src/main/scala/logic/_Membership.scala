package it.grypho.scala.leonardo
package logic

import core.*


/** Marker trait for the nodes that *produce* a truth degree from crisp input: the
 *  hedges ([[Very]], [[Somewhat]]) and the membership curves ([[TriMF]], [[TrapMF]],
 *  [[GaussMF]], [[SigMF]]), plus the explicit conversion [[_TruthOf]].
 *
 *  These are the fuzzy tier's entry points: a membership function maps a crisp
 *  measurement into `[0, 1]`, and everything downstream is an ordinary truth value that
 *  the connectives already understand.  Their `eval` therefore returns `_Truth.of(degree)`
 *  rather than a plain number — a membership degree *is* a truth value, and returning one
 *  is what lets `very(trimf(t, 0, 10, 20)) and somewhat(x)` compose without an explicit
 *  cast at every step.
 *
 *  Arguments are read with [[asDegree]], which accepts a bare number in `[0, 1]` as well
 *  as a `_Bool`/`_Truth`: a hedge has no other domain, so there is none of the ambiguity
 *  that makes the connectives insist on [[asTruth]].  An argument outside the domain, or
 *  one that does not reduce, leaves the node symbolic.
 */
sealed trait _Membership extends _Expression


/** Evaluates `args` and applies `f` to the resulting degrees, or stays symbolic.
 *
 *  @param args   the sub-expressions to reduce
 *  @param env    evaluation environment
 *  @param read   how to read each reduced operand (degree or plain number)
 *  @param f      the curve, applied when every operand reduced
 *  @param wrap   rebuilds the symbolic residual from the reduced operands
 *  @return `Right(_Truth.of(f(...)))` on success, `Left(residual)` otherwise
 */
private def reduceMembership(
    args: List[_Expression],
    env:  Environment,
    read: _Value => Option[Double],
    f:    List[Double] => Option[Double],
    wrap: List[_Expression] => _Expression
): Either[_Expression, _Value] =
  val reduced = args.map(_.eval(env))
  val degrees = reduced.foldRight(Option(List.empty[Double])) { (r, acc) =>
    for tail <- acc; v <- r.toOption; d <- read(v) yield d :: tail
  }
  degrees.flatMap(f) match
    case Some(d) => Right(_Truth.of(d))
    case None    => Left(wrap(reduced.map(_.toExpression)))


/** Explicit conversion of a scalar degree into a truth value: `truth(x)`.
 *
 *  Also the printed form of a graded `core._Truth` (`truth(0.25)`), which is what makes
 *  a fuzzy degree round-trip through `toString` and through a `:save`/`:load` — without
 *  it a degree would print as a bare number and re-parse as a plain `_Number`.  The
 *  Kleene midpoint keeps its own literal, `unknown`.
 *
 *  Out-of-range arguments are clamped by `_Truth.of`, matching that factory's contract.
 *
 *  @param a the degree expression
 */
case class _TruthOf(a: _Expression) extends _Membership:
  override def toString: String = s"truth($a)"
  override def children: List[_Expression] = List(a)
  override def rebuild(c: List[_Expression]): _Expression = _TruthOf(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(a), env, asDegree(_, env.symmetricLogic),
      { case d :: Nil => Some(d); case _ => None }, cs => _TruthOf(cs.head))


/** The concentration hedge `very(a) = a²`: sharpens a degree towards its extremes.
 *  @param a the degree expression
 */
case class Very(a: _Expression) extends _Membership:
  override def toString: String = s"very($a)"
  override def children: List[_Expression] = List(a)
  override def rebuild(c: List[_Expression]): _Expression = Very(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(a), env, asDegree(_, env.symmetricLogic),
      { case d :: Nil => Some(d * d); case _ => None }, cs => Very(cs.head))


/** The dilation hedge `somewhat(a) = √a`: softens a degree towards the middle.
 *  @param a the degree expression
 */
case class Somewhat(a: _Expression) extends _Membership:
  override def toString: String = s"somewhat($a)"
  override def children: List[_Expression] = List(a)
  override def rebuild(c: List[_Expression]): _Expression = Somewhat(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(a), env, asDegree(_, env.symmetricLogic),
      { case d :: Nil => Some(math.sqrt(d)); case _ => None }, cs => Somewhat(cs.head))


/** Triangular membership curve `trimf(x, a, b, c)`: rises from `a` to the apex `b`,
 *  falls from `b` to `c`, and is zero outside `[a, c]`.
 *
 *  Degenerate shoulders (`a == b` or `b == c`) give a vertical edge rather than a
 *  division by zero.  Requires `a <= b <= c`; otherwise the node stays symbolic.
 *
 *  @param x the crisp input
 *  @param a left foot
 *  @param b apex
 *  @param c right foot
 */
case class TriMF(x: _Expression, a: _Expression, b: _Expression, c: _Expression) extends _Membership:
  override def toString: String = s"trimf($x, $a, $b, $c)"
  override def children: List[_Expression] = List(x, a, b, c)
  override def rebuild(cs: List[_Expression]): _Expression = TriMF(cs.head, cs(1), cs(2), cs(3))

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(x, a, b, c), env, numberOf, {
      case xv :: av :: bv :: cv :: Nil if av <= bv && bv <= cv =>
        Some(triangle(xv, av, bv, cv))
      case _ => None
    }, cs => TriMF(cs.head, cs(1), cs(2), cs(3)))


/** Trapezoidal membership curve `trapmf(x, a, b, c, d)`: rises from `a` to `b`, is one
 *  on the plateau `[b, c]`, falls from `c` to `d`, and is zero outside `[a, d]`.
 *  Requires `a <= b <= c <= d`; otherwise the node stays symbolic.
 *
 *  @param x the crisp input
 *  @param a left foot
 *  @param b left shoulder
 *  @param c right shoulder
 *  @param d right foot
 */
case class TrapMF(x: _Expression, a: _Expression, b: _Expression, c: _Expression, d: _Expression)
    extends _Membership:
  override def toString: String = s"trapmf($x, $a, $b, $c, $d)"
  override def children: List[_Expression] = List(x, a, b, c, d)
  override def rebuild(cs: List[_Expression]): _Expression = TrapMF(cs.head, cs(1), cs(2), cs(3), cs(4))

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(x, a, b, c, d), env, numberOf, {
      case xv :: av :: bv :: cv :: dv :: Nil if av <= bv && bv <= cv && cv <= dv =>
        if xv >= bv && xv <= cv then Some(1.0)
        else if xv <= av || xv >= dv then Some(0.0)
        else if xv < bv then Some(rise(xv, av, bv))
        else Some(fall(xv, cv, dv))
      case _ => None
    }, cs => TrapMF(cs.head, cs(1), cs(2), cs(3), cs(4)))


/** Gaussian membership curve `gaussmf(x, mean, sigma) = exp(-(x - mean)² / (2σ²))`.
 *  A non-positive `sigma` leaves the node symbolic (no curve is defined).
 *
 *  @param x     the crisp input
 *  @param mean  the centre, where the degree is one
 *  @param sigma the standard deviation; must be positive
 */
case class GaussMF(x: _Expression, mean: _Expression, sigma: _Expression) extends _Membership:
  override def toString: String = s"gaussmf($x, $mean, $sigma)"
  override def children: List[_Expression] = List(x, mean, sigma)
  override def rebuild(cs: List[_Expression]): _Expression = GaussMF(cs.head, cs(1), cs(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(x, mean, sigma), env, numberOf, {
      case xv :: m :: s :: Nil if s > 0.0 =>
        val z = (xv - m) / s
        Some(math.exp(-0.5 * z * z))
      case _ => None
    }, cs => GaussMF(cs.head, cs(1), cs(2)))


/** Sigmoid membership curve `sigmf(x, a, c) = 1 / (1 + exp(-a(x - c)))`.
 *  A positive `a` opens to the right, a negative one to the left.
 *
 *  @param x the crisp input
 *  @param a the slope
 *  @param c the inflection point, where the degree is one half
 */
case class SigMF(x: _Expression, a: _Expression, c: _Expression) extends _Membership:
  override def toString: String = s"sigmf($x, $a, $c)"
  override def children: List[_Expression] = List(x, a, c)
  override def rebuild(cs: List[_Expression]): _Expression = SigMF(cs.head, cs(1), cs(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceMembership(List(x, a, c), env, numberOf, {
      case xv :: av :: cv :: Nil =>
        val d = 1.0 / (1.0 + math.exp(-av * (xv - cv)))
        Option.when(!d.isNaN)(d)
      case _ => None
    }, cs => SigMF(cs.head, cs(1), cs(2)))


/** Reads a plain real from a reduced value; the curves take crisp inputs, not degrees. */
private def numberOf(v: _Value): Option[Double] = v match
  case _Number(d) if !d.isNaN && !d.isInfinite => Some(d)
  case _                                       => None

/** Rising shoulder from `lo` (degree 0) to `hi` (degree 1); vertical when `lo == hi`. */
private def rise(x: Double, lo: Double, hi: Double): Double =
  if hi == lo then 1.0 else (x - lo) / (hi - lo)

/** Falling shoulder from `lo` (degree 1) to `hi` (degree 0); vertical when `lo == hi`. */
private def fall(x: Double, lo: Double, hi: Double): Double =
  if hi == lo then 1.0 else (hi - x) / (hi - lo)

/** The triangular curve with feet `a`, `c` and apex `b`.
 *
 *  The apex is tested before the support so a degenerate shoulder (`a == b` or `b == c`)
 *  reads as a vertical edge at full membership rather than falling into the
 *  outside-the-support case.
 */
private def triangle(x: Double, a: Double, b: Double, c: Double): Double =
  if x == b then 1.0
  else if x <= a || x >= c then 0.0
  else if x < b then rise(x, a, b)
  else fall(x, b, c)
