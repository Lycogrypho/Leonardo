package it.grypho.scala.leonardo
package domain

import core.*
import scalar.*
import matrix._Matrix
import equation.{_Comparison, CompareOp, _Equation, exactCutPoints, exactDomainChart, withExactLeaves}
import logic.{And, Or}


/** `domain(e, v)` — where `e` is defined, written as a relation in `v`.
 *
 *  `v` follows the `scalar._Taylor` convention rather than the binder convention: it
 *  appears **free in the result** (`x > 0`), so it is excluded from `children` but carried
 *  through `rebuild` so `substitute` cannot rewrite the variable the answer is phrased in.
 *
 *  Stays symbolic when the domain cannot be written down — see the package overview.
 *
 *  @param e    the expression to analyse
 *  @param v    the variable the answer is expressed in
 *  @param kind real or complex
 */
case class _Domain(e: _Expression, v: _Variable, kind: DomainKind = DomainKind.Real)
    extends _Expression:
  override def toString: String =
    if kind == DomainKind.Real then s"domain($e, $v)" else s"domain($e, $v, complex)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Domain(c.head, v, kind)

  override def eval(env: Environment): Either[_Expression, _Value] =
    renderIn(domainOf(e, v, kind, env), e, v, env, smooth = false).fold(Left(this))(r => Left(r))


/** `differentiable(e, v)` — where `e` is differentiable, written as a relation in `v`. */
case class _Differentiable(e: _Expression, v: _Variable, kind: DomainKind = DomainKind.Real)
    extends _Expression:
  override def toString: String =
    if kind == DomainKind.Real then s"differentiable($e, $v)"
    else s"differentiable($e, $v, complex)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Differentiable(c.head, v, kind)

  override def eval(env: Environment): Either[_Expression, _Value] =
    renderIn(differentiableDomainOf(e, v, kind, env), e, v, env, smooth = kind == DomainKind.Real).fold(Left(this))(r => Left(r))


/** `singularities(e, v)` — the isolated singularities of a rational `e`.
 *
 *  Three outcomes, deliberately distinguishable:
 *
 *  - **some** — a `2×n` matrix, locations in the first row and pole order in the second,
 *    with `0` marking a removable singularity.  Two rows rather than one because the order
 *    is the part the Laurent series actually needs — it is the length of the principal part —
 *    and a bare list of locations would lose it.
 *  - **provably none** — `_Bool(false)`, the empty set.  This follows the inequality solver, which renders an
 *    empty solution set the same way, and `_Matrix` cannot be `2×0` in any case.
 *  - **cannot enumerate** — stays symbolic, which is everything that is not a numeric
 *    rational function.  Collapsing this into "none" would turn "I do not know" into a
 *    confident and wrong answer.
 *
 *  Varying the result type by cardinality is the convention `equation._Solve` already
 *  uses: one solution is an equation, several are a matrix, none stays symbolic.
 */
case class _Singularities(e: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"singularities($e, $v)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Singularities(c.head, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    singularitiesOf(e, v, env) match
      case None                  => Left(this)
      case Some(ss) if ss.isEmpty => Left(_Bool(false))
      case Some(ss) =>
        // F_0337: in the exact tier the orders are exact integers and a location the exact root it reads
        val exact  = _Rational.containsExactIn(e, env)
        val locs   = ss.map(s => _Number(s.at): _Expression)
        val orders = ss.map(s => (s.kind match
                                    case SingularityKind.Removable  => literal(0, exact)
                                    case SingularityKind.Pole(o)    => literal(o, exact)): _Expression)
        val cells  = if exact then locs.map(withExactLeaves(_, exactCutPoints(e, v, 0, env))) else locs
        // F_0343: an exact rational function's poles and orders are read exactly, so two close poles
        // are not one double pole
        val exactly = if exact then exactSingularities(e, v, env).filter(_.nonEmpty) else None
        exactly match
          case Some(xs) => Left(_Matrix(2, xs.size, xs.map(_._1.expression) ++ xs.map(x => literal(x._2, true))))
          case None     => Left(_Matrix(2, ss.size, cells ++ orders))


/** Renders a [[scalar.DomainSet]] as a relation in `v`, or `None` when it cannot be written.
 *
 *  Prefers the resolved intervals, which give a tight answer (`x < -1 or x > 1`).  Falls
 *  back to the raw constraints, which survive symbolic coefficients (`x - a > 0`) — the
 *  whole reason `DomainSet` keeps both.
 */
private[domain] def render(d: DomainSet, v: _Variable): Option[_Expression] = render(d, v, exact = false)

/** [[render]] in the tier of `e` (F_0337): under `exact on`, or for exact input, the constants the
 *  rendering invents (`0`, `±1`) are exact, and each interval endpoint — a root found in `Double` — is
 *  replaced by the exact root of the constraint it reads, where one exists.  `domain(ln(x - 1/3), x)`
 *  answered `x > 0.33333`. */
private def renderIn(d: DomainSet, e: _Expression, v: _Variable, env: Environment, smooth: Boolean): Option[_Expression] =
  if !_Rational.containsExactIn(e, env) then render(d, v)
  else
    def thresholds(r: Requirement): Seq[Int] = r match
      case Requirement.Positive | Requirement.NonNegative | Requirement.NonZero => Seq(0)
      case Requirement.InClosedUnit                                             => Seq(-1, 1)
      case _                                                                    => Seq.empty
    lazy val candidates = d.constraints.flatMap(c => thresholds(c.req).flatMap(t => exactCutPoints(c.arg, v, t, env))).distinct
    // F_0343: exact rational constraints are charted exactly, so two close roots are not merged
    if d.isEmpty || d.isUnrestricted then render(d, v, exact = true)
    else exactDomainChart(d.constraints, v, env, p => smooth && smoothPointAt(e, v, p, env))
      .orElse(render(d, v, exact = true).map(withExactLeaves(_, candidates)))

private def render(d: DomainSet, v: _Variable, exact: Boolean): Option[_Expression] =
  if d.isEmpty then Some(_Bool(false))
  else if d.isUnrestricted then Some(_Bool(true))
  else d.intervals match
    case Some(is) => Some(renderIntervals(is, v))
    case None     => renderConstraints(d.constraints, exact)

/** `n` in the tier asked for. */
private def literal(n: Int, exact: Boolean): _Expression = if exact then _Rational(n) else _Number(n)

/** A union of intervals as an `or` of relations. */
private def renderIntervals(is: Vector[Interval], v: _Variable): _Expression =
  if is.isEmpty then _Bool(false)
  else is.map(i => renderInterval(i, v)).reduce((a, b) => Or(a, b))

private def renderInterval(i: Interval, v: _Variable): _Expression =
  val loInf = i.lo.isNegInfinity
  val hiInf = i.hi.isPosInfinity
  if loInf && hiInf then _Bool(true)
  else if i.lo == i.hi && i.loIncl && i.hiIncl then _Equation(v, _Number(i.lo))
  else if loInf then _Comparison(v, if i.hiIncl then CompareOp.Le else CompareOp.Lt, _Number(i.hi))
  else if hiInf then _Comparison(v, if i.loIncl then CompareOp.Ge else CompareOp.Gt, _Number(i.lo))
  else And(_Comparison(v, if i.loIncl then CompareOp.Ge else CompareOp.Gt, _Number(i.lo)),
           _Comparison(v, if i.hiIncl then CompareOp.Le else CompareOp.Lt, _Number(i.hi)))

/** The raw constraints as an `and` of relations, or `None` if any cannot be expressed. */
private def renderConstraints(cs: Vector[Constraint], exact: Boolean): Option[_Expression] =
  val parts = cs.map(renderConstraint(_, exact))
  if parts.exists(_.isEmpty) then None
  else Some(parts.flatten.reduce((a, b) => And(a, b)))

private def renderConstraint(c: Constraint, exact: Boolean): Option[_Expression] =
  import Requirement.*
  def n(k: Int) = literal(k, exact)
  c.req match
    case Positive     => Some(_Comparison(c.arg, CompareOp.Gt, n(0)))
    case NonNegative  => Some(_Comparison(c.arg, CompareOp.Ge, n(0)))
    case NonZero      => Some(_Comparison(c.arg, CompareOp.Ne, n(0)))
    case InClosedUnit => Some(And(_Comparison(c.arg, CompareOp.Ge, n(-1)),
                                  _Comparison(c.arg, CompareOp.Le, n(1))))
    case Never        => Some(_Bool(false))
    // Infinite exclusion sets: the language has no quantifier, and a truncated list would
    // read as exhaustive.
    case NotOddMultipleOfHalfPi | NotNonPositiveInteger | NotInteger | NotHalfInteger => None
