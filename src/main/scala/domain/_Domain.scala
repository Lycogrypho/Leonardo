package it.grypho.scala.leonardo
package domain

import core.*
import scalar.*
import matrix._Matrix
import equation.{_Comparison, CompareOp, _Equation}
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
    render(domainOf(e, v, kind, env), v).fold(Left(this))(r => Left(r))


/** `differentiable(e, v)` — where `e` is differentiable, written as a relation in `v`. */
case class _Differentiable(e: _Expression, v: _Variable, kind: DomainKind = DomainKind.Real)
    extends _Expression:
  override def toString: String =
    if kind == DomainKind.Real then s"differentiable($e, $v)"
    else s"differentiable($e, $v, complex)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Differentiable(c.head, v, kind)

  override def eval(env: Environment): Either[_Expression, _Value] =
    render(differentiableDomainOf(e, v, kind, env), v).fold(Left(this))(r => Left(r))


/** `singularities(e, v)` — the isolated singularities of a rational `e`.
 *
 *  Three outcomes, deliberately distinguishable:
 *
 *  - **some** — a `2×n` matrix, locations in the first row and pole order in the second,
 *    with `0` marking a removable singularity.  Two rows rather than one because the order
 *    is the part 6.14 (Laurent) actually needs — it is the length of the principal part —
 *    and a bare list of locations would lose it.
 *  - **provably none** — `_Bool(false)`, the empty set.  This follows 3.2, which renders an
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
        val locs   = ss.map(s => _Number(s.at): _Expression)
        val orders = ss.map(s => (s.kind match
                                    case SingularityKind.Removable  => _Number(0)
                                    case SingularityKind.Pole(o)    => _Number(o)): _Expression)
        Left(_Matrix(2, ss.size, locs ++ orders))


/** Renders a [[scalar.DomainSet]] as a relation in `v`, or `None` when it cannot be written.
 *
 *  Prefers the resolved intervals, which give a tight answer (`x < -1 or x > 1`).  Falls
 *  back to the raw constraints, which survive symbolic coefficients (`x - a > 0`) — the
 *  whole reason `DomainSet` keeps both.
 */
private[domain] def render(d: DomainSet, v: _Variable): Option[_Expression] =
  if d.isEmpty then Some(_Bool(false))
  else if d.isUnrestricted then Some(_Bool(true))
  else d.intervals match
    case Some(is) => Some(renderIntervals(is, v))
    case None     => renderConstraints(d.constraints)

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
private def renderConstraints(cs: Vector[Constraint]): Option[_Expression] =
  val parts = cs.map(renderConstraint)
  if parts.exists(_.isEmpty) then None
  else Some(parts.flatten.reduce((a, b) => And(a, b)))

private def renderConstraint(c: Constraint): Option[_Expression] =
  import Requirement.*
  c.req match
    case Positive     => Some(_Comparison(c.arg, CompareOp.Gt, _Number(0)))
    case NonNegative  => Some(_Comparison(c.arg, CompareOp.Ge, _Number(0)))
    case NonZero      => Some(_Comparison(c.arg, CompareOp.Ne, _Number(0)))
    case InClosedUnit => Some(And(_Comparison(c.arg, CompareOp.Ge, _Number(-1)),
                                  _Comparison(c.arg, CompareOp.Le, _Number(1))))
    case Never        => Some(_Bool(false))
    // Infinite exclusion sets: the language has no quantifier, and a truncated list would
    // read as exhaustive.
    case NotOddMultipleOfHalfPi | NotNonPositiveInteger => None
