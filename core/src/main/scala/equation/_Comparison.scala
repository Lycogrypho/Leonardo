package it.grypho.scala.leonardo
package equation

import core.*


/** The ordering relations, and inequality.
 *
 *  `==` is deliberately absent: that is [[_EqualityCheck]], which already exists and which
 *  `!=` is defined as the negation of.
 */
enum CompareOp:
  /** `<` */  case Lt
  /** `>` */  case Gt
  /** `<=` */ case Le
  /** `>=` */ case Ge
  /** `!=` */ case Ne

object CompareOp:
  /** The operator as written, which is also what `toString` emits. */
  def symbol(op: CompareOp): String = op match
    case Lt => "<"
    case Gt => ">"
    case Le => "<="
    case Ge => ">="
    case Ne => "!="

  /** Looks an operator up by its symbol. */
  def fromSymbol(s: String): Option[CompareOp] = values.find(op => symbol(op) == s)


/** A comparison between two expressions: `lhs < rhs` and friends.
 *
 *  Reduces to a `core._Bool` when both sides are concrete and comparable, so comparisons feed
 *  the logic tier directly — `x < 2 and y > 3` needs no new machinery, because the
 *  connectives take untyped operands.
 *
 *  **Deliberately NOT `_ElementWise`, unlike [[_Equation]] and [[_EqualityCheck]].**  That
 *  marker lets algorithms distribute over both sides, which is why `2 * (x = 1)` reduces to
 *  `2x = 2` today.  For an inequality that rewrite is *invalid*: multiplying `x < 1` through
 *  by `-1` gives `-x < -1`, which is false exactly when the original is true, because the
 *  direction has to flip.  Distributing correctly would require knowing the sign of the
 *  multiplier, which in general is not available — so the marker is omitted, per the
 *  codebase rule that it may be present only when distribution is valid for *every*
 *  algorithm.  It is one word long and both neighbouring node types carry it, so this is
 *  written down rather than left to be re-derived.
 *
 *  **Not solvable.**  `_Solve.eval` requires an `_Equation`, so `solve(x < 2, x)` stays
 *  symbolic on its own; inequality solving would need interval-valued solutions, which the
 *  solver has no carrier for.
 *
 *  @param lhs left-hand side
 *  @param op  the relation
 *  @param rhs right-hand side
 */
case class _Comparison(lhs: _Expression, op: CompareOp, rhs: _Expression) extends _Expression:
  /** Fully parenthesised, which [[_Equation]] and [[_EqualityCheck]] are not — and the
   *  difference follows directly from *not* being `_ElementWise`.
   *
   *  Those two distribute, so a relation never survives inside a product: `2 * (x = 1)`
   *  becomes `(2 * x) = 1` before it is ever printed.  A comparison does survive, so an
   *  unparenthesised `x < 1` inside one renders as `(2.0 * x < 1.0)` — which re-parses as
   *  `(2x) < 1`, a *different expression*.  The parentheses are what keep the round-trip
   *  invariant, the same reason `logic._Connective` prints `(a and b)`.
   */
  override def toString: String = s"($lhs ${CompareOp.symbol(op)} $rhs)"
  override def children: List[_Expression] = List(lhs, rhs)
  override def rebuild(c: List[_Expression]): _Expression = _Comparison(c.head, op, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] = op match
    // `!=` is defined wherever `==` is -- including complex values and matrices, neither of
    // which is ordered -- so it delegates rather than going through `ordering`.
    case CompareOp.Ne =>
      compareSides(lhs, rhs, env)((l, r) => _Comparison(l, op, r)) match
        case Right(_Bool(b)) => Right(_Bool(!b))
        case other           => other
    // Each side is evaluated once and the results shared: an `integral` or `ode` side was
    // evaluated three times here, by the interval test, the ordering and the fallback (F_0108).
    case _ =>
      val (l, r) = (lhs.eval(env), rhs.eval(env))
      intervalOrder(op, l, r, env).getOrElse(
        orderingOf(l, r, env) match
          case Some(c) => Right(_Bool(holds(op, c)))
          case None    => Left(_Comparison(l.toExpression, op, r.toExpression)))

  /** An ordering with an interval operand (F_0008): `true` or `false` when the relation holds
   *  for every pair of points or for none, `unknown` when it depends on the points —
   *  `[1, 3] < [2, 4]` is neither.  `None` when no operand is an interval, or when the other
   *  has no real bounds (a complex number, a matrix): that relation is not a question about
   *  points, so it stays symbolic below rather than being answered `unknown` (F_0101). */
  private def intervalOrder(op: CompareOp, l: Either[_Expression, _Value], r: Either[_Expression, _Value],
                            env: Environment): Option[Either[_Expression, _Value]] =
    (l, r) match
      case (Right(a), Right(b)) if _Interval.comparable(a, b) =>
        val tolerance = 0.5 * math.pow(10, -env.precision)
        val verdict = op match
          case CompareOp.Lt => _Interval.below(a, b, strict = true, tolerance)
          case CompareOp.Le => _Interval.below(a, b, strict = false, tolerance)
          case CompareOp.Gt => _Interval.below(b, a, strict = true, tolerance)
          case CompareOp.Ge => _Interval.below(b, a, strict = false, tolerance)
          case CompareOp.Ne => _Interval.equal(a, b, tolerance).map(!_)
        Some(Right(verdict.fold(_Truth.Unknown: _Value)(_Bool(_))))
      case _ => None

  /** Whether `op` holds given a three-way comparison result. */
  private def holds(op: CompareOp, c: Int): Boolean = op match
    case CompareOp.Lt => c < 0
    case CompareOp.Gt => c > 0
    case CompareOp.Le => c <= 0
    case CompareOp.Ge => c >= 0
    case CompareOp.Ne => c != 0    // unreachable: handled above, kept for exhaustiveness


/** Three-way comparison of two expressions: `-1`, `0` or `1`, or `None` when they are not
 *  comparable.
 *
 *  `0` means "equal", and that has to mean **the same thing** it means to [[_Equation]], or
 *  `a == b` and `a < b` could both come out true.  So this mirrors `compareSides` exactly:
 *  exact when both operands are exact, tolerance-based otherwise.  The result is that for any
 *  comparable pair precisely one of `<`, `==`, `>` holds — the trichotomy the relation tier
 *  rests on, and the property the tests pin.
 *
 *  Complex values and matrices return `None`: neither carries a natural total order, so an
 *  ordering comparison over them stays symbolic rather than inventing one.
 */
private[equation] def ordering(lhs: _Expression, rhs: _Expression,
                               env: Environment): Option[Int] =
  orderingOf(lhs.eval(env), rhs.eval(env), env)

/** [[ordering]] of two sides already evaluated, so a caller that has them evaluates nothing twice. */
private[equation] def orderingOf(l: Either[_Expression, _Value], r: Either[_Expression, _Value],
                                 env: Environment): Option[Int] =
  (l, r) match
    // Exact operands compare exactly -- no tolerance is needed or wanted when neither side
    // carries representation error.  Must precede the `_Number` case, which widens over
    // `_Rational` and would otherwise read both as Doubles.
    case (Right(a: _Rational), Right(b: _Rational)) => Some(a.compare(b))
    case (Right(_Number(a)), Right(_Number(b))) =>
      if a.isNaN || b.isNaN then None
      else
        val tolerance = 0.5 * math.pow(10, -env.precision)
        if math.abs(a - b) <= tolerance then Some(0) else Some(if a < b then -1 else 1)
    case _ => None
