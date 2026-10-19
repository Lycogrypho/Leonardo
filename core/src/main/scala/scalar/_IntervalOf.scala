package it.grypho.scala.leonardo
package scalar

import core.*


/** The interval literal `[lo, hi]` (F_0008): evaluates to a [[core._Interval]] once both
 *  endpoints are real numbers with `lo ≤ hi`, and stays as written otherwise — the
 *  `_DistributionOf` split, so `[0, a]` keeps its meaning while `a` is free.
 *
 *  A single pair of brackets was free in the grammar: a matrix row is always written inside
 *  a second pair, `[[a, b]]`.  `toString` is the literal, so it round-trips.
 *
 *  @param lo the lower endpoint
 *  @param hi the upper endpoint
 */
case class _IntervalOf(lo: _Expression, hi: _Expression) extends _Expression:
  override def toString: String = s"[$lo, $hi]"
  override def children: List[_Expression] = List(lo, hi)
  override def rebuild(c: List[_Expression]): _Expression = _IntervalOf(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (lo.eval(env), hi.eval(env)) match
      case (Right(a), Right(b)) => _Interval.of(a, b).toRight(_IntervalOf(a, b))
      case (ra, rb)             => Left(_IntervalOf(ra.toExpression, rb.toExpression))
