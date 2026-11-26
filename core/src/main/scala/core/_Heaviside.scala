package it.grypho.scala.leonardo
package core

/** Heaviside unit step function: `step(t) = 1` when `t ≥ 0`, `0` otherwise.
 *
 *  Evaluates to a concrete [[_Number]] when its argument reduces to a `_Number`; otherwise
 *  stays symbolic so transform rules can pattern-match on it.  Not [[_ElementWise]]:
 *  derive / simplify / expand / integrate do not automatically distribute through `step`.
 *
 *  @param arg the argument expression
 */
case class _Heaviside(arg: _Expression) extends _Expression:
  override def toString: String = s"step($arg)"
  override def eval(env: Environment): Either[_Expression, _Value] =
    arg.eval(env) match
      // an exact argument's sign decided exactly, and the value built in its tier (F_0385: `step(1/2)` was `1.0`
      // in exact mode); placed before the widening `_Number` arm, which would read it as a `Double`
      // a reading of zero is zero (F_0384)
      case Right(v) if _Surd.isExact(v) && gridPointRead(arg, v, _Rational.Zero, env).exists(_.isZero) => Right(_Rational(1))
      case Right(v) if _Surd.isExact(v) =>
        _Surd.signum(v).fold[Either[_Expression, _Value]](Left(this))(s => Right(_Rational(if s >= 0 then 1 else 0)))
      case Right(_Number(d)) => Right(_Number(if d >= 0 then 1.0 else 0.0))
      case _                 => Left(this)
  override def children: List[_Expression] = List(arg)
  override def rebuild(c: List[_Expression]): _Expression = _Heaviside(c.head)
