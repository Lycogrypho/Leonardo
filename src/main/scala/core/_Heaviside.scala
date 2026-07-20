package it.grypho.scala.leonardo
package core

// The Heaviside unit step function: step(t) = 0 for t < 0, 1 for t >= 0.
// Evaluates to a concrete _Number when its argument reduces to a _Number;
// otherwise stays symbolic so transform rules can pattern-match on it.
// Not _ElementWise: derive/simplify/expand/integrate do not distribute through step.
case class _Heaviside(arg: _Expression) extends _Expression:
  override def toString: String = s"step($arg)"
  override def eval(env: Environment): Either[_Expression, _Value] =
    arg.eval(env) match
      case Right(_Number(d)) => Right(_Number(if d >= 0 then 1.0 else 0.0))
      case _                 => Left(this)
  override def children: List[_Expression] = List(arg)
  override def rebuild(c: List[_Expression]): _Expression = _Heaviside(c.head)
