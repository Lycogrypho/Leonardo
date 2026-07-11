package it.grypho.scala.leonardo
package equation

import core.*


// An explicit equality check — "a == b" — always reduces to _Bool when both sides
// are concrete, using the same tolerance as _Equation. Unlike _Equation it is:
//   - NOT solvable: _Solve.eval requires an _Equation, so solve(a == b, x) stays
//     symbolic (use "=" to build a solvable equation).
//   - Still _ElementWise: simplify / expand / derive distribute over both sides,
//     mirroring the _Equation treatment.
// toString is "lhs == rhs", which round-trips through the parser.
case class _EqualityCheck(lhs: _Expression, rhs: _Expression) extends _ElementWise:
  override def toString: String = s"$lhs == $rhs"
  override def children: List[_Expression] = List(lhs, rhs)
  override def rebuild(c: List[_Expression]): _Expression = _EqualityCheck(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    def tolerance: Double = 0.5 * math.pow(10, -env.precision)
    (lhs.eval(env), rhs.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) =>
        Right(_Bool(math.abs(a - b) <= tolerance))
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        val equal = x.rows == y.rows && x.cols == y.cols &&
          x.toVector.zip(y.toVector).forall((a, b) => math.abs(a - b) <= tolerance)
        Right(_Bool(equal))
      case (ra, rb) => Left(_EqualityCheck(ra.toExpression, rb.toExpression))
