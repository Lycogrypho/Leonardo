package it.grypho.scala.leonardo
package equation

import core.*


// Shared helper for _Equation.eval and _EqualityCheck.eval: both bodies are
// line-for-line identical except for the node type used to rebuild symbolic
// residuals. `wrap` receives the two (possibly reduced) operands and produces
// the appropriate node, keeping the shared logic in one place.
private[equation] def compareSides(
    lhs: _Expression, rhs: _Expression, env: Environment
)(wrap: (_Expression, _Expression) => _Expression): Either[_Expression, _Value] =
  val tolerance = 0.5 * math.pow(10, -env.precision)
  (lhs.eval(env), rhs.eval(env)) match
    case (Right(_Number(a)), Right(_Number(b))) =>
      Right(_Bool(math.abs(a - b) <= tolerance))
    case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
      val equal = x.rows == y.rows && x.cols == y.cols &&
        x.toVector.zip(y.toVector).forall((a, b) => math.abs(a - b) <= tolerance)
      Right(_Bool(equal))
    case (Right(av: _Value), Right(bv: _Value))
      if _Complex.parts(av).isDefined && _Complex.parts(bv).isDefined =>
      val close = for (ar, ai) <- _Complex.parts(av); (br, bi) <- _Complex.parts(bv)
        yield math.abs(ar - br) <= tolerance && math.abs(ai - bi) <= tolerance
      close.map(b => Right(_Bool(b))).getOrElse(Left(wrap(av, bv)))
    case (ra, rb) => Left(wrap(ra.toExpression, rb.toExpression))


// Equation domain: a relation between two expressions, following the matrix
// blueprint (own package importing core; nothing imports back).
//
// eval reduces both sides; when both fold to concrete values the equation itself
// reduces to a _Bool. Numeric equality is tolerance-based, tied to env.precision —
// exact Double comparison would make "sin(pi) = 0" false on floating-point noise;
// instead two numbers are equal when their difference vanishes at the configured
// number of decimals (|a − b| ≤ 0.5·10⁻ᵖ). Concrete matrices compare element-wise
// under the same tolerance. Anything else stays symbolic with the sides reduced.
//
// _ElementWise: an equation is a container of its two sides — differentiating,
// simplifying, expanding, or integrating an equation applies the algorithm to both
// sides (d/dx (lhs = rhs) is d(lhs)/dx = d(rhs)/dx), so the marker is sound here.
//
// toString is "lhs = rhs" (no outer parentheses): equations exist only at the top
// level of the grammar, and the round-trip invariant parse(toString(e)) == e holds.
case class _Equation(lhs: _Expression, rhs: _Expression) extends _ElementWise:
  override def toString: String = s"$lhs = $rhs"
  override def children: List[_Expression] = List(lhs, rhs)
  override def rebuild(c: List[_Expression]): _Expression = _Equation(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    compareSides(lhs, rhs, env)(_Equation(_, _))
