package it.grypho.scala.leonardo
package equation

import core.*


/** Shared comparison helper for [[_Equation]] and [[_EqualityCheck]].
 *
 *  Both node types apply identical tolerance-based equality logic; they differ only in
 *  the node used to rebuild the symbolic residual when the sides are not yet concrete.
 *  `wrap` receives the two (possibly already-reduced) operands and produces the
 *  appropriate node, keeping the shared logic in one place.
 *
 *  Two numbers are equal when `|a - b| <= 0.5 * 10^(-env.precision)`.  Matrices
 *  compare element-wise under the same tolerance.  Complex values are compared
 *  component-wise via `_Complex.parts`.
 *
 *  @param lhs  left-hand side expression
 *  @param rhs  right-hand side expression
 *  @param env  evaluation environment (supplies precision and variable bindings)
 *  @param wrap factory for the residual node when the result stays symbolic
 *  @return `Right(_Bool(true/false))` when both sides are concrete, `Left(wrap(...))` otherwise
 */
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
    // Both sides are concrete: for-comprehension extracts (re,im) via _Complex.parts;
    // None on non-numeric values -> close = None -> stays symbolic. No double-call of parts.
    case (Right(av: _Value), Right(bv: _Value)) =>
      val close = for (ar, ai) <- _Complex.parts(av); (br, bi) <- _Complex.parts(bv)
        yield math.abs(ar - br) <= tolerance && math.abs(ai - bi) <= tolerance
      close.map(b => Right(_Bool(b))).getOrElse(Left(wrap(av, bv)))
    case (ra, rb) => Left(wrap(ra.toExpression, rb.toExpression))


/** A relation between two expressions: `lhs = rhs`.
 *
 *  `eval` reduces both sides and compares them when both are concrete.  Numeric
 *  equality is tolerance-based, tied to `env.precision` -- exact `Double` comparison
 *  would make `sin(pi) = 0` false on floating-point noise; instead two numbers are
 *  equal when `|a - b| <= 0.5 * 10^(-p)`.  Concrete matrices compare element-wise
 *  under the same tolerance.  Anything else stays symbolic with the sides reduced.
 *
 *  Marked `_ElementWise`: an equation is a container of its two sides, so
 *  `derive`/`simplify`/`expand`/`integrate` apply the algorithm to both sides
 *  (e.g. `d/dx (lhs = rhs)` is `d(lhs)/dx = d(rhs)/dx`).
 *
 *  `toString` is `"lhs = rhs"` (no outer parentheses): equations exist only at the
 *  top level of the grammar, and the round-trip invariant `parse(toString(e)) == e` holds.
 *
 *  @param lhs left-hand side expression
 *  @param rhs right-hand side expression
 */
case class _Equation(lhs: _Expression, rhs: _Expression) extends _ElementWise:
  override def toString: String = s"$lhs = $rhs"
  override def children: List[_Expression] = List(lhs, rhs)
  override def rebuild(c: List[_Expression]): _Expression = _Equation(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    compareSides(lhs, rhs, env)(_Equation(_, _))
