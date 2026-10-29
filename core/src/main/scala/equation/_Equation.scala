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
 *  Two numbers are equal when `|a - b| <= 0.5 * 10^(-env.precision)`; two exact values when they
 *  agree to the working precision ([[agreeToWorkingPrecision]], F_0177).  Matrices compare
 *  element-wise under the same rules.  Complex values are compared component-wise via
 *  `_Complex.parts`.
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
    // Exact operands compare to the WORKING precision, not the display one (F_0177): in the
    // exact tier `1/3 == 0.33333` must stay false, but an irrational is a rational carried to
    // the working precision, so comparing exactly made `(2^(1/2))^2 == 2` false by 1.2e-41.
    //
    // `orderingOf` reads equality the same way, so two rationals a hair apart never satisfy
    // BOTH `a == b` and `a > b`.  Outside exact mode no `_Rational` is ever constructed, so
    // this arm is unreachable and the `Double` path is byte-identical.
    case (Right(a: _Rational), Right(b: _Rational)) => Right(_Bool(agreeToWorkingPrecision(a, b, env)))
    case (Right(a: _Value), Right(b: _Value)) if _Surd.involves(a, b) =>
      exactAgree(a, b, env).map(e => _Bool(e)).toRight(wrap(a, b))
    case (Right(_Number(a)), Right(_Number(b))) =>
      Right(_Bool(math.abs(a - b) <= tolerance))
    case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
      val equal = x.rows == y.rows && x.cols == y.cols &&
        x.toVector.zip(y.toVector).forall((a, b) => math.abs(a - b) <= tolerance)
      Right(_Bool(equal))
    // F_0008: an interval side is equal for every pair of points, for none, or unknown.  Only
    // when both sides have real bounds (F_0101): beside a complex number, a matrix or a truth
    // value the arms below leave the relation symbolic, which is what it is.
    case (Right(av: _Value), Right(bv: _Value)) if _Interval.comparable(av, bv) =>
      Right(_Interval.equal(av, bv, tolerance).fold(_Truth.Unknown: _Value)(_Bool(_)))
    // Both sides are concrete: for-comprehension extracts (re,im) via _Complex.parts;
    // None on non-numeric values -> close = None -> stays symbolic. No double-call of parts.
    case (Right(av: _Value), Right(bv: _Value)) =>
      val close = for (ar, ai) <- _Complex.parts(av); (br, bi) <- _Complex.parts(bv)
        yield math.abs(ar - br) <= tolerance && math.abs(ai - bi) <= tolerance
      close.map(b => _Bool(b)).toRight(wrap(av, bv))
    // F_0164: two matrices of values when either is not dense -- an exact matrix stays the
    // symbolic carrier, so the dense arm above never sees it and `[[1/2]] == [[1/2]]` echoed.
    // Computed once (F_0212): a guard and a body each called it, comparing every cell twice.
    case (ra, rb) =>
      matrixEqual(ra, rb, tolerance, env).map(b => _Bool(b)).toRight(wrap(ra.toExpression, rb.toExpression))

/** Whether two exact values agree to the working precision `wp` (F_0177):
 *  `|a - b| <= 10^-wp * max(|a|, |b|, 1)`.
 *
 *  Every irrational the exact tier meets is carried to `wp` digits, so a difference below that is
 *  approximation error, not a difference between the values; the floor of 1 makes the test
 *  absolute near zero.  Pure rational arithmetic loses only the ability to tell apart values
 *  closer than `10^-wp` relative.  Decided in integers, multiplied through by both (positive)
 *  denominators, so no rounding enters the decision.
 *
 *  @param a   one exact value
 *  @param b   the other
 *  @param env supplies the working precision
 *  @return whether the two are equal at the working precision
 */
private[leonardo] def agreeToWorkingPrecision(a: _Rational, b: _Rational, env: Environment): Boolean =
  val difference = (a.num * b.den - b.num * a.den).abs * BigInt(10).pow(env.workingPrecision)
  val scale      = (a.num.abs * b.den).max(b.num.abs * a.den).max(a.den * b.den)
  difference <= scale

/** [[agreeToWorkingPrecision]] for two exact values of which at least one is a surd (F_0231).
 *
 *  The F_0177 rule unchanged: a rational in the exact tier may be an irrational carried to the
 *  working precision (a cube root squared), so a surd compared with one must agree to that
 *  precision, not exactly -- or `(2^(1/4))^2 == 2^(1/2)` would turn false.  Both sides are read
 *  at `wp + GuardDigits`, so the reading itself cannot decide the answer.
 *
 *  @return whether they agree, or `None` when either is not exact
 */
private[leonardo] def exactAgree(a: _Value, b: _Value, env: Environment): Option[Boolean] =
  val digits = env.workingPrecision + GuardDigits
  for qa <- _Surd.exactOf(a, digits); qb <- _Surd.exactOf(b, digits)
  yield agreeToWorkingPrecision(qa, qb, env)

/** The order of two exact values, at least one a surd: `0` when they agree to the working
 *  precision ([[exactAgree]]), else the sign of their difference, decided exactly over one
 *  radical and through the precise readings otherwise (which differ, so they cannot tie).
 *
 *  @return `-1`, `0` or `1`, or `None` when either is not exact
 */
private[leonardo] def exactOrder(a: _Value, b: _Value, env: Environment): Option[Int] =
  exactAgree(a, b, env).flatMap {
    case true  => Some(0)
    case false =>
      _Surd.subtract(a, b).flatMap(_Surd.signum).orElse {
        val digits = env.workingPrecision + GuardDigits
        for qa <- _Surd.exactOf(a, digits); qb <- _Surd.exactOf(b, digits) yield qa.compare(qb).sign
      }
  }

/** Whether the relation between `lhs` and `rhs` treated two exact values as equal although they
 *  differ (F_0177): both sides exact — two rationals, or two matrices of them of one shape — every
 *  pair agreeing to the working precision, and at least one pair not exactly equal.  What the REPL
 *  reads to say that the working precision decided the answer.
 *
 *  @param lhs the left side
 *  @param rhs the right side
 *  @param env the evaluation environment
 *  @return whether the working precision, rather than exact equality, made the sides equal
 */
private[leonardo] def equalOnlyToWorkingPrecision(lhs: _Expression, rhs: _Expression, env: Environment): Boolean =
  val (ra, rb) = (lhs.eval(env), rhs.eval(env))
  val pairs: Option[Vector[(_Value, _Value)]] = (ra, rb) match
    case (Right(a: _Rational), Right(b: _Rational)) => Some(Vector((a, b)))
    case _ =>
      for
        (r1, c1, xs) <- _MatrixValue.cellsOf(ra)
        (r2, c2, ys) <- _MatrixValue.cellsOf(rb)
        if r1 == r2 && c1 == c2
      yield xs.zip(ys)
  pairs.exists { ps =>
    // A surd counts as exact (F_0231): it is equal to another value exactly by `==`, and to a
    // working-precision rational only by the tolerance, which is what the note reports.
    val exact = ps.filter((x, y) => _Surd.isExact(x) && _Surd.isExact(y))
    exact.size == ps.size && exact.forall((x, y) => exactAgree(x, y, env).contains(true)) &&
      exact.exists((x, y) => x != y)
  }

/** Whether two evaluated matrices of values are equal, cell by cell (F_0164): two rationals at the
 *  working precision (F_0177), anything else within `tolerance`, and `false` for different shapes;
 *  `None` when either is not a matrix of values or a pair of cells cannot be compared. */
private def matrixEqual(ra: Either[_Expression, _Value], rb: Either[_Expression, _Value],
                        tolerance: Double, env: Environment): Option[Boolean] =
  def cellEqual(a: _Value, b: _Value): Option[Boolean] = (a, b) match
    case (x: _Rational, y: _Rational) => Some(agreeToWorkingPrecision(x, y, env))
    case (x, y) if _Surd.involves(x, y) => exactAgree(x, y, env)
    case _ =>
      for (ar, ai) <- _Complex.parts(a); (br, bi) <- _Complex.parts(b)
      yield math.abs(ar - br) <= tolerance && math.abs(ai - bi) <= tolerance
  for
    (r1, c1, xs) <- _MatrixValue.cellsOf(ra)
    (r2, c2, ys) <- _MatrixValue.cellsOf(rb)
    equal        <- if r1 != r2 || c1 != c2 then Some(false)
                    else sequence(xs.lazyZip(ys).map(cellEqual)).map(_.forall(identity))
  yield equal


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
