package it.grypho.scala.leonardo
package scalar

import core.*
import transform.inverseLaplaceOf
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.1 — the shared dense-polynomial helpers in `Polynomial.scala`.
 *
 *  These were two divergent private copies before, and the divergence was reachable: a
 *  denominator whose top term cancels leaves `collect` a *trailing zero*, which the two
 *  copies read at different degrees.  The tests below pin the trimming reading and the
 *  behaviour change it produces in the inverse Laplace transform.
 */
class PolynomialTest extends AnyFlatSpec:

  val s = _Variable("s")
  val t = _Variable("t")

  /** `v^3 - v^3 + v^2 + 1` — a cancelling top term, which `collect` does not trim. */
  private def cancellingTop(v: _Variable): _Expression =
    Sum(Sum(Power(v, _Number(3)), Product(_Number(-1), Power(v, _Number(3)))),
        Sum(Power(v, _Number(2)), _Number(1)))

  // --- collect trims, which is what makes the consolidation behaviour-preserving ---
  //
  // This is the load-bearing invariant.  The two former copies of `polyRoots` disagreed
  // about a vector with a trailing zero, but every caller is fed from `collect`, and
  // `collect` ends with `trimTrailingZeros` applied *after* `simplifyFully` -- so a
  // cancelling top term folds to a literal zero and is dropped before it can reach either.
  // If this ever stops holding, the two readings become distinguishable again and the
  // note in Polynomial.scala needs revisiting.

  "collect" should "trim a cancelling top term rather than leaving a trailing zero" in
  {
    collect(cancellingTop(s), s) match
      case None     => fail("collect should handle a polynomial in s")
      case Some(cs) =>
        assert(cs.size == 3, s"s^3 - s^3 + s^2 + 1 should collect to degree 2, got $cs")
        cs.last.eval(new Environment()) match
          case Right(_Number(d)) => assert(math.abs(d - 1.0) < 1e-12, s"top coefficient should be 1, got $d")
          case other             => fail(s"top coefficient should be numeric, got $other")
  }

  it should "never return a trailing zero for any of these shapes" in
  {
    val shapes = Vector(
      cancellingTop(s),
      Sum(Power(s, _Number(2)), _Number(1)),
      Product(Sum(s, _Number(1)), Sum(s, _Number(-1))),          // s^2 - 1
      Sum(Product(_Number(0), Power(s, _Number(4))), Power(s, _Number(2)))
    )
    for e <- shapes do
      collect(e, s) match
        case None     => fail(s"collect should handle $e")
        case Some(cs) =>
          val trailingIsZero = cs.last.eval(new Environment()) match
            case Right(_Number(d)) => math.abs(d) < 1e-12
            case _                 => false
          assert(!trailingIsZero || cs.size == 1, s"$e collected to $cs with a trailing zero")
  }

  // --- polyDegree: the true degree, not the vector length ---

  "polyDegree" should "report the true degree of a vector with trailing zeros" in
  {
    assert(polyDegree(Vector(1.0, 0.0, 1.0, 0.0)) == 2)
  }

  it should "report the length-derived degree when there is no trailing zero" in
  {
    assert(polyDegree(Vector(1.0, 0.0, 1.0)) == 2)
  }

  it should "report -1 for the zero polynomial" in
  {
    assert(polyDegree(Vector(0.0, 0.0)) == -1)
  }

  // --- polyRoots: solved at the true degree ---

  "polyRoots" should "solve a trailing-zero vector at its true degree" in
  {
    // [1, 0, 1, 0] is s^2 + 1 written in a length-4 vector: two roots, both non-real.
    polyRoots(Vector(1.0, 0.0, 1.0, 0.0)) match
      case None        => fail("the former transform copy returned None here; it should now solve")
      case Some(roots) =>
        assert(roots.size == 2, s"s^2 + 1 has two roots, got $roots")
        assert(roots.forall(_.isInstanceOf[_Complex]), s"both roots are non-real, got $roots")
  }

  it should "agree with the untrimmed vector for the same polynomial" in
  {
    val trimmed   = polyRoots(Vector(1.0, 0.0, 1.0))
    val untrimmed = polyRoots(Vector(1.0, 0.0, 1.0, 0.0))
    assert(trimmed.map(_.size) == untrimmed.map(_.size))
  }

  it should "return None below degree 1" in
  {
    assert(polyRoots(Vector(3.0)).isEmpty)
    assert(polyRoots(Vector(3.0, 0.0)).isEmpty)   // a constant in a length-2 vector
  }

  // --- derivCoeffs: the guard the transform copy lacked ---

  "derivCoeffs" should "return [0.0] for a constant instead of an empty vector" in
  {
    assert(derivCoeffs(Vector(5.0)) == Vector(0.0))
  }

  it should "not throw on an empty vector" in
  {
    assert(derivCoeffs(Vector.empty) == Vector(0.0))
  }

  it should "differentiate a dense vector" in
  {
    // 1 + 0*s + 1*s^2 + 0*s^3  ->  0 + 2s + 0*s^2
    assert(derivCoeffs(Vector(1.0, 0.0, 1.0, 0.0)) == Vector(0.0, 2.0, 0.0))
  }

  // --- the inverse transform is unchanged by the consolidation ---
  //
  // These are regression checks, not evidence of a behaviour change: because `collect`
  // trims, this input reached `invQuadratic` before the merge and still does.

  "invlaplace" should "invert a denominator whose top term cancelled" in
  {
    val got = inverseLaplaceOf(Ratio(_Number(1), cancellingTop(s)), s, t)
    assert(!got.isInstanceOf[transform._InverseLaplace],
           s"should not stay symbolic; got $got")
  }

  it should "give the cancelling form the same values as the plain one" in
  {
    val plain     = inverseLaplaceOf(Ratio(_Number(1), Sum(Power(s, _Number(2)), _Number(1))), s, t)
    val cancelled = inverseLaplaceOf(Ratio(_Number(1), cancellingTop(s)), s, t)
    for x <- Vector(0.0, 0.5, 1.0, 2.0) do
      val env = new Environment(variables = Map("t" -> _Number(x)))
      (plain.eval(env), cancelled.eval(env)) match
        case (Right(_Number(a)), Right(_Number(b))) =>
          assert(math.abs(a - b) < 1e-9, s"at t=$x: plain=$a cancelled=$b")
        case (pa, cb) => fail(s"at t=$x both should fold to numbers: $pa / $cb")
  }
