package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.33 — the one-sided z-transform and its inverse.
 *
 *  Results are compared **numerically at sample points** rather than by `toString`, for the
 *  reason `IntegralTableTest` documents: two algebraically equal expressions have many
 *  spellings, and pinning one makes the suite fail on a harmless simplification change.
 */
class ZTransformTest extends AnyFlatSpec:

  val n = _Variable("n")
  val z = _Variable("z")

  def parse(input: String): _Expression =
    val r = Parser.parse(input)
    assert(r.successful, s"parse failed for \"$input\": $r")
    r.get

  /** Evaluates `e` with `v` bound to `at`, failing the test if it stays symbolic. */
  def evalAt(e: _Expression, v: _Variable, at: Double): Double =
    e.eval(new Environment().withBinding(v.variable, _Number(at))) match
      case Right(_Number(d)) => d
      case other             => fail(s"expected a number for $e at ${v.variable}=$at but got: $other")

  /** Asserts two expressions agree numerically over several sample points of `v`. */
  def assertAgrees(actual: _Expression, expected: _Expression, v: _Variable,
                   points: Seq[Double], tol: Double = 1e-9): Unit =
    for p <- points do
      val a = evalAt(actual, v, p)
      val b = evalAt(expected, v, p)
      assert(math.abs(a - b) < tol * math.max(1.0, math.abs(b)),
        s"at ${v.variable}=$p: expected $b but got $a  (from $actual)")

  val zs: Seq[Double] = Seq(1.7, 2.3, 3.1, 5.0)

  // --- forward transform: the standard table ---

  "Z{c} for a constant sequence" should "be c*z/(z-1), not c/z" in
  {
    // The unilateral z-transform of the constant sequence c, c, c, ... The Laplace analogue
    // L{c} = c/s does NOT carry over; getting this wrong is the classic first slip.
    assertAgrees(zTransformOf(_Number(3), n, z), parse("3*z/(z-1)"), z, zs)
  }

  "Z{u[n]}" should "be z/(z-1)" in
  {
    assertAgrees(zTransformOf(_Heaviside(n), n, z), parse("z/(z-1)"), z, zs)
  }

  "Z{a^n}" should "be z/(z-a)" in
  {
    assertAgrees(zTransformOf(parse("2^n"), n, z), parse("z/(z-2)"), z, Seq(2.7, 3.4, 5.0))
  }

  "Z{exp(c*n)}" should "be z/(z - e^c)" in
  {
    assertAgrees(zTransformOf(parse("exp(2*n)"), n, z), parse("z/(z - exp(2))"), z, Seq(8.0, 9.5, 12.0))
  }

  "Z{n}" should "be z/(z-1)^2" in
  {
    assertAgrees(zTransformOf(n, n, z), parse("z/(z-1)^2"), z, zs)
  }

  "Z{n^2}" should "be z*(z+1)/(z-1)^3" in
  {
    // Falls out of the derivative-of-transform rule applied twice, not a table entry.
    assertAgrees(zTransformOf(parse("n^2"), n, z), parse("z*(z+1)/(z-1)^3"), z, zs)
  }

  "Z{n*a^n}" should "be a*z/(z-a)^2" in
  {
    assertAgrees(zTransformOf(parse("n*2^n"), n, z), parse("2*z/(z-2)^2"), z, Seq(2.7, 3.4, 5.0))
  }

  "Z{sin(w*n)}" should "be z*sin(w)/(z^2 - 2*z*cos(w) + 1)" in
  {
    assertAgrees(zTransformOf(parse("sin(0.5*n)"), n, z),
                 parse("z*sin(0.5)/(z^2 - 2*z*cos(0.5) + 1)"), z, zs)
  }

  "Z{cos(w*n)}" should "be z*(z - cos(w))/(z^2 - 2*z*cos(w) + 1)" in
  {
    assertAgrees(zTransformOf(parse("cos(0.5*n)"), n, z),
                 parse("z*(z - cos(0.5))/(z^2 - 2*z*cos(0.5) + 1)"), z, zs)
  }

  "linearity" should "split a sum and pull out a constant factor" in
  {
    assertAgrees(zTransformOf(parse("3*2^n + 5"), n, z), parse("3*z/(z-2) + 5*z/(z-1)"), z, Seq(2.7, 3.4, 5.0))
  }

  // --- forward transform: refusals ---

  "an untransformable shape" should "stay symbolic rather than guess" in
  {
    val e = _ZTransform(parse("fact(n)"), n, z)
    assert(e.eval(new Environment()) == Left(e))
  }

  "a sequence in the wrong variable" should "be treated as a constant sequence" in
  {
    // Free of n, so it is the constant sequence k -- k*z/(z-1), which is correct and not a
    // refusal. Pinned because the alternative reading (refuse) would be defensible but wrong.
    // `k` is bound here so the comparison has numbers to compare; the transform itself keeps
    // it symbolic, which is the point.
    val actual   = zTransformOf(_Variable("k"), n, z)
    val expected = parse("k*z/(z-1)")
    for p <- zs do
      val env = new Environment().withBinding("k", _Number(4)).withBinding("z", _Number(p))
      assert(actual.eval(env) == expected.eval(env), s"at z=$p")
  }

  // --- parser and round-trip ---

  "ztrans(...) and invztrans(...)" should "parse to the right nodes and round-trip" in
  {
    parse("ztrans(2^n, n, z)") match
      case _ZTransform(_, a, b) => assert(a.variable == "n" && b.variable == "z")
      case other                => fail(s"unexpected: $other")

    parse("invztrans(z/(z-2), z, n)") match
      case _InverseZTransform(_, a, b) => assert(a.variable == "z" && b.variable == "n")
      case other                       => fail(s"unexpected: $other")

    for src <- List("ztrans(2^n, n, z)", "invztrans((z / (z - 2.0)), z, n)") do
      val first = parse(src)
      assert(parse(first.toString) == first, s"round-trip failed for $src")
  }

  "bare 'ztrans' and 'invztrans'" should "be reserved words" in
  {
    assert(!Parser.parse("ztrans").successful)
    assert(!Parser.parse("invztrans").successful)
  }

  // --- inverse transform ---

  "invztrans(z/(z-a))" should "recover a^n" in
  {
    assertAgrees(inverseZTransformOf(parse("z/(z-2)"), z, n), parse("2^n"), n, Seq(0, 1, 2, 3, 5))
  }

  "invztrans(z/(z-1))" should "recover the unit sequence" in
  {
    assertAgrees(inverseZTransformOf(parse("z/(z-1)"), z, n), _Number(1), n, Seq(0, 1, 2, 4))
  }

  "invztrans with two distinct real poles" should "split by partial fractions" in
  {
    // z/((z-1)(z-2)) -> X(z)/z = 1/((z-1)(z-2)) = -1/(z-1) + 1/(z-2) -> x[n] = 2^n - 1
    assertAgrees(inverseZTransformOf(parse("z/((z-1)*(z-2))"), z, n), parse("2^n - 1"),
                 n, Seq(0, 1, 2, 3, 5))
  }

  "invztrans with a repeated pole" should "give the n*a^(n-1) family" in
  {
    // z/(z-2)^2 -> x[n] = n*2^(n-1)
    assertAgrees(inverseZTransformOf(parse("z/(z-2)^2"), z, n), parse("n*2^(n-1)"),
                 n, Seq(1, 2, 3, 4, 6))
  }

  "the forward and inverse transforms" should "round-trip" in
  {
    for src <- List("2^n", "n", "3*2^n") do
      val fwd = zTransformOf(parse(src), n, z)
      assert(!fwd.isInstanceOf[_ZTransform], s"forward transform failed for $src")
      val back = inverseZTransformOf(fwd, z, n)
      assert(!back.isInstanceOf[_InverseZTransform], s"inverse failed for $src (from $fwd)")
      assertAgrees(back, parse(src), n, Seq(0, 1, 2, 3, 5))
  }

  "a non-rational X(z)" should "stay symbolic" in
  {
    val e = _InverseZTransform(parse("exp(z)"), z, n)
    assert(e.eval(new Environment()) == Left(e))
  }
