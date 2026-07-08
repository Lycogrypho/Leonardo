package it.grypho.scala.leonardo

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class IndefiniteIntegrationTest extends AnyFlatSpec:

  val x = _Variable("x")
  def env: Environment = new Environment()

  def evalAt(e: _Expression, value: Double): Double =
    val en = new Environment(10)
    en.assign("x", _Number(value))
    e.eval(en) match
      case Right(_Number(y)) => y
      case other             => fail(s"expected numeric result but got: $other")

  /**
   * Fundamental theorem check: d/dx (∫ f dx) must equal f. We differentiate the
   * computed antiderivative and compare to the original integrand at several
   * sample points, which validates the antiderivative regardless of its algebraic
   * form (constant of integration vanishes under differentiation).
   */
  def assertAntiderivative(f: _Expression, samples: Double*): Unit =
    val antideriv = integrate(f, x)
    assert(antideriv != _Integral(f, x), s"expected a closed form for ∫$f dx, got symbolic")
    val backDeriv = _Derivative(antideriv, x)
    for s <- samples do
      val expected = evalAt(f, s)
      val actual   = evalAt(backDeriv, s)
      assert(math.abs(actual - expected) < 1e-4,
        s"d/dx ∫($f) dx at x=$s: got $actual, expected $expected  (antiderivative = $antideriv)")

  // --- fundamental theorem: derive(integrate(f)) == f ---

  "∫ 1 dx" should "have derivative 1" in
  {
    assertAntiderivative(_Number(1), -2.0, 0.5, 3.0)
  }

  "∫ x dx" should "have derivative x" in
  {
    assertAntiderivative(x, -2.0, 0.5, 3.0)
  }

  "∫ x² dx" should "have derivative x²" in
  {
    assertAntiderivative(Power(x, _Number(2)), -2.0, 0.5, 3.0)
  }

  "∫ x³ dx" should "have derivative x³" in
  {
    assertAntiderivative(Power(x, _Number(3)), -2.0, 0.5, 3.0)
  }

  "∫ 3x² dx" should "have derivative 3x²" in
  {
    assertAntiderivative(Product(_Number(3), Power(x, _Number(2))), -2.0, 0.5, 3.0)
  }

  "∫ (x² + x) dx" should "have derivative x² + x" in
  {
    assertAntiderivative(Sum(Power(x, _Number(2)), x), -2.0, 0.5, 3.0)
  }

  "∫ exp(x) dx" should "have derivative exp(x)" in
  {
    assertAntiderivative(Exp(x), -1.0, 0.5, 2.0)
  }

  "∫ sin(x) dx" should "have derivative sin(x)" in
  {
    assertAntiderivative(Sin(x), -1.0, 0.5, 2.0)
  }

  "∫ cos(x) dx" should "have derivative cos(x)" in
  {
    assertAntiderivative(Cos(x), -1.0, 0.5, 2.0)
  }

  "∫ 1/x dx" should "have derivative 1/x" in
  {
    assertAntiderivative(Ratio(_Number(1), x), 0.5, 2.0, 3.0)
  }

  // --- chain rule over linear arguments u = a*x + b ---

  "∫ sin(2x) dx" should "have derivative sin(2x)" in
  {
    assertAntiderivative(Sin(Product(_Number(2), x)), -1.0, 0.5, 2.0)
  }

  "∫ cos(3x) dx" should "have derivative cos(3x)" in
  {
    assertAntiderivative(Cos(Product(_Number(3), x)), -1.0, 0.5, 2.0)
  }

  "∫ exp(2x) dx" should "have derivative exp(2x)" in
  {
    assertAntiderivative(Exp(Product(_Number(2), x)), -1.0, 0.5, 1.5)
  }

  "∫ (2x + 1)³ dx" should "have derivative (2x + 1)³" in
  {
    val u = Sum(Product(_Number(2), x), _Number(1))
    assertAntiderivative(Power(u, _Number(3)), -1.0, 0.5, 2.0)
  }

  // --- eval of the _Integral node reduces through the antiderivative ---

  "_Integral(x, x).eval with x bound" should "compute the antiderivative numerically" in
  {
    val en = new Environment()
    en.assign("x", _Number(4))
    _Integral(x, x).eval(en) match
      case Right(_Number(y)) => assert(math.abs(y - 8.0) < 1e-4)  // x²/2 = 16/2 = 8
      case other             => fail(s"expected 8.0 but got: $other")
  }

  "parse+eval of \"integral(x, x)\" with x = 4" should "equal 8.0" in
  {
    val result = Parser.parse("integral(x, x)")
    assert(result.successful, s"parse failed: $result")
    val en = new Environment()
    en.assign("x", _Number(4))
    result.get.eval(en) match
      case Right(_Number(y)) => assert(math.abs(y - 8.0) < 1e-4)
      case other             => fail(s"expected 8.0 but got: $other")
  }

  // --- unsupported forms stay symbolic ---

  "∫ x*sin(x) dx (needs integration by parts)" should "stay symbolic" in
  {
    assert(integrate(Product(x, Sin(x)), x) == _Integral(Product(x, Sin(x)), x))
  }

  "∫ sin(x²) dx (non-linear argument)" should "stay symbolic" in
  {
    assert(integrate(Sin(Power(x, _Number(2))), x) == _Integral(Sin(Power(x, _Number(2))), x))
  }

  "∫ tan(x) dx (no rule)" should "stay symbolic" in
  {
    assert(integrate(Tg(x), x) == _Integral(Tg(x), x))
  }

  // --- constant / independent-variable integrand ---

  "∫ y dx (y independent of x)" should "equal y*x" in
  {
    val y = _Variable("y")
    assert(integrate(y, x) == Product(y, x))
  }

  // --- Syntax sugar ---

  "e.integrate(v)" should "forward to the package-level integrate" in
  {
    val direct = integrate(Power(x, _Number(2)), x)
    import scalar.Syntax.integrate   // bring only the extension into local scope
    assert(Power(x, _Number(2)).integrate(x) == direct)
  }
