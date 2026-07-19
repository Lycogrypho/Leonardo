package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class IndefiniteIntegrationTest extends AnyFlatSpec:

  val x = _Variable("x")
  def env: Environment = new Environment()

  def evalAt(e: _Expression, value: Double): Double =
    val en = new Environment().withBinding("x", _Number(value))
    e.eval(en) match
      case Right(_Number(y)) => y
      case other             => fail(s"expected numeric result but got: $other")

  /**
   * Fundamental theorem check: d/dx (âˆ« f dx) must equal f. We differentiate the
   * computed antiderivative and compare to the original integrand at several
   * sample points, which validates the antiderivative regardless of its algebraic
   * form (constant of integration vanishes under differentiation).
   */
  def assertAntiderivative(f: _Expression, samples: Double*): Unit =
    val antideriv = integrate(f, x)
    assert(antideriv != _Integral(f, x), s"expected a closed form for âˆ«$f dx, got symbolic")
    val backDeriv = _Derivative(antideriv, x)
    for s <- samples do
      val expected = evalAt(f, s)
      val actual   = evalAt(backDeriv, s)
      assert(math.abs(actual - expected) < 1e-4,
        s"d/dx âˆ«($f) dx at x=$s: got $actual, expected $expected  (antiderivative = $antideriv)")

  // --- fundamental theorem: derive(integrate(f)) == f ---

  "âˆ« 1 dx" should "have derivative 1" in
  {
    assertAntiderivative(_Number(1), -2.0, 0.5, 3.0)
  }

  "âˆ« x dx" should "have derivative x" in
  {
    assertAntiderivative(x, -2.0, 0.5, 3.0)
  }

  "âˆ« xÂ² dx" should "have derivative xÂ²" in
  {
    assertAntiderivative(Power(x, _Number(2)), -2.0, 0.5, 3.0)
  }

  "âˆ« xÂ³ dx" should "have derivative xÂ³" in
  {
    assertAntiderivative(Power(x, _Number(3)), -2.0, 0.5, 3.0)
  }

  "âˆ« 3xÂ² dx" should "have derivative 3xÂ²" in
  {
    assertAntiderivative(Product(_Number(3), Power(x, _Number(2))), -2.0, 0.5, 3.0)
  }

  "âˆ« (xÂ² + x) dx" should "have derivative xÂ² + x" in
  {
    assertAntiderivative(Sum(Power(x, _Number(2)), x), -2.0, 0.5, 3.0)
  }

  "âˆ« exp(x) dx" should "have derivative exp(x)" in
  {
    assertAntiderivative(Exp(x), -1.0, 0.5, 2.0)
  }

  "âˆ« sin(x) dx" should "have derivative sin(x)" in
  {
    assertAntiderivative(Sin(x), -1.0, 0.5, 2.0)
  }

  "âˆ« cos(x) dx" should "have derivative cos(x)" in
  {
    assertAntiderivative(Cos(x), -1.0, 0.5, 2.0)
  }

  "âˆ« 1/x dx" should "have derivative 1/x" in
  {
    assertAntiderivative(Ratio(_Number(1), x), 0.5, 2.0, 3.0)
  }

  // --- chain rule over linear arguments u = a*x + b ---

  "âˆ« sin(2x) dx" should "have derivative sin(2x)" in
  {
    assertAntiderivative(Sin(Product(_Number(2), x)), -1.0, 0.5, 2.0)
  }

  "âˆ« cos(3x) dx" should "have derivative cos(3x)" in
  {
    assertAntiderivative(Cos(Product(_Number(3), x)), -1.0, 0.5, 2.0)
  }

  "âˆ« exp(2x) dx" should "have derivative exp(2x)" in
  {
    assertAntiderivative(Exp(Product(_Number(2), x)), -1.0, 0.5, 1.5)
  }

  "âˆ« (2x + 1)Â³ dx" should "have derivative (2x + 1)Â³" in
  {
    val u = Sum(Product(_Number(2), x), _Number(1))
    assertAntiderivative(Power(u, _Number(3)), -1.0, 0.5, 2.0)
  }

  // --- eval of the _Integral node reduces through the antiderivative ---

  "_Integral(x, x).eval with x bound" should "compute the antiderivative numerically" in
  {
    val en = new Environment().withBinding("x", _Number(4))
    _Integral(x, x).eval(en) match
      case Right(_Number(y)) => assert(math.abs(y - 8.0) < 1e-4)  // xÂ²/2 = 16/2 = 8
      case other             => fail(s"expected 8.0 but got: $other")
  }

  "parse+eval of \"integral(x, x)\" with x = 4" should "equal 8.0" in
  {
    val result = Parser.parse("integral(x, x)")
    assert(result.successful, s"parse failed: $result")
    val en = new Environment().withBinding("x", _Number(4))
    result.get.eval(en) match
      case Right(_Number(y)) => assert(math.abs(y - 8.0) < 1e-4)
      case other             => fail(s"expected 8.0 but got: $other")
  }

  // --- inverse-trig primitives (issue #24) ---

  "âˆ« 1/(1 + xÂ²) dx" should "have derivative 1/(1+xÂ²)" in
  {
    assertAntiderivative(Ratio(_Number(1), Sum(_Number(1), Power(x, _Number(2)))), 0.5, 1.0, 2.0)
  }

  "âˆ« 1/(xÂ² + 1) dx (commuted denominator)" should "have derivative 1/(xÂ²+1)" in
  {
    assertAntiderivative(Ratio(_Number(1), Sum(Power(x, _Number(2)), _Number(1))), 0.5, 1.0, 2.0)
  }

  "âˆ« 1/(1 + (2x)Â²) dx" should "have derivative 1/(1+(2x)Â²)" in
  {
    val u = Product(_Number(2), x)
    assertAntiderivative(Ratio(_Number(1), Sum(_Number(1), Power(u, _Number(2)))), 0.5, 1.0, 2.0)
  }

  "âˆ« 1/âˆš(1 - xÂ²) dx" should "have derivative 1/âˆš(1-xÂ²)" in
  {
    // domain: |x| < 1
    assertAntiderivative(
      Ratio(_Number(1), Power(Sum(_Number(1), Product(_Number(-1), Power(x, _Number(2)))), _Number(0.5))),
      0.1, 0.5, 0.9
    )
  }

  // --- unsupported forms stay symbolic ---

  "âˆ« x*sin(x) dx (needs integration by parts)" should "stay symbolic" in
  {
    assert(integrate(Product(x, Sin(x)), x) == _Integral(Product(x, Sin(x)), x))
  }

  "âˆ« sin(xÂ²) dx (non-linear argument)" should "stay symbolic" in
  {
    assert(integrate(Sin(Power(x, _Number(2))), x) == _Integral(Sin(Power(x, _Number(2))), x))
  }

  "âˆ« tan(x) dx (no rule)" should "stay symbolic" in
  {
    assert(integrate(Tg(x), x) == _Integral(Tg(x), x))
  }

  // --- constant / independent-variable integrand ---

  "âˆ« y dx (y independent of x)" should "equal y*x" in
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
