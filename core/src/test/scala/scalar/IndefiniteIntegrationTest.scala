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
    val en = new Environment().withBinding("x", _Number(4))
    _Integral(x, x).eval(en) match
      case Right(_Number(y)) => assert(math.abs(y - 8.0) < 1e-4)  // x²/2 = 16/2 = 8
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

  "∫ 1/(1 + x²) dx" should "have derivative 1/(1+x²)" in
  {
    assertAntiderivative(Ratio(_Number(1), Sum(_Number(1), Power(x, _Number(2)))), 0.5, 1.0, 2.0)
  }

  "∫ 1/(x² + 1) dx (commuted denominator)" should "have derivative 1/(x²+1)" in
  {
    assertAntiderivative(Ratio(_Number(1), Sum(Power(x, _Number(2)), _Number(1))), 0.5, 1.0, 2.0)
  }

  "∫ 1/(1 + (2x)²) dx" should "have derivative 1/(1+(2x)²)" in
  {
    val u = Product(_Number(2), x)
    assertAntiderivative(Ratio(_Number(1), Sum(_Number(1), Power(u, _Number(2)))), 0.5, 1.0, 2.0)
  }

  "∫ 1/√(1 - x²) dx" should "have derivative 1/√(1-x²)" in
  {
    // domain: |x| < 1
    assertAntiderivative(
      Ratio(_Number(1), Power(Sum(_Number(1), Product(_Number(-1), Power(x, _Number(2)))), _Number(0.5))),
      0.1, 0.5, 0.9
    )
  }

  // --- integration by parts (LIATE): polynomial × {exp, sin, cos} and standalone log ---

  "∫ x*sin(x) dx" should "have derivative x*sin(x)" in
  {
    assertAntiderivative(Product(x, Sin(x)), -1.0, 0.5, 2.0)
  }

  "∫ x*cos(x) dx" should "have derivative x*cos(x)" in
  {
    assertAntiderivative(Product(x, Cos(x)), -1.0, 0.5, 2.0)
  }

  "∫ x*exp(x) dx" should "have derivative x*exp(x)" in
  {
    assertAntiderivative(Product(x, Exp(x)), -1.0, 0.5, 1.5)
  }

  "∫ x²*exp(x) dx (two parts levels)" should "have derivative x²*exp(x)" in
  {
    assertAntiderivative(Product(Power(x, _Number(2)), Exp(x)), -1.0, 0.5, 1.5)
  }

  "∫ x²*sin(x) dx (two parts levels)" should "have derivative x²*sin(x)" in
  {
    assertAntiderivative(Product(Power(x, _Number(2)), Sin(x)), -1.0, 0.5, 2.0)
  }

  "∫ (2x+1)*exp(x) dx (polynomial factor)" should "have derivative (2x+1)*exp(x)" in
  {
    val poly = Sum(Product(_Number(2), x), _Number(1))
    assertAntiderivative(Product(poly, Exp(x)), -1.0, 0.5, 1.5)
  }

  "∫ exp(x)*x dx (factor order swapped)" should "have derivative exp(x)*x" in
  {
    assertAntiderivative(Product(Exp(x), x), -1.0, 0.5, 1.5)
  }

  "∫ ln(x) dx (standalone log, dv = 1)" should "have derivative ln(x)" in
  {
    assertAntiderivative(Ln(x), 0.5, 1.0, 2.0)
  }

  // --- trigonometric power reduction formulas (4.B) ---

  "∫ sin²(x) dx" should "have derivative sin²(x)" in
  {
    assertAntiderivative(Power(Sin(x), _Number(2)), -1.0, 0.5, 2.0)
  }

  "∫ sin³(x) dx (odd power, bottoms at n=1)" should "have derivative sin³(x)" in
  {
    assertAntiderivative(Power(Sin(x), _Number(3)), -1.0, 0.5, 2.0)
  }

  "∫ cos²(x) dx" should "have derivative cos²(x)" in
  {
    assertAntiderivative(Power(Cos(x), _Number(2)), -1.0, 0.5, 2.0)
  }

  "∫ cos³(x) dx (odd power, bottoms at n=1)" should "have derivative cos³(x)" in
  {
    assertAntiderivative(Power(Cos(x), _Number(3)), -1.0, 0.5, 2.0)
  }

  "∫ sin⁴(x) dx (even power, bottoms at n=0)" should "have derivative sin⁴(x)" in
  {
    assertAntiderivative(Power(Sin(x), _Number(4)), -1.0, 0.5, 2.0)
  }

  "∫ cos⁵(x) dx (deeper recursion)" should "have derivative cos⁵(x)" in
  {
    assertAntiderivative(Power(Cos(x), _Number(5)), -1.0, 0.5, 2.0)
  }

  "∫ sin²(2x) dx (linear argument, slope 2)" should "have derivative sin²(2x)" in
  {
    assertAntiderivative(Power(Sin(Product(_Number(2), x)), _Number(2)), -1.0, 0.5, 2.0)
  }

  "∫ cos²(3x + 1) dx (linear argument, slope 3)" should "have derivative cos²(3x+1)" in
  {
    val u = Sum(Product(_Number(3), x), _Number(1))
    assertAntiderivative(Power(Cos(u), _Number(2)), -1.0, 0.5, 2.0)
  }

  "∫ 3*sin²(x) dx (constant multiple peeled before reduction)" should "have derivative 3*sin²(x)" in
  {
    assertAntiderivative(Product(_Number(3), Power(Sin(x), _Number(2))), -1.0, 0.5, 2.0)
  }

  "∫ sin²¹(x) dx (exponent above MaxReductionPower)" should "stay symbolic" in
  {
    val e = Power(Sin(x), _Number(21))
    assert(integrate(e, x) == _Integral(e, x))
  }

  "∫ sin²(x²) dx (non-linear argument)" should "stay symbolic" in
  {
    val e = Power(Sin(Power(x, _Number(2))), _Number(2))
    assert(integrate(e, x) == _Integral(e, x))
  }

  // --- rational functions via partial fractions (4.C) ---

  "∫ 1/(1 + x²) dx (quadratic denominator, complex roots)" should "have derivative 1/(1+x²)" in
  {
    // reaches the general rational tier when written x/x-independent numerator form;
    // here the dedicated atan rule already covers it, but the rational tier agrees.
    assertAntiderivative(Ratio(_Number(1), Sum(_Number(1), Power(x, _Number(2)))), 0.5, 1.0, 2.0)
  }

  "∫ x/(1 + x²) dx (log-substitution shape, half-log result)" should "have derivative x/(1+x²)" in
  {
    assertAntiderivative(Ratio(x, Sum(_Number(1), Power(x, _Number(2)))), -1.0, 0.5, 2.0)
  }

  "∫ 1/(x² + x + 1) dx (irreducible quadratic, pure arctan)" should "have derivative 1/(x²+x+1)" in
  {
    val den = Sum(Sum(Power(x, _Number(2)), x), _Number(1))
    assertAntiderivative(Ratio(_Number(1), den), -1.0, 0.5, 2.0)
  }

  "∫ 1/(x² - 1) dx (distinct real roots)" should "have derivative 1/(x²-1)" in
  {
    // domain x > 1 so both ln(x-1), ln(x+1) are real
    assertAntiderivative(Ratio(_Number(1), Sum(Power(x, _Number(2)), _Number(-1))), 1.5, 2.0, 3.0)
  }

  "∫ (2x + 3)/(x² + 3x + 2) dx (numerator = D', becomes ln D)" should "have derivative (2x+3)/(x²+3x+2)" in
  {
    val den = Sum(Sum(Power(x, _Number(2)), Product(_Number(3), x)), _Number(2))
    val num = Sum(Product(_Number(2), x), _Number(3))
    assertAntiderivative(Ratio(num, den), 0.5, 1.0, 2.0)   // x > 0 keeps roots -1, -2 out of range
  }

  "∫ 1/(x - 1)² dx (repeated real root at deg 2)" should "have derivative 1/(x-1)²" in
  {
    val den = Power(Sum(x, _Number(-1)), _Number(2))
    assertAntiderivative(Ratio(_Number(1), den), 1.5, 2.0, 3.0)
  }

  "∫ x²/(x² + 1) dx (improper: long division -> 1 - 1/(x²+1))" should "have derivative x²/(x²+1)" in
  {
    assertAntiderivative(Ratio(Power(x, _Number(2)), Sum(Power(x, _Number(2)), _Number(1))), -1.0, 0.5, 2.0)
  }

  "∫ x³/(x² + 1) dx (improper: quotient x, remainder -x)" should "have derivative x³/(x²+1)" in
  {
    assertAntiderivative(Ratio(Power(x, _Number(3)), Sum(Power(x, _Number(2)), _Number(1))), -1.0, 0.5, 2.0)
  }

  "∫ 1/((x-1)(x-2)(x-3)) dx (deg 3, distinct real roots via residues)" should "have derivative it" in
  {
    // denominator x^3 - 6x^2 + 11x - 6; sample x > 3 so all logs are real
    val den = Sum(Sum(Sum(Power(x, _Number(3)), Product(_Number(-6), Power(x, _Number(2)))),
                      Product(_Number(11), x)), _Number(-6))
    assertAntiderivative(Ratio(_Number(1), den), 3.5, 4.0, 5.0)
  }

  "∫ 1/((x²+1)(x-1)) dx (deg 3 with complex roots)" should "now close via full partial fractions" in
  {
    // This test previously asserted "stays symbolic": the old residue path declined any
    // complex root at deg >= 3.  Issue 3.12 lifts that to the full real decomposition
    // (irreducible quadratics in log+arctan form), so the integral now closes.  Sample x > 1
    // so ln(x-1) stays real.
    val den = Product(Sum(Power(x, _Number(2)), _Number(1)), Sum(x, _Number(-1)))
    assertAntiderivative(Ratio(_Number(1), den), 1.5, 2.0, 3.0)
  }

  // --- formerly deferred parts cases, now unblocked by the rational tier (4.C) ---

  "∫ arctan(x) dx (parts + ∫x/(1+x²) via rational tier)" should "have derivative arctan(x)" in
  {
    assertAntiderivative(Atan(x), -1.0, 0.5, 2.0)
  }

  "∫ x*ln(x) dx (parts + x²/(2x) long division)" should "have derivative x*ln(x)" in
  {
    assertAntiderivative(Product(x, Ln(x)), 0.5, 1.0, 2.0)
  }

  // --- unsupported forms stay symbolic ---

  "∫ sin(x²) dx (non-linear argument)" should "stay symbolic" in
  {
    assert(integrate(Sin(Power(x, _Number(2))), x) == _Integral(Sin(Power(x, _Number(2))), x))
  }

  "tan(x) dx" should "now integrate via the 6.21 rule table" in
  {
    // This test previously asserted "stays symbolic (no rule)".  6.21's data-driven table
    // supplies the rule, so the capability it documented the absence of now exists.  The
    // compiled arms are unchanged -- only integrands that used to give up can newly match.
    assert(integrate(Tg(x), x) == simplifyFully(Product(_Number(-1), Ln(Cos(x)))))
  }

  // --- constant / independent-variable integrand ---

  "∫ y dx (y independent of x)" should "equal y*x" in
  {
    val y = _Variable("y")
    assert(integrate(y, x) == Product(y, x))
  }

  // --- Syntax sugar ---

  // --- issue 4.3: ∫ step(u) dv = u·step(u) / a ---

  "integrate(step(x), x)" should "equal x*step(x)" in
  {
    val result = integrate(_Heaviside(x), x)
    assert(result == Ratio(Product(x, _Heaviside(x)), _Number(1.0)),
      s"got: $result")
  }

  "integrate(step(2*x), x)" should "equal (2x)*step(2x) / 2" in
  {
    val twoX = Product(_Number(2), x)
    val result = integrate(_Heaviside(twoX), x)
    assert(result == Ratio(Product(twoX, _Heaviside(twoX)), _Number(2.0)),
      s"got: $result")
  }

  "integrate(step(x+1), x)" should "equal (x+1)*step(x+1) / 1" in
  {
    val xp1 = Sum(x, _Number(1))
    val result = integrate(_Heaviside(xp1), x)
    assert(result == Ratio(Product(xp1, _Heaviside(xp1)), _Number(1.0)),
      s"got: $result")
  }

  "integrate(step(x^2), x)" should "stay symbolic (non-linear argument)" in
  {
    val e = _Heaviside(Power(x, _Number(2)))
    assert(integrate(e, x) == _Integral(e, x))
  }

  "integrate(step(x), x) antiderivative" should "evaluate to x at positive x and 0 at negative x" in
  {
    val antideriv = integrate(_Heaviside(x), x)  // x * step(x) / 1
    def evalAt(d: Double): Double =
      antideriv.eval(new Environment().withBinding("x", _Number(d))) match
        case Right(_Number(v)) => v
        case other => fail(s"expected numeric at x=$d, got $other")
    assert(math.abs(evalAt(3.0)  - 3.0) < 1e-9)   // 3  * step(3)  = 3
    assert(math.abs(evalAt(-2.0) - 0.0) < 1e-9)   // -2 * step(-2) = 0
    assert(math.abs(evalAt(0.0)  - 0.0) < 1e-9)   // 0  * step(0)  = 0
  }

  "e.integrate(v)" should "forward to the package-level integrate" in
  {
    val direct = integrate(Power(x, _Number(2)), x)
    import scalar.Syntax.integrate   // bring only the extension into local scope
    assert(Power(x, _Number(2)).integrate(x) == direct)
  }
