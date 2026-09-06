package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class DerivativeTest extends AnyFlatSpec:

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  def evalNum(expr: _Expression, bindings: (String, Double)*): Double =
    expr.eval(envWith(bindings*)) match
      case Right(_Number(x)) => x
      case other             => fail(s"expected a numeric result but got: $other")

  val x = _Variable("x")
  val y = _Variable("y")

  // --- constants and variables ---

  "derivative of a constant" should "be 0" in
  {
    assert(evalNum(_Derivative(_Number(3), x)) == 0.0)
  }

  "derivative of x w.r.t. x" should "be 1" in
  {
    assert(evalNum(_Derivative(x, x)) == 1.0)
  }

  "derivative of y w.r.t. x" should "be 0" in
  {
    assert(evalNum(_Derivative(y, x)) == 0.0)
  }

  // --- polynomial rules ---

  // d/dx(3x) = 3
  "derivative of 3x" should "be 3" in
  {
    assert(evalNum(_Derivative(Product(_Number(3), x), x)) == 3.0)
  }

  // d/dx(xÂ² + 2x) at x=1 â†’ 2 + 2 = 4
  "derivative of xÂ² + 2x at x=1" should "be 4.0" in
  {
    val expr = _Derivative(Sum(Power(x, _Number(2)), Product(_Number(2), x)), x)
    assert(math.abs(evalNum(expr, "x" -> 1.0) - 4.0) < 1e-4)
  }

  // d/dx(xÂ³) at x=2 â†’ 3*4 = 12
  "derivative of xÂ³ at x=2" should "be 12.0" in
  {
    val expr = _Derivative(Power(x, _Number(3)), x)
    assert(math.abs(evalNum(expr, "x" -> 2.0) - 12.0) < 1e-4)
  }

  // --- product and quotient rules ---

  // d/dx(x * sin(x)) = sin(x) + x*cos(x), at x=0 â†’ 0
  "derivative of xÂ·sin(x) at x=0" should "be 0.0" in
  {
    val expr = _Derivative(Product(x, Sin(x)), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 0.0) < 1e-5)
  }

  // d/dx(1/x) = -1/xÂ², at x=2 â†’ -0.25
  "derivative of 1/x at x=2" should "be -0.25" in
  {
    val expr = _Derivative(Ratio(_Number(1), x), x)
    assert(math.abs(evalNum(expr, "x" -> 2.0) + 0.25) < 1e-4)
  }

  // --- transcendental functions ---

  // d/dx(sin(x)) = cos(x), at x=0 â†’ 1
  "derivative of sin(x) at x=0" should "be 1.0" in
  {
    val expr = _Derivative(Sin(x), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 1.0) < 1e-5)
  }

  // d/dx(cos(x)) = -sin(x), at x=Ï€/2 â†’ -1
  "derivative of cos(x) at x=Ï€/2" should "be -1.0" in
  {
    val expr = _Derivative(Cos(x), x)
    assert(math.abs(evalNum(expr, "x" -> math.Pi / 2) + 1.0) < 1e-4)
  }

  // d/dx(exp(x)) = exp(x), at x=0 â†’ 1
  "derivative of exp(x) at x=0" should "be 1.0" in
  {
    val expr = _Derivative(Exp(x), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 1.0) < 1e-5)
  }

  // d/dx(ln(x)) = 1/x, at x=1 â†’ 1
  "derivative of ln(x) at x=1" should "be 1.0" in
  {
    val expr = _Derivative(Ln(x), x)
    assert(math.abs(evalNum(expr, "x" -> 1.0) - 1.0) < 1e-4)
  }

  // d/dx(logâ‚â‚€(x)) = 1/(xÂ·ln(10)), at x=1 â†’ 1/ln(10) â‰ˆ 0.4343
  "derivative of log(x, 10) at x=1" should "be 1/ln(10)" in
  {
    val expr = _Derivative(LogBase(x, _Number(10)), x)
    assert(math.abs(evalNum(expr, "x" -> 1.0) - (1.0 / math.log(10))) < 1e-4)
  }

  // --- chain rule ---

  // d/dx(sin(xÂ²)) = 2xÂ·cos(xÂ²), at x=0 â†’ 0
  "derivative of sin(xÂ²) at x=0" should "be 0.0" in
  {
    val expr = _Derivative(Sin(Power(x, _Number(2))), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 0.0) < 1e-5)
  }

  // d/dx(exp(2x)) = 2Â·exp(2x), at x=0 â†’ 2
  "derivative of exp(2x) at x=0" should "be 2.0" in
  {
    val expr = _Derivative(Exp(Product(_Number(2), x)), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 2.0) < 1e-4)
  }

  // --- higher-order and functional nodes (no infinite recursion) ---

  // d/dx(d/dx(xÂ³)) = 6x, at x=2 â†’ 12
  "second derivative of xÂ³ at x=2" should "be 12.0" in
  {
    val expr = _Derivative(_Derivative(Power(x, _Number(3)), x), x)
    assert(math.abs(evalNum(expr, "x" -> 2.0) - 12.0) < 1e-4)
  }

  // d/dx(d/dx(sin(x))) = -sin(x), at x=Ï€/2 â†’ -1
  "second derivative of sin(x) at x=Ï€/2" should "be -1.0" in
  {
    val expr = _Derivative(_Derivative(Sin(x), x), x)
    assert(math.abs(evalNum(expr, "x" -> math.Pi / 2) + 1.0) < 1e-4)
  }

  // Differentiating an indefinite integral w.r.t. an unrelated variable must not
  // loop forever; it stays symbolic.
  "derivative of an integral w.r.t. an unrelated variable" should "stay symbolic" in
  {
    _Derivative(_Integral(x, x), y).eval(new Environment()) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  // d/dx âˆ«x dx = x (fundamental theorem of calculus), at x=4 â†’ 4
  "derivative of the integral of x w.r.t. x" should "recover the integrand" in
  {
    val expr = _Derivative(_Integral(x, x), x)
    assert(math.abs(evalNum(expr, "x" -> 4.0) - 4.0) < 1e-4)
  }

  // --- base-0 power rule guard (issue #25) ---

  // d/dx(0^y) where y is independent of x: 0^y is a constant â†’ 0
  "derivative of 0^y w.r.t. x (y independent of x)" should "be 0" in
  {
    assert(evalNum(_Derivative(Power(_Number(0), y), x)) == 0.0)
  }

  // d/dx(0^x): the general power rule would inject log(0) into the tree;
  // the guard stays symbolic instead to avoid that domain error expression.
  "derivative of 0^x w.r.t. x (base 0, exponent depends on x)" should "stay symbolic" in
  {
    _Derivative(Power(_Number(0), x), x).eval(new Environment()) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  // --- memoization (issue 4.1, split from legacy 19) ---

  // The result tree is freshly built on a cache miss, so reference identity (eq)
  // across two calls â€” even from structurally equal but distinct input trees â€”
  // proves the memo hit.
  "repeated derive calls on equal trees" should "return the cached instance" in
  {
    val first = derive(Product(x, Sin(x)), x)
    assert(derive(Product(x, Sin(x)), x) eq first)
  }

  "memoized derivatives" should "key on the differentiation variable" in
  {
    val e = Product(x, y)
    assert(derive(e, x) == y)
    assert(derive(e, y) == x)
  }

  // --- parse + derive round-trip ---

  "parse+derive of \"x * x\" w.r.t. x at x=3" should "equal 6.0" in
  {
    val result = Parser.parse("x * x")
    assert(result.successful, s"parse failed: $result")
    val expr = _Derivative(result.get, x)
    assert(math.abs(evalNum(expr, "x" -> 3.0) - 6.0) < 1e-4)
  }

  // --- deriveN: higher-order single-variable derivatives ---

  // d2/dx2(x^3) = 6x; at x=2 -> 12
  "deriveN(x^3, x, 2) at x=2" should "be 12.0" in:
    assert(math.abs(evalNum(deriveN(Power(x, _Number(3)), x, 2), "x" -> 2.0) - 12.0) < 1e-4)

  // d2/dx2(sin x) = -sin x; at x=pi/2 -> -1
  "deriveN(sin(x), x, 2) at x=pi/2" should "be -1.0" in:
    assert(math.abs(evalNum(deriveN(Sin(x), x, 2), "x" -> math.Pi / 2) + 1.0) < 1e-4)

  // d3/dx3(e^x) = e^x; at x=0 -> 1
  "deriveN(exp(x), x, 3) at x=0" should "be 1.0" in:
    assert(math.abs(evalNum(deriveN(Exp(x), x, 3), "x" -> 0.0) - 1.0) < 1e-4)

  // 0th derivative is the identity
  "deriveN(x^2, x, 0)" should "return x^2 unchanged" in:
    assert(deriveN(Power(x, _Number(2)), x, 0) == Power(x, _Number(2)))

  // deriveN result matches iterated _Derivative nesting numerically
  "deriveN(x^3, x, 2)" should "match nested _Derivative at x=1.5" in:
    val nested = _Derivative(_Derivative(Power(x, _Number(3)), x), x)
    val flat   = deriveN(Power(x, _Number(3)), x, 2)
    assert(math.abs(evalNum(nested, "x" -> 1.5) - evalNum(flat, "x" -> 1.5)) < 1e-8)

  // --- derive varargs: mixed / repeated partial derivatives ---

  // d2(x*y^2)/dx dy = d/dy(y^2) = 2y; at y=3 -> 6
  "derive(x*y^2, x, y) at y=3" should "be 6.0" in:
    assert(math.abs(evalNum(derive(Product(x, Power(y, _Number(2))), x, y), "y" -> 3.0) - 6.0) < 1e-4)

  // d2(x^2*y)/dx dx = d/dx(2xy) = 2y; at y=5 -> 10
  "derive(x^2*y, x, x) at y=5" should "be 10.0" in:
    assert(math.abs(evalNum(derive(Product(Power(x, _Number(2)), y), x, x), "y" -> 5.0) - 10.0) < 1e-4)

  // three-variable: d3(x^2*y*z)/dx dx dy = d/dy(2yz) = 2z; at z=4 -> 8
  "derive(x^2*y*z, x, x, y) at z=4" should "be 8.0" in:
    val z = _Variable("z")
    val f = Product(Product(Power(x, _Number(2)), y), z)
    assert(math.abs(evalNum(derive(f, x, x, y), "z" -> 4.0) - 8.0) < 1e-4)

  // varargs with two equal vars matches deriveN(_, _, 2) numerically
  "derive(f, x, x)" should "equal deriveN(f, x, 2) at x=2.7" in:
    val f   = Sum(Power(x, _Number(3)), Product(_Number(2), x))
    val v2a = derive(f, x, x)
    val v2b = deriveN(f, x, 2)
    assert(math.abs(evalNum(v2a, "x" -> 2.7) - evalNum(v2b, "x" -> 2.7)) < 1e-8)

  // --- Syntax sugar ---

  "Syntax e.derive(x, y)" should "compute the mixed partial" in:
    import scalar.Syntax.*
    val f = Product(x, Power(y, _Number(2)))
    assert(math.abs(evalNum(f.derive(x, y), "y" -> 3.0) - 6.0) < 1e-4)

  "Syntax e.deriveN(x, 2)" should "compute the second derivative" in:
    import scalar.Syntax.*
    val f = Power(x, _Number(3))
    assert(math.abs(evalNum(f.deriveN(x, 2), "x" -> 2.0) - 12.0) < 1e-4)