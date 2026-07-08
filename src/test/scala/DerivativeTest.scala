package it.grypho.scala.leonardo

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

  // d/dx(x² + 2x) at x=1 → 2 + 2 = 4
  "derivative of x² + 2x at x=1" should "be 4.0" in
  {
    val expr = _Derivative(Sum(Power(x, _Number(2)), Product(_Number(2), x)), x)
    assert(math.abs(evalNum(expr, "x" -> 1.0) - 4.0) < 1e-4)
  }

  // d/dx(x³) at x=2 → 3*4 = 12
  "derivative of x³ at x=2" should "be 12.0" in
  {
    val expr = _Derivative(Power(x, _Number(3)), x)
    assert(math.abs(evalNum(expr, "x" -> 2.0) - 12.0) < 1e-4)
  }

  // --- product and quotient rules ---

  // d/dx(x * sin(x)) = sin(x) + x*cos(x), at x=0 → 0
  "derivative of x·sin(x) at x=0" should "be 0.0" in
  {
    val expr = _Derivative(Product(x, Sin(x)), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 0.0) < 1e-5)
  }

  // d/dx(1/x) = -1/x², at x=2 → -0.25
  "derivative of 1/x at x=2" should "be -0.25" in
  {
    val expr = _Derivative(Ratio(_Number(1), x), x)
    assert(math.abs(evalNum(expr, "x" -> 2.0) + 0.25) < 1e-4)
  }

  // --- transcendental functions ---

  // d/dx(sin(x)) = cos(x), at x=0 → 1
  "derivative of sin(x) at x=0" should "be 1.0" in
  {
    val expr = _Derivative(Sin(x), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 1.0) < 1e-5)
  }

  // d/dx(cos(x)) = -sin(x), at x=π/2 → -1
  "derivative of cos(x) at x=π/2" should "be -1.0" in
  {
    val expr = _Derivative(Cos(x), x)
    assert(math.abs(evalNum(expr, "x" -> math.Pi / 2) + 1.0) < 1e-4)
  }

  // d/dx(exp(x)) = exp(x), at x=0 → 1
  "derivative of exp(x) at x=0" should "be 1.0" in
  {
    val expr = _Derivative(Exp(x), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 1.0) < 1e-5)
  }

  // d/dx(log(x)) = 1/x, at x=1 → 1
  "derivative of log(x) at x=1" should "be 1.0" in
  {
    val expr = _Derivative(Log(x), x)
    assert(math.abs(evalNum(expr, "x" -> 1.0) - 1.0) < 1e-4)
  }

  // --- chain rule ---

  // d/dx(sin(x²)) = 2x·cos(x²), at x=0 → 0
  "derivative of sin(x²) at x=0" should "be 0.0" in
  {
    val expr = _Derivative(Sin(Power(x, _Number(2))), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 0.0) < 1e-5)
  }

  // d/dx(exp(2x)) = 2·exp(2x), at x=0 → 2
  "derivative of exp(2x) at x=0" should "be 2.0" in
  {
    val expr = _Derivative(Exp(Product(_Number(2), x)), x)
    assert(math.abs(evalNum(expr, "x" -> 0.0) - 2.0) < 1e-4)
  }

  // --- higher-order and functional nodes (no infinite recursion) ---

  // d/dx(d/dx(x³)) = 6x, at x=2 → 12
  "second derivative of x³ at x=2" should "be 12.0" in
  {
    val expr = _Derivative(_Derivative(Power(x, _Number(3)), x), x)
    assert(math.abs(evalNum(expr, "x" -> 2.0) - 12.0) < 1e-4)
  }

  // d/dx(d/dx(sin(x))) = -sin(x), at x=π/2 → -1
  "second derivative of sin(x) at x=π/2" should "be -1.0" in
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

  // d/dx ∫x dx = x (fundamental theorem of calculus), at x=4 → 4
  "derivative of the integral of x w.r.t. x" should "recover the integrand" in
  {
    val expr = _Derivative(_Integral(x, x), x)
    assert(math.abs(evalNum(expr, "x" -> 4.0) - 4.0) < 1e-4)
  }

  // --- parse + derive round-trip ---

  "parse+derive of \"x * x\" w.r.t. x at x=3" should "equal 6.0" in
  {
    val result = Parser.parse("x * x")
    assert(result.successful, s"parse failed: $result")
    val expr = _Derivative(result.get, x)
    assert(math.abs(evalNum(expr, "x" -> 3.0) - 6.0) < 1e-4)
  }
