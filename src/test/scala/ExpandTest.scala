package it.grypho.scala.leonardo

import core.*
import scalar.*
import scalar.Syntax.*
import org.scalatest.flatspec.AnyFlatSpec


class ExpandTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")
  val z = _Variable("z")

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  def evalNum(expr: _Expression, bindings: (String, Double)*): Double =
    expr.eval(envWith(bindings*)) match
      case Right(_Number(v)) => v
      case other             => fail(s"expected a numeric result but got: $other")

  // --- distribution ---

  "expand(x * (y + z))" should "distribute to x*y + x*z" in
  {
    val result = Product(x, Sum(y, z)).expand()
    assert(result == Sum(Product(x, y), Product(x, z)))
  }

  "(expand((x + y) * z))" should "distribute to x*z + y*z" in
  {
    val result = Product(Sum(x, y), z).expand()
    assert(result == Sum(Product(x, z), Product(y, z)))
  }

  "expand((x+y) * (x+y)) at x=2, y=3" should "equal 25" in
  {
    val expr = Product(Sum(x, y), Sum(x, y))
    assert(math.abs(evalNum(expr.expand(), "x" -> 2.0, "y" -> 3.0) - 25.0) < 1e-4)
  }

  "expand((x+1) * (x+2)) at x=3" should "equal 20" in
  {
    val expr = Product(Sum(x, _Number(1)), Sum(x, _Number(2)))
    assert(math.abs(evalNum(expr.expand(), "x" -> 3.0) - 20.0) < 1e-4)
  }

  "expand(2 * (x + 3)) at x=5" should "equal 16" in
  {
    val expr = Product(_Number(2), Sum(x, _Number(3)))
    assert(math.abs(evalNum(expr.expand(), "x" -> 5.0) - 16.0) < 1e-4)
  }

  // --- power expansion ---

  "expand((x+y)^2) at x=2, y=3" should "equal 25" in
  {
    val expr = Power(Sum(x, y), _Number(2))
    assert(math.abs(evalNum(expr.expand(), "x" -> 2.0, "y" -> 3.0) - 25.0) < 1e-4)
  }

  "expand((x+1)^3) at x=2" should "equal 27" in
  {
    val expr = Power(Sum(x, _Number(1)), _Number(3))
    assert(math.abs(evalNum(expr.expand(), "x" -> 2.0) - 27.0) < 1e-4)
  }

  "expand((x+y)^4) at x=1, y=2" should "equal 81" in
  {
    val expr = Power(Sum(x, y), _Number(4))
    assert(math.abs(evalNum(expr.expand(), "x" -> 1.0, "y" -> 2.0) - 81.0) < 1e-4)
  }

  "expand((x+1)^1)" should "leave the sum unchanged" in
  {
    val result = Power(Sum(x, _Number(1)), _Number(1)).expand()
    assert(result == Sum(x, _Number(1)))
  }

  // non-sum base: expand should recurse but not distribute
  "expand(x^2) where base is not a sum" should "remain x^2" in
  {
    assert(Power(x, _Number(2)).expand() == Power(x, _Number(2)))
  }

  // --- non-integer or zero exponent: no expansion ---

  "expand((x+y)^0)" should "return (x+y)^0 without expanding" in
  {
    val result = Power(Sum(x, y), _Number(0)).expand()
    // expand does not evaluate — simplify() would fold this to 1
    assert(result == Power(Sum(x, y), _Number(0)))
  }

  "expand((x+y)^0.5)" should "return (x+y)^0.5 without expanding" in
  {
    val result = Power(Sum(x, y), _Number(0.5)).expand()
    assert(result == Power(Sum(x, y), _Number(0.5)))
  }

  // --- expand then simplify ---

  "expand((x+1)^2) then simplify at x=3" should "equal 16" in
  {
    val expr = Power(Sum(x, _Number(1)), _Number(2)).expand().simplify()
    assert(math.abs(evalNum(expr, "x" -> 3.0) - 16.0) < 1e-4)
  }

  "constant terms in (x+2)^2 simplify correctly" should "fold 2*2 to 4" in
  {
    // (x+2)^2 expands to x*x + x*2 + 2*x + 2*2
    // after simplify: x^2 + 2x + 2x + 4   (x*2 and 2*x not yet merged — single pass)
    // but numeric eval should still match (x+2)^2
    val expanded = Power(Sum(x, _Number(2)), _Number(2)).expand().simplify()
    assert(math.abs(evalNum(expanded, "x" -> 5.0) - 49.0) < 1e-4)
  }

  // --- nested structure ---

  "expand((x + y) * (x - y))" should "equal the numeric result of x^2 - y^2" in
  {
    // (x - y) = Sum(x, Product(-1, y))
    val xmy = Sum(x, Product(_Number(-1), y))
    val expr = Product(Sum(x, y), xmy).expand()
    // Numeric check: at x=5, y=3: (5+3)(5-3) = 16, 25-9 = 16
    assert(math.abs(evalNum(expr, "x" -> 5.0, "y" -> 3.0) - 16.0) < 1e-4)
  }

  // --- inverse trig: arguments must be expanded (regression for issue #20) ---

  "expand(asin(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = Asin(Product(x, Sum(y, z))).expand()
    assert(result == Asin(Sum(Product(x, y), Product(x, z))))
  }

  "expand(acos(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = Acos(Product(x, Sum(y, z))).expand()
    assert(result == Acos(Sum(Product(x, y), Product(x, z))))
  }

  "expand(atan(x * (y + z)))" should "distribute inside the argument" in
  {
    val result = Atan(Product(x, Sum(y, z))).expand()
    assert(result == Atan(Sum(Product(x, y), Product(x, z))))
  }

  "expand(asin((x+1)^2)) at x=0" should "equal asin(1)" in
  {
    val expr = Asin(Power(Sum(x, _Number(1)), _Number(2))).expand()
    assert(math.abs(evalNum(expr, "x" -> 0.0) - math.asin(1.0)) < 1e-5)
  }
