package it.grypho.scala.leonardo

import core.*
import scalar.*
import scalar.Syntax.*
import org.scalatest.flatspec.AnyFlatSpec


class SimplifyTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  // --- memoization (issue 4.1, split from legacy 19) ---

  // simplify(Sum(x, x)) builds a fresh Product(2, x) on a cache miss, so reference
  // identity across two calls proves the memo hit.
  "repeated simplify calls on equal trees" should "return the cached instance" in
  {
    val first = Sum(x, x).simplify()
    assert(Sum(x, x).simplify() eq first)
    assert(first == Product(_Number(2), x))
  }

  // --- identity / absorbing element rules ---

  "simplify(x + 0)" should "equal x" in
  {
    assert(Sum(x, _Number(0)).simplify() == x)
  }

  "simplify(0 + x)" should "equal x" in
  {
    assert(Sum(_Number(0), x).simplify() == x)
  }

  "simplify(x * 1)" should "equal x" in
  {
    assert(Product(x, _Number(1)).simplify() == x)
  }

  "simplify(1 * x)" should "equal x" in
  {
    assert(Product(_Number(1), x).simplify() == x)
  }

  "simplify(x * 0)" should "equal 0" in
  {
    assert(Product(x, _Number(0)).simplify() == _Number(0))
  }

  "simplify(0 * x)" should "equal 0" in
  {
    assert(Product(_Number(0), x).simplify() == _Number(0))
  }

  "simplify(x / 1)" should "equal x" in
  {
    assert(Ratio(x, _Number(1)).simplify() == x)
  }

  "simplify(0 / x)" should "equal 0" in
  {
    assert(Ratio(_Number(0), x).simplify() == _Number(0))
  }

  "simplify(x ^ 0)" should "equal 1" in
  {
    assert(Power(x, _Number(0)).simplify() == _Number(1))
  }

  "simplify(x ^ 1)" should "equal x" in
  {
    assert(Power(x, _Number(1)).simplify() == x)
  }

  "simplify(1 ^ x)" should "equal 1" in
  {
    assert(Power(_Number(1), x).simplify() == _Number(1))
  }

  "simplify(0 ^ 0)" should "remain 0^0 (undefined, not 1)" in
  {
    assert(Power(_Number(0), _Number(0)).simplify() == Power(_Number(0), _Number(0)))
  }

  "simplify(0 ^ 2)" should "still fold to 0" in
  {
    assert(Power(_Number(0), _Number(2)).simplify() == _Number(0))
  }

  // --- constant folding ---

  "simplify(2 + 3)" should "equal 5" in
  {
    assert(Sum(_Number(2), _Number(3)).simplify() == _Number(5))
  }

  "simplify(2 * 3)" should "equal 6" in
  {
    assert(Product(_Number(2), _Number(3)).simplify() == _Number(6))
  }

  "simplify(9 / 3)" should "equal 3" in
  {
    assert(Ratio(_Number(9), _Number(3)).simplify() == _Number(3))
  }

  "simplify(2 ^ 3)" should "equal 8" in
  {
    assert(Power(_Number(2), _Number(3)).simplify() == _Number(8))
  }

  // --- same-operand rules ---

  "simplify(x + x)" should "equal 2*x" in
  {
    assert(Sum(x, x).simplify() == Product(_Number(2), x))
  }

  // x + (-1 * x) → 0
  "simplify(x + (-1)*x)" should "equal 0" in
  {
    assert(Sum(x, Product(_Number(-1), x)).simplify() == _Number(0))
  }

  // (-1 * x) + x → 0  (mirror)
  "simplify((-1)*x + x)" should "equal 0" in
  {
    assert(Sum(Product(_Number(-1), x), x).simplify() == _Number(0))
  }

  "simplify(x * x)" should "equal x^2" in
  {
    assert(Product(x, x).simplify() == Power(x, _Number(2)))
  }

  "simplify(x / x)" should "equal 1" in
  {
    assert(Ratio(x, x).simplify() == _Number(1))
  }

  "simplify(0 / 0)" should "remain 0/0 (undefined, not 1)" in
  {
    assert(Ratio(_Number(0), _Number(0)).simplify() == Ratio(_Number(0), _Number(0)))
  }

  // --- double negation ---

  "simplify(-(-x))" should "equal x" in
  {
    assert(Product(_Number(-1), Product(_Number(-1), x)).simplify() == x)
  }

  "simplify(-(x * -1))" should "equal x" in
  {
    assert(Product(_Number(-1), Product(x, _Number(-1))).simplify() == x)
  }

  "simplify((-1 * x) * -1)" should "equal x" in
  {
    assert(Product(Product(_Number(-1), x), _Number(-1)).simplify() == x)
  }

  "simplify((x * -1) * -1)" should "equal x" in
  {
    assert(Product(Product(x, _Number(-1)), _Number(-1)).simplify() == x)
  }

  // --- inverse function pairs ---

  "simplify(log(exp(x)))" should "equal x" in
  {
    assert(Log(Exp(x)).simplify() == x)
  }

  "simplify(exp(log(x)))" should "equal x" in
  {
    assert(Exp(Log(x)).simplify() == x)
  }

  // --- known function values ---

  "simplify(exp(0))" should "equal 1" in
  {
    assert(Exp(_Number(0)).simplify() == _Number(1))
  }

  "simplify(log(1))" should "equal 0" in
  {
    assert(Log(_Number(1)).simplify() == _Number(0))
  }

  "simplify(sin(0))" should "equal 0" in
  {
    assert(Sin(_Number(0)).simplify() == _Number(0))
  }

  "simplify(cos(0))" should "equal 1" in
  {
    assert(Cos(_Number(0)).simplify() == _Number(1))
  }

  // --- recursive descent ---

  "simplify applied to nested expression ((x*0)+1)*x" should "equal x" in
  {
    // ((x*0) + 1) * x  →  (0 + 1) * x  →  1 * x  →  x
    val expr = Product(Sum(Product(x, _Number(0)), _Number(1)), x)
    assert(expr.simplify() == x)
  }

  // --- numeric correctness after simplification ---

  "simplify of (sin(x)^2 + cos(x)^2) at x=1" should "evaluate to 1" in
  {
    val e = new Environment().withBinding("x", _Number(1))
    val identity = Sum(Power(Sin(x), _Number(2)), Power(Cos(x), _Number(2)))
    identity.simplify().eval(e) match
      case Right(_Number(v)) => assert(math.abs(v - 1.0) < 1e-4)
      case other             => fail(s"expected a numeric result but got: $other")
  }

  // --- simplifyFully ---

  "simplifyFully" should "be identical to simplify when one pass suffices" in
  {
    assert(Sum(x, _Number(0)).simplifyFully() == x)
  }

  it should "cascade identity eliminations across nesting levels" in
  {
    // (x * 1 + 0) * 1: single simplify gives (x + 0) * 1; second pass gives x
    val expr = Product(Sum(Product(x, _Number(1)), _Number(0)), _Number(1))
    assert(expr.simplifyFully() == x)
  }

  it should "reach fixpoint on a deeply nested constant expression" in
  {
    // (((2 + 3) * 1) + 0) ^ 1
    val expr = Power(Sum(Product(Sum(_Number(2), _Number(3)), _Number(1)), _Number(0)), _Number(1))
    assert(expr.simplifyFully() == _Number(5))
  }

  // --- issue 1.3: Power constant-fold must not produce NaN / Infinity ---

  "simplify((-2)^0.5)" should "stay symbolic rather than fold to NaN" in
  {
    val e = Power(_Number(-2), _Number(0.5))
    val s = e.simplify()
    assert(s == e, s"expected Power(-2, 0.5) but got $s")
  }

  "simplify(10^400)" should "stay symbolic rather than fold to Infinity" in
  {
    val e = Power(_Number(10), _Number(400))
    val s = e.simplify()
    assert(s == e, s"expected Power(10, 400) but got $s")
  }

  "simplify(0^(-1))" should "stay symbolic (division-by-zero domain error)" in
  {
    val e = Power(_Number(0), _Number(-1))
    val s = e.simplify()
    assert(s == e, s"expected Power(0, -1) but got $s")
  }

  "simplify(2^3)" should "still constant-fold to 8 (finite result)" in
  {
    assert(Power(_Number(2), _Number(3)).simplify() == _Number(8))
  }
