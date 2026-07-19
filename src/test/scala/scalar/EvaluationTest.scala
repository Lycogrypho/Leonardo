package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import org.scalatest.flatspec.AnyFlatSpec


class EvaluationTest extends AnyFlatSpec:

  implicit def env: Environment = new Environment()

  val x_var = _Variable("x")

  // Expected result is an _Expression: a _Number for rows that reduce to a value
  // (matched on the Right branch), a symbolic expression for rows that stay symbolic.
  // Numeric results are compared via toString (display at DefaultPrecision=5) so that
  // floating-point intermediate values (e.g. 7*3.1 = 21.700000000000003) round correctly.
  val evaluationTests: List[(_Expression, _Expression)] = List(
    (_Number(10),                                   _Number(10.0)),
    (_Number(3.1234567890),                         _Number(3.12346)),
    (Sum(_Number(2), _Number(3)),                   _Number(5.0)),
    (Product(_Number(7.0), _Number(3.1)),           _Number(21.7)),
    (Ratio(_Number(1), _Number(4)),                 _Number(0.25)),
    (Sum(_Number(1), Sum(_Number(4), _Number(5))),  _Number(10.0)),
    (Ratio(Product(_Number(3), _Number(8)), _Number(4)), _Number(6.0)),
    (Sin(_Number(scala.math.Pi / 2)),               _Number(1.0)),
    (x_var,                                         x_var),
    (Sum(Sum(_Number(1), _Number(4)), x_var),       Sum(_Number(5), x_var))
  )

  for s <- evaluationTests do
    s"expression \"${s._1}\" " should s"be evaluated to \"${s._2}\" " in
    {
      s._1.eval(env) match
        case Right(x) => assert(x.toString == s._2.toString)
        case Left(e)  => assert(e == s._2)
    }

  // --- domain errors stay symbolic (no silent Infinity/NaN) ---

  "division by zero" should "stay symbolic instead of evaluating to Infinity" in
  {
    val e = Ratio(_Number(1), _Number(0))
    assert(e.eval(env) == Left(e))
  }

  "negative division by zero" should "stay symbolic instead of evaluating to -Infinity" in
  {
    val e = Ratio(_Number(-1), _Number(0))
    assert(e.eval(env) == Left(e))
  }

  "zero divided by zero" should "stay symbolic instead of evaluating to NaN" in
  {
    val e = Ratio(_Number(0), _Number(0))
    assert(e.eval(env) == Left(e))
  }

  // Complex closure: ln/sqrt of a negative real now yield the principal complex
  // value rather than staying symbolic (see ComplexTest for the full behaviour).
  "ln of a negative number" should "yield the principal complex value ln|x| + iÏ€" in
  {
    Ln(_Number(-1)).eval(env) match
      case Right(c: _Complex) =>
        assert(math.abs(c.re) < 1e-9 && math.abs(c.im - math.Pi) < 1e-9)
      case other => fail(s"expected a complex value but got $other")
  }

  "ln of zero" should "stay symbolic instead of evaluating to -Infinity" in
  {
    val e = Ln(_Number(0))
    assert(e.eval(env) == Left(e))
  }

  "zero to a negative power" should "stay symbolic instead of evaluating to Infinity" in
  {
    val e = Power(_Number(0), _Number(-1))
    assert(e.eval(env) == Left(e))
  }

  "negative base with fractional exponent" should "yield the principal complex root" in
  {
    // (-2)^0.5 = iÂ·âˆš2
    Power(_Number(-2), _Number(0.5)).eval(env) match
      case Right(c: _Complex) =>
        assert(math.abs(c.re) < 1e-9 && math.abs(c.im - math.sqrt(2)) < 1e-9)
      case other => fail(s"expected a complex value but got $other")
  }

  // tan(pi/2) in floating-point is NOT Infinity (pi/2 has no exact double representation);
  // it evaluates to ~1.633e16, which is finite and propagates normally.
  // The guard fires for NaN inputs (e.g. tan(NaN) = NaN, tan(Infinity) = NaN).
  "tan(pi/4)" should "evaluate to approximately 1" in
  {
    Tg(_Number(math.Pi / 4)).eval(env) match
      case Right(_Number(x)) => assert(math.abs(x - 1.0) < 1e-10)
      case other             => fail(s"expected 1.0 but got $other")
  }

  "tan(NaN input)" should "stay symbolic instead of propagating NaN" in
  {
    val e = Tg(_Number(Double.NaN))
    assert(e.eval(env) == Left(e))
  }

  "a very large number" should "evaluate without Long overflow" in
  {
    val large = 1e15
    _Number(large).eval(env) match
      case Right(_Number(x)) => assert(x == large)
      case other             => fail(s"expected $large but got: $other")
  }

  // --- display precision: _Number.display(p) rounds for REPL output ---

  "_Number(3.14159265).display(3)" should "produce \"3.142\"" in
  {
    assert(_Number(3.14159265).display(3) == "3.142")
  }

  "_Number(2.718281828459045).display(8)" should "round to 8 decimal places" in
  {
    assert(_Number(2.718281828459045).display(8) == "2.71828183")
  }

  // --- issue 1: withBinding child env ---

  "withBinding" should "resolve local binding without copying parent variables" in
  {
    val parent = new Environment().withBinding("a", _Number(10))
    val child = parent.withBinding("b", _Number(20))
    assert(child.get("b").contains(_Number(20)))
    assert(child.get("a").contains(_Number(10)))
    assert(child.get("c").isEmpty)
    assert(child.isBound("b"))
    assert(child.isBound("a"))
    assert(!child.isBound("c"))
  }

  // --- issue 4: Product short-circuits on zero ---

  "Product(0, unbound variable)" should "evaluate to 0 without evaluating the rhs" in
  {
    val z = _Variable("z")
    Product(_Number(0), z).eval(new Environment()) match
      case Right(_Number(y)) => assert(y == 0.0)
      case other             => fail(s"expected 0.0 but got: $other")
  }

  "Product(unbound variable, 0)" should "evaluate to 0 without evaluating the lhs further" in
  {
    val z = _Variable("z")
    Product(z, _Number(0)).eval(new Environment()) match
      case Right(_Number(y)) => assert(y == 0.0)
      case other             => fail(s"expected 0.0 but got: $other")
  }

  // --- variable binding ---

  val a = _Variable("a")
  val b = _Variable("b")

  "an assigned variable" should "be evaluated to a numeric value" in
  {
    val e1 = new Environment().withBinding("a", _Number(3))
    assert(Sum(a, b).eval(e1) == Sum(_Number(3), _Variable("b")).eval(e1))

    val e2 = e1.withBinding("b", _Number(7))
    assert(Sum(a, b).eval(e2) == _Number(10).eval(e2))
  }
