package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import org.scalatest.flatspec.AnyFlatSpec


class SubstituteTest extends AnyFlatSpec:

  val x = _Variable("x")
  val f = _Variable("f")
  val g = _Variable("g")

  "substitute" should "replace a variable with its definition" in
  {
    val defs = Map("f" -> Sum(Sin(x), x))
    assert(substitute(f, defs) == Sum(Sin(x), x))
  }

  it should "substitute inside operations and functions" in
  {
    val defs = Map("f" -> x)
    val e = Sum(Product(f, f), Cos(Ratio(f, Power(f, _Number(2)))))
    val expected = Sum(Product(x, x), Cos(Ratio(x, Power(x, _Number(2)))))
    assert(substitute(e, defs) == expected)
  }

  it should "resolve chained definitions in one call" in
  {
    val defs = Map("g" -> Product(f, _Number(2)), "f" -> Sum(x, _Number(1)))
    assert(substitute(g, defs) == Product(Sum(x, _Number(1)), _Number(2)))
  }

  it should "leave variables without a definition untouched" in
  {
    assert(substitute(Sum(x, g), Map("f" -> x)) == Sum(x, g))
  }

  it should "be the identity for an empty definitions map" in
  {
    val e = Sum(Sin(x), Product(f, g))
    assert(substitute(e, Map()) == e)
  }

  // --- termination on cycles ---

  it should "terminate on a self-referential definition" in
  {
    val defs = Map("f" -> Sum(f, _Number(1)))
    // inner f stays free at the point of recursion
    assert(substitute(f, defs) == Sum(f, _Number(1)))
  }

  it should "terminate on mutually recursive definitions" in
  {
    val defs = Map("f" -> Sum(g, _Number(1)), "g" -> Sum(f, _Number(2)))
    // f â†’ g + 1 â†’ (f + 2) + 1, then f is blocked by the seen set
    assert(substitute(f, defs) == Sum(Sum(f, _Number(2)), _Number(1)))
  }

  // --- binder positions ---

  it should "not substitute the differentiation variable" in
  {
    val defs = Map("x" -> _Number(3))
    assert(substitute(_Derivative(Product(x, x), x), defs)
        == _Derivative(Product(_Number(3), _Number(3)), x))
  }

  it should "substitute the integrand and limits but not the integration variable" in
  {
    val defs = Map("f" -> Product(x, x), "a" -> _Number(2))
    val a = _Variable("a")
    assert(substitute(_DefIntegral(f, x, _Number(0), a), defs)
        == _DefIntegral(Product(x, x), x, _Number(0), _Number(2)))
  }

  // --- composition with the other algorithms ---

  it should "compose with derive so definitions differentiate correctly" in
  {
    val defs = Map("f" -> Product(x, x))
    val derived = derive(substitute(f, defs), x)
    val env = new Environment().withBinding("x", _Number(3))
    assert(derived.eval(env) == Right(_Number(6.0)))
  }

  it should "be available as method-call syntax" in
  {
    import Syntax.*
    assert(f.substitute(Map("f" -> x)) == x)
  }
