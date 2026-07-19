package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import matrix.*
import equation.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class NormalizeTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  // --- collect: polynomial coefficient extraction ---

  "collect(10x - 2x)" should "fold the like terms into one coefficient" in
  {
    assert(collect(parse("10 * x - 2 * x"), x).contains(Vector(_Number(0), _Number(8))))
  }

  "collect of a quadratic" should "produce the dense coefficient list" in
  {
    // xÂ² + 3x + 2 - x  â†’  2 + 2x + xÂ²
    assert(collect(parse("x^2 + 3 * x + 2 - x"), x)
     .contains(Vector(_Number(2), _Number(2), _Number(1))))
  }

  "collect((x + 1)^2)" should "expand by convolution" in
  {
    assert(collect(parse("(x + 1)^2"), x)
     .contains(Vector(_Number(1), _Number(2), _Number(1))))
  }

  "collect with symbolic coefficients" should "keep them as expressions free of v" in
  {
    assert(collect(parse("a * x + b * x"), x)
     .contains(Vector(_Number(0), Sum(_Variable("a"), _Variable("b")))))
  }

  "collect of constants and unrelated variables" should "be a degree-0 vector" in
  {
    assert(collect(_Number(5), x).contains(Vector(_Number(5))))
    assert(collect(y, x).contains(Vector(y)))
    assert(collect(x, x).contains(Vector(_Number(0), _Number(1))))
  }

  "collect(x / 2)" should "divide the coefficients through" in
  {
    assert(collect(parse("x / 2"), x).contains(Vector(_Number(0), _Number(0.5))))
  }

  "collect(x^0)" should "be the constant 1" in
  {
    assert(collect(Power(x, _Number(0)), x).contains(Vector(_Number(1))))
  }

  "collect of non-polynomial forms" should "be None" in
  {
    assert(collect(Sin(x), x).isEmpty)                       // sin(v)
    assert(collect(parse("1 / x"), x).isEmpty)               // v in a denominator
    assert(collect(Power(_Number(2), x), x).isEmpty)         // v in an exponent
    assert(collect(Power(x, _Number(2.5)), x).isEmpty)       // non-integer power
    assert(collect(parse("(x + 1)^25"), x).isEmpty)          // beyond the expansion cap
  }

  // --- normalize: rebuild with like terms folded ---

  "normalize(10x - 2x)" should "be 8x regardless of tree shape" in
  {
    assert(normalize(parse("10 * x - 2 * x"), x) == Product(_Number(8), x))
  }

  "normalize(x - x)" should "collapse to 0" in
  {
    assert(normalize(parse("x - x"), x) == _Number(0))
  }

  "normalize((x + 1)^2)" should "be the expanded ascending polynomial" in
  {
    assert(normalize(parse("(x + 1)^2"), x)
      == Sum(Sum(_Number(1), Product(_Number(2), x)), Power(x, _Number(2))))
  }

  "normalize" should "elide unit coefficients and zero terms" in
  {
    // xÂ² + x, no constant term, both coefficients 1
    assert(normalize(parse("x * x + x"), x) == Sum(x, Power(x, _Number(2))))
  }

  "normalize of a non-polynomial expression" should "return it unchanged" in
  {
    val e = Sin(Sum(x, x))
    assert(normalize(e, x) == e)
  }

  "normalize of a matrix" should "fold like terms element-wise" in
  {
    val m = _Matrix(1, 2, Vector(parse("10 * x - 2 * x"), _Number(7)))
    assert(normalize(m, x) == _Matrix(1, 2, Vector(Product(_Number(8), x), _Number(7))))
  }

  "normalize of an equation" should "fold like terms on both sides" in
  {
    val eq = _Equation(parse("10 * x - 2 * x"), parse("x + x"))
    assert(normalize(eq, x) == _Equation(Product(_Number(8), x), Product(_Number(2), x)))
  }

  // --- Syntax sugar ---

  "e.normalize(v) and e.collect(v)" should "forward to the package-level functions" in
  {
    val directNormalize = normalize(parse("10 * x - 2 * x"), x)
    val directCollect   = collect(parse("10 * x - 2 * x"), x)
    // bring only the extensions into local scope (specific import wins over wildcard)
    import scalar.Syntax.{normalize, collect}
    assert(parse("10 * x - 2 * x").normalize(x) == directNormalize)
    assert(parse("10 * x - 2 * x").collect(x) == directCollect)
  }
