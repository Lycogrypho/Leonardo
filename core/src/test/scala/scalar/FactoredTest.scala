package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0351 -- a rational function read as its written factors, and the real roots of one polynomial. */
class FactoredTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def factored(src: String): Option[Factored] = Parser.parse(src) match
    case Parser.Success(e, _) => factoredIn(e, x, env)
    case other                => fail(s"parse failed for '$src': $other")

  private def read(src: String): Factored = factored(src).fold(fail(s"not read: $src"))(identity)

  "a quadratic" should "have a double root only when its discriminant is rounding" in {
    realRoots(Vector(0.09, -0.6, 1.0)) match
      case Vector(RealRoot(r, 2, _)) => assert(math.abs(r - 0.3) < 1e-15)
      case other                     => fail(other.toString)
    assert(realRoots(Vector(1.000001, -2.000001, 1.0)).map(_.multiplicity) == Vector(1, 1))
    assert(realRoots(Vector(1.0, 0.0, 1.0)).isEmpty)
    // the stable form: the small root of x^2 + 1e8*x + 1
    assert(realRoots(Vector(1.0, 1e8, 1.0)).map(_.at) == Vector(-1e8, -1e-8))
    // and the verdict does not move with the units of x or of the polynomial
    for k <- Seq(1e-9, 1e9) do
      assert(realRoots(Vector(0.09 * k * k, -0.6 * k, 1.0)).map(_.multiplicity) == Vector(2), k.toString)
      assert(realRoots(Vector(1.000001, -2.000001, 1.0).map(_ * k)).size == 2, k.toString)
  }

  "the written factors" should "give each root its multiplicity and the sign between them" in {
    val f = read("(x - 1)^2*(x + 3)/(x - 2)")
    assert(f.cuts == Vector(CutPoint(-3.0, 1, 0), CutPoint(1.0, 2, 0), CutPoint(2.0, 0, 1)))
    assert((0 to 3).map(f.intervalSign) == Vector(1, -1, -1, 1))
    assert(!f.isPolynomial && !f.isZero && read("(x - 1)^2*(x + 3)/2").isPolynomial)
  }

  it should "add the orders of two factors that share a root, and keep close roots apart" in {
    assert(read("(x - 1)*(x^2 - 1)").cuts == Vector(CutPoint(-1.0, 1, 0), CutPoint(1.0, 2, 0)))
    assert(read("(x^3 - 1)/(x - 1)").cuts == Vector(CutPoint(1.0, 1, 1)))
    assert(read("(x - 1)*(x - 1.00000001)").cuts.map(_.at) == Vector(1.0, 1.00000001))
    // the same root computed twice, one ulp apart, is one point
    assert(read("(x - 0.3)*(x - 0.1*3)").cuts.map(_.zeroOrder) == Vector(2))
  }

  it should "read a negative power below the bar, a constant's sign, and the zero function" in {
    assert(read("(x - 1)^(-2)").cuts == Vector(CutPoint(1.0, 0, 2)))
    assert((0 to 1).map(read("-2*(x - 1)").intervalSign) == Vector(1, -1))
    assert((0 to 1).map(read("(x - 1)/(-2)").intervalSign) == Vector(1, -1))
    val zero = read("0*(x - 1)")
    assert(zero.isZero && zero.intervalSign(0) == 0)
  }

  it should "decline what is not a rational function with numeric coefficients" in {
    assert(factored("sin(x)*(x - 1)").isEmpty)
    assert(factored("(x - a)*(x - 1)").isEmpty)
    assert(factored("(x - 1)^(1/2)").isEmpty)
    assert(factored("(x - 1)/(0*x)").isEmpty)
  }
