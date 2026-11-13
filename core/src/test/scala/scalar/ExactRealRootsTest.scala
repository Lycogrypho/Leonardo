package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0343 -- the real roots of an exact polynomial, found with no floating point: a Sturm sequence over
 *  the integers, bisection on dyadic points, the rational roots and a residual quadratic's surds exact. */
class ExactRealRootsTest extends AnyFlatSpec:

  private val env = new Environment(workingPrecision = 30, exactMode = true)
  private val x   = _Variable("x")
  private val Big = Long.MaxValue

  private def poly(s: String): QPoly = Parser.parse(s, Some(30)) match
    case Parser.Success(e, _) => qFraction(e, x, env).fold(fail(s"not a polynomial: $s"))(_._1)
    case other                => fail(s"parse failed for '$s': $other")

  private def roots(s: String): Vector[ExactRoot] = exactRealRoots(poly(s), Big).fold(fail(s"declined: $s"))(identity)

  private def exact(s: String): _Value = Parser.parse(s, Some(30)) match
    case Parser.Success(e, _) => e.eval(env).fold(l => fail(s"no value: $l"), identity)
    case other                => fail(other.toString)

  "the real roots" should "be counted exactly, whatever the signs along the Sturm sequence" in {
    // a remainder that is one negative constant: read as its own gcd, it flipped and two roots vanished
    val rs = roots("1/2 - 9/4*x + 23/10*x^2 - 49/28*x^3 + 93/82*x^4")
    assert(rs.size == 2 && rs.forall(_.isInstanceOf[ExactRoot.Isolated]), rs.toString)
    assert(roots("x^2 + 1").isEmpty && roots("x^4 + x^2 + 7").isEmpty && roots("5").isEmpty)
    assert(roots("(x - 1)^2*(x + 2)").map(_.expression) == Vector(_Rational(-2), _Rational(1)))
    assert(exactRealRoots(Vector.empty, Big).isEmpty)
  }

  "rational roots" should "be exact however close they lie" in {
    val rs = roots("(x - 1)*(x - 1 - 1/10^20)*(x - 1 - 2/10^20)")
    assert(rs.map(_.expression) == Vector(exact("1"), exact("1 + 1/10^20"), exact("1 + 2/10^20")))
    assert(roots("(x - 3/8)*(x + 7/3)*(x - 22/7)*(3*x - 1)").map(_.expression) ==
      Vector(exact("-7/3"), exact("1/3"), exact("3/8"), exact("22/7")))
  }

  "a quadratic left after the rational roots" should "give its surds, in order" in {
    val rs = roots("(x - 1/3)*(x^2 - 2)")
    assert(rs.forall(_.isInstanceOf[ExactRoot.Exact]), rs.toString)
    assert(rs(1).expression == exact("1/3"))
    assert(math.abs(rs(0).toDouble + math.sqrt(2)) < 1e-15 && math.abs(rs(2).toDouble - math.sqrt(2)) < 1e-15)
    assert(rs(0).expression == exact("-(2^(1/2))") && rs(2).expression == exact("2^(1/2)"))
  }

  "a root with no exact form" should "be read to Double precision from its isolation" in {
    roots("x^3 - 2*x - 5") match
      case Vector(ExactRoot.Isolated(r)) => assert(math.abs(r - 2.0945514815423265) <= 4 * math.ulp(r), r.toString)
      case other                         => fail(other.toString)
    // two quadratics: four surds exist, but this tier forms only a single residual quadratic's
    val rs = roots("(x^2 - 2)*(x^2 - 3)")
    assert(rs.size == 4 && rs.map(_.toDouble).zip(Seq(-math.sqrt(3), -math.sqrt(2), math.sqrt(2), math.sqrt(3)))
      .forall((a, b) => math.abs(a - b) <= 4 * math.ulp(b)), rs.toString)
  }

  "products of random rational factors" should "give back exactly the roots they were built from" in {
    var seed = 20261112L
    def next(n: Int): Int = { seed = (seed * 6364136223846793005L + 1442695040888963407L); ((seed >>> 33) % n).toInt }
    for _ <- 1 to 40 do
      val k    = 1 + next(6)
      val made = Vector.fill(k)((next(41) - 20, 1 + next(30))).distinctBy((n, d) => _Rational.of(BigInt(n), BigInt(d)))
      val src  = made.map((n, d) => s"($d*x - ($n))").mkString("*") + (if next(2) == 0 then "*(x^2 + 1)" else "")
      val want = made.flatMap((n, d) => _Rational.of(BigInt(n), BigInt(d))).sortBy(_.toDouble)
      assert(roots(src).map(_.expression) == want, src)
  }

  "the budget" should "decline, not answer part of the roots" in {
    assert(exactRealRoots(poly("(x - 1)*(x - 2)*(x - 3)"), 5).isEmpty)
    val p = poly("(x - 1)*(x - 2)*(x - 3)")
    val (_, work) = isolateRootsMeasured(p, Vector.empty, Big)
    assert(exactRealRoots(p, work).isDefined && exactRealRoots(p, work - 1).isEmpty)
  }

  "the sign chart's data" should "give each factor's sign between the roots and its zeros at them" in {
    val (n, d) = (poly("(x - 1)^2*(x + 3)"), poly("x - 2"))
    val iso = isolateRoots(qTimesPoly(n, d), Vector(n, d), Big, multiplicities = true).fold(fail("declined"))(identity)
    assert(iso.roots.map(_.expression) == Vector(_Rational(-3), _Rational(1), _Rational(2)))
    assert(iso.intervalSigns == Vector(Vector(-1, 1, 1, 1), Vector(-1, -1, -1, 1)))
    assert(iso.vanishes == Vector(Vector(true, true, false), Vector(false, false, true)))
    assert(iso.multiplicity == Vector(Vector(1, 2, 0), Vector(0, 0, 1)))
  }

  "singularities" should "be read with their exact orders" in {
    def sing(s: String) = Parser.parse(s, Some(30)) match
      case Parser.Success(e, _) => exactSingularities(e, x, env).map(_.map((r, o) => (r.expression, o)))
      case other                => fail(other.toString)
    assert(sing("1/((x - 1)^2*(x - 2))").contains(Vector((_Rational(1), 2), (_Rational(2), 1))))
    assert(sing("(x - 1)/((x - 1)*(x + 1))").contains(Vector((_Rational(-1), 1), (_Rational(1), 0))))
    // two poles 1e-10 apart are two simple poles, not one double pole
    assert(sing("1/((x - 1)*(x - 1 - 1/10^10))").contains(Vector((exact("1"), 1), (exact("1 + 1/10^10"), 1))))
    assert(sing("sin(x)/x").isEmpty)
  }
