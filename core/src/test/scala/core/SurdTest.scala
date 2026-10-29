package it.grypho.scala.leonardo
package core

import org.scalatest.flatspec.AnyFlatSpec
import spire.math.Real


/** F_0231 -- the quadratic surd `a + b√d` as an exact value.  Runs on Node too, which is what pins
 *  the integer square root (Scala.js has no `BigInteger.sqrt`). */
class SurdTest extends AnyFlatSpec:

  private def rat(n: Int, d: Int = 1): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail(s"$n/$d"))
  private def surd(a: _Rational, b: _Rational, d: Int): _Value = _Surd.of(a, b, BigInt(d)).getOrElse(fail("no value"))
  private def sqrt(n: Int, d: Int = 1): _Value = _Surd.sqrtOf(rat(n, d)).getOrElse(fail("no root"))
  private def get[A](o: Option[A]): A = o.getOrElse(fail("None"))

  /** Two exact values agree with their `Real` meaning to 40 digits. */
  private def same(v: _Value, r: Real): Unit =
    val got = get(_Surd.toReal(v))
    assert((got - r).abs.toRational(200).compare(SpireRationalHelper.tenTo(-40)) < 0, s"$v vs ${r.toRational(200)}")

  "isqrt" should "be the floor of the square root, on every platform" in {
    for n <- List(0, 1, 2, 3, 4, 15, 16, 17, 99, 100, 101) do
      val r = _Surd.isqrt(BigInt(n))
      assert(r * r <= n && (r + 1) * (r + 1) > n, n)
    val big = BigInt("123456789012345678901234567890")
    val r   = _Surd.isqrt(big * big + 7)
    assert(r == big)
  }

  "of" should "collapse to a rational when the radical vanishes or is a perfect square" in {
    assert(surd(rat(3), rat(0), 2) == rat(3))
    assert(surd(rat(1), rat(2), 9) == rat(7))
    assert(sqrt(4, 9) == rat(2, 3))
    assert(_Surd.sqrtOf(rat(-2)).isEmpty)
  }

  it should "move small square factors out of the radicand" in {
    sqrt(8) match
      case s: _Surd => assert(s.d == BigInt(2) && s.b == rat(2))
      case other    => fail(s"$other")
  }

  "equality" should "not depend on the radicand being canonical" in {
    // 2√2 written as 1·√8 and as 2·√2 -- equal, and hashing alike.
    val big = BigInt("1000003") * 1000003 * 2   // a square factor past the trial bound
    val x = get(_Surd.of(rat(0), rat(1000003), BigInt(2)))
    val y = get(_Surd.of(rat(0), rat(1), big))
    assert(x == y && x.hashCode == y.hashCode)
    assert(sqrt(2) != sqrt(3))
    assert(sqrt(2) != get(_Surd.negate(sqrt(2))))
  }

  "the arithmetic" should "be exact over one radical, checked against Real" in {
    val rnd = new scala.util.Random(20261029)
    def r(): _Rational = rat(rnd.nextInt(41) - 20, rnd.nextInt(9) + 1)
    for _ <- 1 to 200 do
      val d  = List(2, 3, 5, 6, 7, 10, 13)(rnd.nextInt(7))
      val x  = surd(r(), r(), d)
      val y  = surd(r(), r(), d * (rnd.nextInt(3) + 1) * (rnd.nextInt(3) + 1))   // same radical class
      val (rx, ry) = (get(_Surd.toReal(x)), get(_Surd.toReal(y)))
      // y's radicand is d·k² only when both factors agree; otherwise the radicals may not combine.
      _Surd.add(x, y).foreach(s => same(s, rx + ry))
      _Surd.multiply(x, y).foreach(s => same(s, rx * ry))
      if get(_Surd.signum(y)) != 0 then _Surd.divide(x, y).foreach(s => same(s, rx / ry))
    same(get(_Surd.multiply(sqrt(2), sqrt(2))), Real(2))
    assert(get(_Surd.multiply(sqrt(2), sqrt(8))) == rat(4))
    assert(get(_Surd.divide(rat(1), sqrt(2))) == get(_Surd.divide(sqrt(2), rat(2))))
  }

  it should "decline radicals that do not combine" in {
    assert(_Surd.add(sqrt(2), sqrt(3)).isEmpty)
    assert(_Surd.multiply(sqrt(2), sqrt(3)).isEmpty)
    assert(_Surd.divide(sqrt(2), rat(0)).isEmpty)
  }

  it should "raise to integer powers within the budget" in {
    assert(get(_Surd.pow(sqrt(2), 2, 1L << 20)) == rat(2))
    same(get(_Surd.pow(surd(rat(1), rat(1), 2), 5, 1L << 20)), (Real(1) + Real(2).sqrt).pow(5))
    same(get(_Surd.pow(sqrt(3), -3, 1L << 20)), Real(1) / Real(3).sqrt.pow(3))
    assert(_Surd.pow(sqrt(2), 2000000000, 1L << 20).isEmpty)
  }

  "signum" should "be decided exactly where the parts disagree" in {
    assert(_Surd.signum(surd(rat(-1), rat(1), 2)).contains(1))     // √2 - 1 > 0
    assert(_Surd.signum(surd(rat(-3, 2), rat(1), 2)).contains(-1)) // √2 - 3/2 < 0
    assert(_Surd.signum(surd(rat(7), rat(-5), 2)).contains(-1))    // 7 - 5√2 < 0
  }

  "exactOf" should "read a surd as its working-precision rational" in {
    val q = get(_Surd.exactOf(sqrt(2), 30))
    assert(math.abs(q.toDouble - math.sqrt(2)) < 1e-15)
    assert(_Surd.exactOf(rat(1, 3), 30).contains(rat(1, 3)))
  }

  "the widening reader" should "read a surd as the real number it is" in {
    sqrt(2) match
      case _Number(x) => assert(math.abs(x - math.sqrt(2)) < 1e-15)
      case other      => fail(s"$other")
    assert(_Rational.containsExact(sqrt(2)))
  }

  "display" should "write the surd as a reader does, and :save in ASCII" in {
    def shown(v: _Value): String = v match
      case s: _Surd => s.display(5)
      case other    => other.toString
    assert(shown(sqrt(13)) == "√13")
    assert(shown(sqrt(1, 2)) == "√2/2")
    assert(shown(get(_Surd.negate(sqrt(5)))) == "-√5")
    assert(shown(surd(rat(3), rat(2), 13)) == "(3 + 2√13)")
    assert(shown(surd(rat(1, 3), rat(-2, 3), 5)) == "(1/3 - 2√5/3)")
    sqrt(1, 2) match
      case s: _Surd => assert(s.exact == "(1/2 * 2^(1/2))")
      case other    => fail(s"$other")
  }


/** `10^k` as a spire rational, for the agreement bound. */
private object SpireRationalHelper:
  def tenTo(k: Int): spire.math.Rational = spire.math.Rational(BigInt(10)).pow(k)
