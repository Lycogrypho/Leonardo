package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** F_0008: interval arithmetic. The property that matters is CONTAINMENT: every point of the
 *  input interval maps into the output interval. It is checked on random intervals and random
 *  points for every operation and function, on top of the hand cases. */
class IntervalTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def value(s: String, exact: Option[Int] = None): Either[_Expression, _Value] = parse(s, exact).eval(env)

  /** The endpoints of an interval result, read as Doubles. */
  private def bounds(s: String, exact: Option[Int] = None): (Double, Double) = value(s, exact) match
    case Right(i: _Interval) => (i.loDouble, i.hiDouble)
    case other               => fail(s"$s: not an interval: $other")

  private def interval(lo: Double, hi: Double): _Interval = _Interval.of(_Number(lo), _Number(hi)) match
    case Some(i: _Interval) => i
    case other              => fail(s"[$lo, $hi]: $other")

  // ── the value ──

  "an interval literal" should "evaluate to an interval, collapse a point, and refuse lo > hi" in {
    assert(bounds("[1, 2]") == (1.0, 2.0))
    assert(value("[1, 1]") == Right(_Number(1.0)), "a point is a number")
    assert(value("[2, 1]").isLeft, "lo > hi is not an interval")
    assert(value("[1, x]").isLeft, "a symbolic endpoint waits")
    assert(bounds("[1, x]".replace("x", "3")) == (1.0, 3.0))
  }

  it should "NOT be read as a number by the widening extractor (Decision A)" in {
    assert(_Number.unapply(interval(1, 2)).isEmpty)
  }

  it should "round-trip through the parser, and display outward" in {
    val e = parse("[1, 2.5]")
    assert(parse(e.toString) == e)
    // 1/3 at five decimals: the lower end rounds DOWN, the upper end UP.
    interval(1.0 / 3, 2.0 / 3).display(5) match
      case s => assert(s == "[0.33333, 0.66667]", s)
    interval(-2.0 / 3, -1.0 / 3).display(5) match
      case s => assert(s == "[-0.66667, -0.33333]", s)
  }

  it should "be exact in exact mode" in {
    value("[1/3, 1/2] + 1/6", Some(30)) match
      case Right(i: _Interval) =>
        assert(i.isExact)
        assert(_Rational.of(1, 2).contains(i.lo) && _Rational.of(2, 3).contains(i.hi), i)
      case other => fail(s"$other")
    value("[1/3, 1/2]", Some(30)) match
      case Right(i) => assert(_Rational.containsExact(i), "script grouping sees inside it")
      case other    => fail(s"$other")
  }

  // ── field arithmetic ──

  "interval arithmetic" should "give the textbook results" in {
    assert(bounds("[1, 2] + [3, 4]") == (4.0, 6.0))
    assert(bounds("[1, 2] - [1, 2]") == (-1.0, 1.0), "x - x is [-1, 1]: correct interval arithmetic, not a bug")
    assert(bounds("[-1, 2] * [3, 4]") == (-4.0, 8.0))
    assert(bounds("[1, 2] / [4, 8]") == (0.125, 0.5))
    assert(bounds("2 * [1, 3]") == (2.0, 6.0))
    assert(bounds("[1, 3] - 1") == (0.0, 2.0))
  }

  it should "decline a divisor that straddles zero" in {
    assert(value("[1, 2] / [-1, 1]").isLeft)
    assert(value("1 / [0, 1]").isLeft)
  }

  it should "square through zero correctly: [-2, 1]^2 = [0, 4], [-2, 1]^3 = [-8, 1]" in {
    assert(bounds("[-2, 1]^2") == (0.0, 4.0))
    assert(bounds("[-2, 1]^3") == (-8.0, 1.0))
    assert(bounds("[2, 4]^-1") == (0.25, 0.5))
    assert(bounds("[1, 4]^0.5") == (1.0, 2.0))
    assert(value("[-1, 4]^0.5").isLeft, "a fractional power of a negative part is not real")
  }

  it should "round outward, so the real result is inside even where Double rounds" in {
    // 0.1 + 0.2 is 0.30000000000000004 in Double; the true sum of the two Doubles the literals
    // hold is just above 0.3. An interval sum must still contain it on both sides.
    val (lo, hi) = bounds("[0.1, 0.3] + 0.2")
    val exactLo = new java.math.BigDecimal(0.1).add(new java.math.BigDecimal(0.2))
    val exactHi = new java.math.BigDecimal(0.3).add(new java.math.BigDecimal(0.2))
    assert(new java.math.BigDecimal(lo).compareTo(exactLo) <= 0, s"$lo above $exactLo")
    assert(new java.math.BigDecimal(hi).compareTo(exactHi) >= 0, s"$hi below $exactHi")
  }

  // ── elementary functions ──

  "interval functions" should "find the extrema inside the interval, not only at the ends" in {
    val (slo, shi) = bounds("sin([0, pi])")
    assert(shi == 1.0 && slo <= 1e-15 && slo >= -1e-15, (slo, shi))
    val (clo, chi) = bounds("cos([-1, 1])")
    assert(chi == 1.0 && clo <= math.cos(1) && clo >= math.cos(1) - 1e-15)
    assert(bounds("sin([0, 10])") == (-1.0, 1.0))
    val (hlo, hhi) = bounds("cosh([-1, 2])")
    assert(hlo == 1.0 && hhi >= math.cosh(2))
    assert(bounds("abs([-3, 2])") == (0.0, 3.0))
  }

  it should "leave exactly known values unpadded" in {
    val (lo, _) = bounds("exp([0, 1])")
    assert(lo == 1.0)
    val (l2, _) = bounds("ln([1, 2])")
    assert(l2 == 0.0)
  }

  it should "decline outside a domain or across a pole" in {
    assert(value("ln([-1, 1])").isLeft)
    assert(value("asin([0, 2])").isLeft)
    assert(value("tan([1, 2])").isLeft, "pi/2 lies inside")
    assert(value("Gamma([1, 2])").isLeft, "a function with no interval rule stays symbolic")
  }

  it should "bound an exact interval through spire, as an exact interval" in {
    value("exp([0, 1])", Some(30)) match
      case Right(i: _Interval) =>
        assert(i.isExact)
        assert(i.lo == _Rational(1), "exp(0) is exactly 1")
        // The upper end encloses e at the working precision: above it, and by very little.
        val e30 = BigDecimal("2.718281828459045235360287471352662")
        i.hi match
          case r: _Rational =>
            val hi = BigDecimal(r.num) / BigDecimal(r.den)
            assert(hi >= e30 && hi - e30 < BigDecimal("1e-25"), hi)
          case other => fail(s"$other")
      case other => fail(s"$other")
  }

  "containment" should "hold for every operation and function on random intervals" in {
    val rnd = new Random(8)
    def draw(): (Double, Double) =
      val a = rnd.nextDouble() * 8 - 4
      val b = a + rnd.nextDouble() * 3
      (a, b)
    def inside(lo: Double, hi: Double): Double = lo + rnd.nextDouble() * (hi - lo)
    val unary: Seq[(String, Double => Double, ((Double, Double)) => Boolean)] = Seq(
      ("exp", math.exp, _ => true), ("ln", math.log, (l, _) => l > 0), ("sin", math.sin, _ => true),
      ("cos", math.cos, _ => true), ("atan", math.atan, _ => true), ("sinh", math.sinh, _ => true),
      ("cosh", math.cosh, _ => true), ("tanh", math.tanh, _ => true), ("abs", math.abs, _ => true),
      ("asin", math.asin, (l, h) => l >= -1 && h <= 1), ("acos", math.acos, (l, h) => l >= -1 && h <= 1))
    for trial <- 1 to 300 do
      val (a, b) = draw()
      val (c, d) = draw()
      val x = interval(a, b)
      val y = interval(c, d)
      def check(name: String, got: Either[_Expression, _Value], truth: Double): Unit = got match
        case Right(i: _Interval)   => assert(i.loDouble <= truth && truth <= i.hiDouble, s"$name: $truth outside $i")
        case Right(_Number(v))      => assert(v == truth, s"$name: point $v vs $truth")
        case other                  => fail(s"$name declined: $other")
      val (p, q) = (inside(a, b), inside(c, d))
      check(s"[$a,$b] + [$c,$d]", Sum(x, y).eval(env), p + q)
      check(s"[$a,$b] * [$c,$d]", Product(x, y).eval(env), p * q)
      if c > 0 || d < 0 then check(s"[$a,$b] / [$c,$d]", Ratio(x, y).eval(env), p / q)
      check(s"[$a,$b]^3", Power(x, _Number(3)).eval(env), p * p * p)
      check(s"[$a,$b]^2", Power(x, _Number(2)).eval(env), p * p)
      for (name, f, ok) <- unary if ok((a, b)) do
        val node = Parser.parse(s"$name(t)").get
        check(s"$name([$a,$b])", node.eval(env.withBinding("t", x)), f(p))
  }

  // ── comparisons ──

  "a comparison" should "decide when it holds for every pair of points, and say unknown otherwise" in {
    assert(value("[1, 2] < [3, 4]") == Right(_Bool(true)))
    assert(value("[3, 4] < [1, 2]") == Right(_Bool(false)))
    assert(value("[1, 3] < [2, 4]") == Right(_Truth.Unknown))
    assert(value("[3, 4] > 2") == Right(_Bool(true)))
    assert(value("[1, 2] <= [2, 3]") == Right(_Bool(true)), "the touching end satisfies <=")
    assert(value("[1, 2] < [2, 3]") == Right(_Truth.Unknown), "but not <: both could be 2")
    assert(value("[1, 2] = [5, 6]") == Right(_Bool(false)))
    assert(value("[1, 2] = [1, 2]") == Right(_Truth.Unknown), "two values in the same range need not be equal")
    assert(value("[1, 2] != [5, 6]") == Right(_Bool(true)))
  }
