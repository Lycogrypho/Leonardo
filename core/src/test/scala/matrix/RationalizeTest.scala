package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0170: `rationalize(x[, digits])` brings a decimal back into the exact tier when asked. */
class RationalizeTest extends AnyFlatSpec:

  private val env = new Environment()
  private def eval(s: String, exactMode: Boolean = false) =
    Parser.parse(s, Option.when(exactMode)(30)).get.eval(env)
  private def q(n: Int, d: Int): _Rational = _Rational(n).divide(_Rational(d)).getOrElse(fail("no rational"))

  "rationalize" should "turn a decimal into the rational it is closest to" in {
    assert(eval("rationalize(0.5)") == Right(q(1, 2)))
    // 1/3 computed in floating point: the dyadic 0.3333333333333333 comes back as one third
    assert(eval("rationalize(1/3)") == Right(q(1, 3)))
  }

  it should "bound the denominator by 10^digits" in {
    // the best fraction with a denominator up to 100 to 3.14159
    assert(eval("rationalize(3.14159, 2)") == Right(q(311, 99)))
  }

  it should "make a dense matrix an exact one, and keep a symbolic expression's shape" in {
    eval("rationalize([[0.5, 0.25], [0.75, 1]])") match
      case Left(m: _Matrix) => assert(m.elems == Vector(q(1, 2), q(1, 4), q(3, 4), _Rational(1)), m)
      case other            => fail(s"$other")
    val r = eval("rationalize(0.5*x + 0.25)").fold(identity, identity)
    assert(_Rational.containsExact(r) && !r.toString.contains("0.5") && r.freeVars == Set("x"), r)
  }

  it should "leave an exact value as it is" in {
    assert(eval("rationalize(1/3)", exactMode = true) == Right(q(1, 3)))
  }

  it should "decline, as a whole, what it cannot make exact" in {
    for s <- List("rationalize(1 + i)", "rationalize(inf)", "rationalize([1, 2])", "rationalize(x + i)",
                  "rationalize(0.5, 16)", "rationalize(0.5, 0)", "rationalize(0.5, 2.5)") do
      Parser.parse(s).get match
        case n: _Rationalize =>
          assert(n.eval(env).left.exists(_.isInstanceOf[_Rationalize]), s)
          assert(n.declineReason(env).nonEmpty, s)
        case other => fail(s"$s parsed as $other")
  }

  "looksRational" should "accept a decimal near a small fraction and refuse an irrational" in {
    assert(looksRational(2.0 / 3.0) && looksRational(0.0) && looksRational(-1.75))
    assert(!looksRational(math.sqrt(13)) && !looksRational(math.Pi))
  }

  it should "hold its verdict at every magnitude (the units rule)" in {
    // A fixed denominator bound makes almost every decimal above 1 "look rational": 4 ulps grow
    // with the value while 10⁶'s fractions approximate anything to ~10⁻¹².  cond() of an LA
    // lesson's matrix, 2451.94087..., was offered as exact.
    for k <- Seq(1e-6, 1.0, 1e3, 1e6, 1e9) do
      assert(!looksRational(k * math.Pi), s"$k · π")
      assert(!looksRational(k * math.sqrt(2)), s"$k · √2")
    assert(!looksRational(math.sqrt(6012014.0)))
    assert(looksRational(2451.25) && looksRational(6012014.01) && looksRational(1e9 + 0.5) && looksRational(1e-6 / 3))
  }

  "rationalizeDigits" should "name the digits at which rationalize gives the small fractions" in {
    // 3 ulps below 2 (the ulp halves below 2): at 15 digits rationalize keeps it as
    // (2e15 - 1)/1e15, which still prints 2.0
    val nearTwo = 2.0 - 3 * Math.ulp(1.5)
    assert(_Rational.fromApproximation(nearTwo, 15).exists(_ != _Rational(2)))
    rationalizeDigits(List(nearTwo, 61.0 / 70, 0.25)) match
      case None => fail("no digits found")
      case Some(k) =>
        assert(k == 2, k)
        assert(_Rational.fromApproximation(nearTwo, k).contains(_Rational(2)))
        assert(_Rational.fromApproximation(61.0 / 70, k) == _Rational(61).divide(_Rational(70)))
    assert(rationalizeDigits(List(0.5, math.Pi)).isEmpty)
  }
