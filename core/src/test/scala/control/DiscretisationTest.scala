package it.grypho.scala.leonardo
package control

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0009: the remaining discretisation methods. Every expected value is the textbook formula
 *  for the method, worked by hand, never the implementation's output. */
class DiscretisationTest extends AnyFlatSpec:

  private val env = new Environment()
  private val (s, z) = (_Variable("s"), _Variable("z"))

  private def parse(src: String): _Expression =
    val r = Parser.parse(src)
    assert(r.successful, s"parse failed for \"$src\": $r")
    r.get

  private def discretised(g: String, ts: Double, m: DiscretisationMethod): _Expression =
    c2d(parse(g), s, z, ts, m).getOrElse(fail(s"$g by $m declined"))

  /** `G_d` at a real point `z0`. */
  private def at(gd: _Expression, z0: Double): Double =
    gd.eval(env.withBinding("z", _Number(z0))) match
      case Right(_Number(d)) => d
      case other             => fail(s"$gd at $z0: $other")

  /** `G` at a complex point, through the complex closure. */
  private def complexAt(g: _Expression, v: String, re: Double, im: Double): (Double, Double) =
    g.eval(env.withBinding(v, _Complex.of(re, im))) match
      case Right(c: _Complex) => (c.re, c.im)
      case Right(_Number(d))  => (d, 0.0)
      case other              => fail(s"$g at $re + ${im}i: $other")

  private def close(a: Double, b: Double, tol: Double = 1e-9): Boolean = math.abs(a - b) <= tol * math.max(1.0, math.abs(b))

  private def singlePole(gd: _Expression): Double = poles(gd, z) match
    case Some(Vector(_Number(p))) => p
    case other                    => fail(s"$gd: poles $other")

  "forward Euler" should "substitute s = (z - 1)/Ts" in {
    // 1/(s + 1) at Ts = 0.1: 0.1/(z - 0.9), DC gain 1.
    val gd = discretised("1/(s + 1)", 0.1, ForwardEuler)
    assert(close(singlePole(gd), 0.9) && close(at(gd, 1.0), 1.0) && close(at(gd, 2.0), 0.1 / 1.1))
  }

  it should "map a stable plant to an unstable one when Ts is too long (the documented caveat)" in {
    // pole -30, Ts = 0.1: z = 1 - 3 = -2, outside the unit circle.
    val gd = discretised("1/(s + 30)", 0.1, ForwardEuler)
    assert(close(singlePole(gd), -2.0))
    assert(isStableDiscrete(gd, z).contains(false))
  }

  "backward Euler" should "substitute s = (z - 1)/(Ts z)" in {
    // 1/(s + 1) at Ts = 0.1: 0.1 z/(1.1 z - 1), pole 1/1.1, DC gain 1.
    val gd = discretised("1/(s + 1)", 0.1, BackwardEuler)
    assert(close(singlePole(gd), 1 / 1.1) && close(at(gd, 1.0), 1.0) && close(at(gd, 2.0), 0.2 / 1.2))
  }

  "impulse invariance" should "be Ts times the z-transform of the sampled impulse response" in {
    // h(t) = e^-t: G_d = Ts z/(z - e^-Ts).
    val ts = 0.1
    val gd = discretised("1/(s + 1)", ts, ImpulseInvariant)
    assert(close(singlePole(gd), math.exp(-ts)))
    assert(close(at(gd, 2.0), ts * 2 / (2 - math.exp(-ts))))
  }

  it should "decline a plant with direct feedthrough" in {
    assert(c2d(parse("(s + 2)/(s + 1)"), s, z, 0.1, ImpulseInvariant).isEmpty)
  }

  "first-order hold" should "be (z - 1)^2/(Ts z) times the z-transform of the sampled ramp response" in {
    // Ramp response of 1/(s + 1) is t - 1 + e^-t, so G_d = 1 - (z - 1)/Ts + (z - 1)^2/(Ts (z - e^-Ts)).
    val ts = 0.2
    val gd = discretised("1/(s + 1)", ts, Foh)
    val expected = (zz: Double) => 1 - (zz - 1) / ts + (zz - 1) * (zz - 1) / (ts * (zz - math.exp(-ts)))
    for zz <- Seq(2.0, -0.5, 3.7) do assert(close(at(gd, zz), expected(zz)), s"z = $zz")
    assert(close(at(gd, 1.0), 1.0), "FOH keeps the DC gain")
  }

  "matched pole-zero" should "map the pole by e^(sTs) and match the DC gain" in {
    val ts = 0.1
    val gd = discretised("1/(s + 1)", ts, MatchedPoleZero)
    assert(close(singlePole(gd), math.exp(-ts)) && close(at(gd, 1.0), 1.0))
    assert(close(at(gd, 2.0), (1 - math.exp(-ts)) / (2 - math.exp(-ts))))
  }

  it should "put all but one zero at infinity at z = -1, and match the low-frequency gain of an integrator" in {
    // 1/(s(s + 1)): poles 1 and e^-T, one zero at -1, and lim (z - 1) G_d(z)/Ts = lim s G(s) = 1.
    val ts = 0.1
    val gd = discretised("1/(s*(s + 1))", ts, MatchedPoleZero)
    assert(zeros(gd, z).exists(_.collect { case _Number(q) => q }.exists(q => close(q, -1.0))), gd)
    val near = 1 + 1e-7
    assert(close(at(gd, near) * (near - 1) / ts, 1.0, 1e-5))
  }

  it should "map a complex pair to a real conjugate pair" in {
    // 1/(s^2 + 2s + 5): poles -1 +- 2i -> e^(-T)(cos 2T +- i sin 2T); denominator
    // z^2 - 2 e^-T cos(2T) z + e^-2T.
    val ts = 0.1
    val gd = discretised("1/(s^2 + 2*s + 5)", ts, MatchedPoleZero)
    poles(gd, z) match
      case Some(ps) =>
        val parts = ps.flatMap(_Complex.parts)
        assert(parts.size == 2)
        assert(parts.forall((re, im) => close(re, math.exp(-ts) * math.cos(2 * ts)) && close(math.abs(im), math.exp(-ts) * math.sin(2 * ts))))
      case None => fail(s"$gd: no poles")
    assert(close(at(gd, 1.0), 1.0 / 5), "DC gain matched")
  }

  "prewarped Tustin" should "match the continuous frequency response exactly at the prewarp frequency" in {
    val (ts, w) = (0.5, 1.0)
    val g  = parse("1/(s + 1)")
    val gd = c2dPrewarped(g, s, z, ts, w).getOrElse(fail("declined"))
    val (cr, ci) = complexAt(g, "s", 0.0, w)
    val (dr, di) = complexAt(gd, "z", math.cos(w * ts), math.sin(w * ts))
    assert(close(cr, dr) && close(ci, di), s"($cr, $ci) vs ($dr, $di)")
    // Plain Tustin does not match there, which is the reason to prewarp.
    val (pr, pi) = complexAt(discretised("1/(s + 1)", ts, Tustin), "z", math.cos(w * ts), math.sin(w * ts))
    assert(math.abs(pr - cr) + math.abs(pi - ci) > 1e-3)
    assert(c2dPrewarped(g, s, z, ts, 10.0).isEmpty, "w Ts / 2 past pi/2 has no prewarp")
  }

  "d2c" should "invert forward Euler, backward Euler and prewarped Tustin exactly" in {
    val g = parse("(s + 3)/(s^2 + 2*s + 5)")
    for m <- Seq(ForwardEuler, BackwardEuler, Tustin) do
      val back = c2d(g, s, z, 0.1, m).flatMap(d2c(_, z, s, 0.1, m)).getOrElse(fail(s"$m declined"))
      for s0 <- Seq(0.3, 1.7) do
        val (a, b) = (g.eval(env.withBinding("s", _Number(s0))), back.eval(env.withBinding("s", _Number(s0))))
        (a, b) match
          case (Right(_Number(x)), Right(_Number(y))) => assert(close(x, y), s"$m at $s0: $x vs $y")
          case other                                  => fail(s"$m: $other")
    val back = c2dPrewarped(g, s, z, 0.1, 2.0).flatMap(d2cPrewarped(_, z, s, 0.1, 2.0)).getOrElse(fail("prewarp"))
    (g.eval(env.withBinding("s", _Number(0.4))), back.eval(env.withBinding("s", _Number(0.4)))) match
      case (Right(_Number(x)), Right(_Number(y))) => assert(close(x, y))
      case other                                  => fail(s"$other")
    for m <- Seq(Zoh, Foh, ImpulseInvariant, MatchedPoleZero) do
      assert(d2c(parse("1/(z - 0.5)"), z, s, 0.1, m).isEmpty, s"$m has no closed-form inverse")
  }

  "the c2d production" should "read every method word, a prewarp frequency after tustin only, and round-trip" in {
    for (word, m) <- Seq("zoh" -> Zoh, "tustin" -> Tustin, "foh" -> Foh, "impulse" -> ImpulseInvariant,
                         "matched" -> MatchedPoleZero, "forward" -> ForwardEuler, "backward" -> BackwardEuler) do
      parse(s"c2d(1/(s + 1), s, z, 0.1, $word)") match
        case c: _C2d => assert(c.method == m && c.prewarp.isEmpty, word)
        case other   => fail(s"$word: $other")
    val e = parse("c2d(1/(s + 1), s, z, 0.5, tustin, 1)")
    assert(parse(e.toString) == e)
    assert(!Parser.parse("c2d(1/(s + 1), s, z, 0.5, zoh, 1)").successful, "a prewarp frequency belongs to tustin")
    for w <- Seq("foh", "matched", "forward", "backward") do
      assert(!Parser.ReservedWords.contains(w) && parse(w) == _Variable(w), s"$w is a slot word")
    e.eval(env) match
      case Left(_: _C2d) => fail("the prewarped call declined")
      case _             => ()
  }
