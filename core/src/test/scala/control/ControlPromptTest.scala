package it.grypho.scala.leonardo
package control

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0092: the control analysis functions at the prompt — `tfpoles`/`tfzeros` (Decision A),
 *  `isStable`/`isStableDiscrete` (Decision B), `stateSpace` and `c2d` — each a thin node over
 *  the library function, so every expectation is the library's own answer. */
class ControlPromptTest extends AnyFlatSpec:

  private val env = new Environment()
  private val s   = _Variable("s")
  private val z   = _Variable("z")

  private def parse(src: String): _Expression =
    val r = Parser.parse(src)
    assert(r.successful, s"parse failed for \"$src\": $r")
    r.get

  private def run(src: String, e: Environment = env) = parse(src).eval(e)

  /** The real parts of a `1×n` row of roots, sorted; complex roots by their real part. */
  private def roots(r: Either[_Expression, _Value]): Vector[(Double, Double)] = r match
    case Left(m: _Matrix) if m.rows == 1 => m.elems.map {
      case c: _Complex => _Complex.parts(c).getOrElse(fail(s"not finite: $c"))
      case _Number(d)  => (d, 0.0)
      case other       => fail(s"not a root: $other")
    }.sortBy(p => (p._1, p._2))
    case other => fail(s"expected a row of roots, got $other")

  private def close(a: Double, b: Double) = math.abs(a - b) < 1e-9

  "tfpoles" should "give the poles as a row, the eigen shape" in
  {
    val ps = roots(run("tfpoles(1/((s + 1)*(s + 3)), s)"))
    assert(ps.size == 2 && close(ps(0)._1, -3) && close(ps(1)._1, -1), ps)
  }

  it should "keep a conjugate pair complex" in
  {
    val ps = roots(run("tfpoles(1/(s^2 + 1), s)"))
    assert(ps.size == 2 && ps.forall(p => close(p._1, 0) && close(math.abs(p._2), 1)), ps)
  }

  it should "answer false for provably none, the stationary/singularities convention" in
  {
    assert(run("tfpoles(s + 1, s)") == Right(_Bool(false)))
    assert(run("tfzeros(1/(s + 1), s)") == Right(_Bool(false)))
  }

  it should "agree with the library, including after cancellation" in
  {
    // feedback's uncancelled form would report spurious poles at 0 and -2.
    val g = feedback(parse("1/(s*(s+2))"), _Number(1), s)
    val viaPrompt = roots(_TfPoles(g, s).eval(env))
    val viaLibrary = poles(g, s).getOrElse(fail("no poles")).map {
      case _Number(d) => (d, 0.0); case c: _Complex => _Complex.parts(c).getOrElse((0.0, 0.0)); case v => fail(s"$v")
    }.sortBy(p => (p._1, p._2))
    assert(viaPrompt.size == viaLibrary.size)
    viaPrompt.zip(viaLibrary).foreach((a, b) => assert(close(a._1, b._1) && close(a._2, b._2), s"$a vs $b"))
  }

  it should "read a bound parameter, but never the frequency variable's own binding" in
  {
    val ps = roots(run("tfpoles(1/(s + a), s)", env.withBinding("a", _Number(2))))
    assert(ps.size == 1 && close(ps(0)._1, -2), ps)
    val withS = roots(run("tfpoles(1/(s + 1), s)", env.withBinding("s", _Number(3))))
    assert(withS.size == 1 && close(withS(0)._1, -1), withS)
  }

  "tfzeros" should "give the numerator's roots" in
  {
    val zs = roots(run("tfzeros((s + 2)/((s + 1)*(s + 3)), s)"))
    assert(zs.size == 1 && close(zs(0)._1, -2), zs)
  }

  "tfpoles and the stability predicates" should "stay as written for a non-rational G or a free coefficient, and say why" in
  {
    for src <- Seq("tfpoles(exp(-2*s)/(s + 1), s)", "tfzeros(exp(-2*s)/(s + 1), s)",
                   "isStable(1/(s^2 + a*s + 1), s)", "isStableDiscrete(1/(z - a), z)") do
      val e = parse(src)
      assert(e.eval(env) == Left(e), src)
    assert(_TfPoles(parse("exp(-2*s)/(s + 1)"), s).declineReason(env).isDefined)
  }

  "isStable" should "decide from the poles, strictly" in
  {
    assert(run("isStable(1/(s^2 + 3*s + 2), s)") == Right(_Bool(true)))
    assert(run("isStable(1/(s - 1), s)") == Right(_Bool(false)))
    assert(run("isStable(1/(s^2 + 1), s)") == Right(_Bool(false)), "marginal is not stable")
  }

  "isStableDiscrete" should "test the unit circle, not the half-plane" in
  {
    assert(run("isStableDiscrete(1/(z - 0.5), z)") == Right(_Bool(true)))
    assert(run("isStableDiscrete(1/(z - 1.5), z)") == Right(_Bool(false)))
    // The same G read as continuous is unstable: the predicates must not be interchangeable.
    assert(run("isStable(1/(z - 0.5), z)") == Right(_Bool(false)))
  }

  it should "check F_0060's acceptance at the prompt: arx2tf's pole is 0.8" in
  {
    val ps = roots(run("tfpoles(arx2tf([[0.8]], [[0.5]], 1, z), z)"))
    assert(ps.size == 1 && close(ps(0)._1, 0.8), ps)
    assert(run("isStableDiscrete(arx2tf([[0.8]], [[0.5]], 1, z), z)") == Right(_Bool(true)))
  }

  "stateSpace" should "build the 1x4 row the rest of control reads" in
  {
    parse("stateSpace([[0, 1], [-2, -3]], [[0], [1]], [[1, 0]], [[0]])") match
      case m: _Matrix => assert(m.rows == 1 && m.cols == 4)
      case other      => fail(s"unexpected: $other")
  }

  "c2d" should "discretise exactly as the library does, the method always named" in
  {
    val g = parse("1/(s + 1)")
    for (word, method) <- Seq("tustin" -> Tustin, "zoh" -> Zoh) do
      val viaPrompt  = run(s"c2d(1/(s + 1), s, z, 0.1, $word)")
      val viaLibrary = c2d(g, s, z, 0.1, method).getOrElse(fail(s"$word declined"))
      val at = env.withBinding("z", _Number(2.0))
      (viaPrompt.toExpression.eval(at), viaLibrary.eval(at)) match
        case (Right(_Number(a)), Right(_Number(b))) => assert(close(a, b), s"$word: $a vs $b")
        case other                                  => fail(s"$word: $other")
  }

  it should "decline a sample period that is not a positive number" in
  {
    for src <- Seq("c2d(1/(s + 1), s, z, 0, tustin)", "c2d(1/(s + 1), s, z, -0.1, zoh)",
                   "c2d(1/(s + 1), s, z, T, tustin)") do
      val e = parse(src)
      assert(e.eval(env) == Left(e), src)
    assert(_C2d(parse("1/(s+1)"), s, z, _Number(0), Tustin).declineReason(env).isDefined)
  }

  "the new productions" should "round-trip, reserve their names, and leave the method words free" in
  {
    for src <- Seq("tfpoles(G, s)", "tfzeros(G, s)", "isStable(G, s)", "isStableDiscrete(G, z)",
                   "c2d(G, s, z, 0.1, zoh)", "c2d(G, s, z, 0.1, tustin)") do
      val e = parse(src)
      assert(parse(e.toString) == e, s"$src -> $e")
    for w <- Seq("tfpoles", "tfzeros", "isStable", "isStableDiscrete", "stateSpace", "c2d") do
      assert(Parser.ReservedWords.contains(w), w)
    for w <- Seq("zoh", "tustin") do
      assert(!Parser.ReservedWords.contains(w), s"$w is a slot word")
      assert(parse(w) == _Variable(w))
  }
