package it.grypho.scala.leonardo
package optimize

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0066 (MPC T3-02): constrained nonlinear programming, `nlp`, by SQP and by an augmented
 *  Lagrangian, each certified by the KKT residual. */
class NlpTest extends AnyFlatSpec:

  private val env = new Environment()
  private val methods = Seq("sqp", "auglag")

  private def parse(s: String): _Expression =
    val r = Parser.parse(s)
    if r.successful then r.get else fail(s"$s did not parse: $r")

  private def solved(s: String, at: Environment = env): NlpResult = parse(s) match
    case n: _Nlp => n.detailed(at).fold(why => fail(s"$s declined: $why"), identity)
    case other   => fail(s"$s parsed as $other")

  private def failure(s: String): NlpFailure = parse(s) match
    case n: _Nlp => n.detailed(env).fold(identity, r => fail(s"$s solved: $r"))
    case other   => fail(s"$s parsed as $other")

  private def near(a: Seq[Double], b: Seq[Double], tol: Double = 1e-6): Boolean =
    a.size == b.size && a.zip(b).forall((p, q) => math.abs(p - q) <= tol * math.max(1.0, math.abs(q)))

  "nlp" should "give T3-02's acceptance (-1, -1) for min x + y on x² + y² ≤ 2, by both methods" in {
    for m <- methods do
      val r = solved(s"nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 - 2, 0, $m)")
      assert(near(r.x, Seq(-1.0, -1.0)), s"$m: ${r.x}")
      // ∇f + μ∇g = (1, 1) + μ(−2, −2) = 0: μ = 1/2, and it is non-negative.
      assert(near(r.inequalityMultipliers, Seq(0.5)), s"$m: ${r.inequalityMultipliers}")
      assert(r.kktResidual <= 0.5e-5, s"$m: ${r.kktResidual}")
    parse("nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 - 2, 0, sqp)").eval(env) match
      case Right(c: _MatrixValue) => assert(c.rows == 2 && c.cols == 1 && near(c.toVector, Seq(-1.0, -1.0)))
      case other                  => fail(s"not a column: $other")
  }

  it should "read a constraint written as a relation, `<=` or `>=`" in {
    for m <- methods do
      assert(near(solved(s"nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 <= 2, 0, $m)").x, Seq(-1.0, -1.0)), m)
      assert(near(solved(s"nlp(x + y, [[x], [y]], [[1], [0]], 2 >= x^2 + y^2, 0, $m)").x, Seq(-1.0, -1.0)), m)
  }

  it should "agree with qp on a convex quadratic program" in {
    // qp's acceptance problem, written as an objective: the optimum is (2/3, 4/3).
    val f = "0.5*x^2 - x*y + y^2 - 2*x - 6*y"
    val g = "[[x + y - 2], [-x + 2*y - 2], [2*x + y - 3]]"
    for m <- methods do
      assert(near(solved(s"nlp($f, [[x], [y]], [[0], [0]], $g, 0, 0, inf, $m)").x, Seq(2.0 / 3, 4.0 / 3)), m)
  }

  it should "find lagrange's exact point under an equality, with its multiplier" in {
    // min x² + 2y² on x + y = 3: 2x + λ = 0 and 4y + λ = 0 give (2, 1), λ = −4.
    for m <- methods; h <- Seq("x + y - 3", "x + y = 3") do
      val r = solved(s"nlp(x^2 + 2*y^2, [[x], [y]], [[0], [0]], 0, $h, $m)")
      assert(near(r.x, Seq(2.0, 1.0)), s"$m, $h: ${r.x}")
      assert(near(r.equalityMultipliers, Seq(-4.0), 1e-5), s"$m, $h: ${r.equalityMultipliers}")
  }

  it should "hold a bound: Rosenbrock with x ≤ 0.5 ends at (0.5, 0.25)" in {
    for m <- methods do
      val r = solved(s"nlp((1 - x)^2 + 100*(y - x^2)^2, [[x], [y]], [[-1.2], [1]], 0, 0, -inf, [[0.5], [inf]], $m)")
      assert(near(r.x, Seq(0.5, 0.25)), s"$m: ${r.x}")
      assert(r.bounded == Vector(0) && r.active.isEmpty, s"$m: x is held by its bound, nothing else binds")
  }

  it should "mix inequalities, an equality and bounds" in {
    // min (x - 2)² + (y - 1)² + z² on x + y + z = 2, x ≤ 1.2, z ≥ 0. Every constraint binds with a
    // positive multiplier: x = 1.2, then y - 1 = z on y + z = 0.8 would make z negative, so z = 0,
    // y = 0.8; λ = 0.4, μ = 1.2 and the bound's 0.4 solve the stationarity equations.
    val p = "nlp((x - 2)^2 + (y - 1)^2 + z^2, [[x], [y], [z]], [[0], [0], [1]], x - 1.2, x + y + z - 2, " +
            "[[-inf], [-inf], [0]], inf, METHOD)"
    for m <- methods do
      val r = solved(p.replace("METHOD", m))
      assert(near(r.x, Seq(1.2, 0.8, 0.0)), s"$m: ${r.x}")
      assert(near(r.inequalityMultipliers, Seq(1.2), 1e-5) && near(r.equalityMultipliers, Seq(0.4), 1e-5), s"$m: $r")
  }

  it should "answer the same point when the objective is rescaled (the units rule)" in {
    for m <- methods; k <- Seq("1e-6", "1", "1e6") do
      assert(near(solved(s"nlp($k*(x + y), [[x], [y]], [[1], [0]], x^2 + y^2 - 2, 0, $m)").x, Seq(-1.0, -1.0)), s"$m at $k")
  }

  it should "not let a session binding of a variable's name fold the problem" in {
    val bound = env.withBinding("x", _Number(5)).withBinding("y", _Number(7))
    for m <- methods do
      assert(near(solved(s"nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 - 2, 0, $m)", bound).x, Seq(-1.0, -1.0)), m)
  }

  it should "decline contradictory constraints as infeasible" in {
    // x ≤ -1 and x ≥ 1.
    assert(failure("nlp(x^2, [[x]], 0, [[x + 1], [1 - x]], 0, sqp)").isInstanceOf[NlpFailure.Infeasible])
    assert(failure("nlp(x^2, [[x]], 0, [[x + 1], [1 - x]], 0, auglag)").isInstanceOf[NlpFailure.Infeasible])
  }

  it should "decline an objective that is not real at the start, and malformed input" in {
    assert(failure("nlp(ln(x), [[x]], -1, 0, 0, sqp)") == NlpFailure.NotEvaluable)
    assert(failure("nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 < 2, 0, sqp)").isInstanceOf[NlpFailure.InvalidInput],
           "a strict inequality is refused, not relaxed")
    assert(failure("nlp(x + y, [[x], [y]], [[1], [0], [2]], 0, 0, sqp)").isInstanceOf[NlpFailure.InvalidInput])
    assert(failure("nlp(x + a, [[x]], 0, x^2 - 1, 0, sqp)").isInstanceOf[NlpFailure.InvalidInput],
           "a free name that is not a variable")
    assert(failure("nlp(x, [[x], [x]], [[0], [0]], 0, 0, sqp)").isInstanceOf[NlpFailure.InvalidInput])
  }

  "the nlp production" should "round-trip, reserve nlp and leave the method words free" in {
    for src <- Seq("nlp(x + y, [[x], [y]], x0, g, h, sqp)", "nlp(f, [[x]], 0, 0, 0, -1, 1, auglag)") do
      val e = parse(src)
      assert(parse(e.toString) == e, src)
    assert(Parser.ReservedWords.contains("nlp"))
    assert(!Parser.ReservedWords.contains("sqp") && !Parser.ReservedWords.contains("auglag"))
    assert(parse("sqp + auglag").freeVars == Set("sqp", "auglag"))
  }

  it should "stay as written when declined" in {
    val e = parse("nlp(x^2, [[x]], 0, [[x + 1], [1 - x]], 0, sqp)")
    assert(e.eval(env) == Left(e))
  }
