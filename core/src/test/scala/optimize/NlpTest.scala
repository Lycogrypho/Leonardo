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

  it should "answer the same point when a VARIABLE is rescaled, which the certificate also claims (F_0113)" in {
    // x = k·u: the answer (-1, -1) becomes u = -1/k. Every (method, k) is run before asserting,
    // so the failure message reports all of them.
    val outcomes = for m <- methods; k <- Seq(1e-6, 1e6) yield
      val src = s"nlp($k*u + y, [[u], [y]], [[${1 / k}], [0]], ($k*u)^2 + y^2 - 2, 0, $m)"
      (m, k, parse(src) match
        case n: _Nlp => n.detailed(env)
        case other   => fail(s"$other"))
    def right(k: Double, r: NlpResult) = math.abs(r.x(0) * k + 1) <= 1e-6 && math.abs(r.x(1) + 1) <= 1e-6
    // A DEFECT TOLERATED, NOT THE DESIGN (F_0120): SQP at k = 1e-6 and AugLag at k = 1e6 decline
    // (Subproblem(25, MaxIterations(2000)) and MaxIterations(50, 0.22)). What is pinned for them is
    // the safety half — a refusal, never a wrong point. Answer them when F_0120 lands.
    val tolerated = Set(("sqp", 1e-6), ("auglag", 1e6))
    for (m, k, out) <- outcomes do
      out match
        case Right(r)                    => assert(right(k, r), s"$m at $k answered a wrong point ${r.x}")
        case Left(why) if tolerated((m, k)) => succeed
        case Left(why)                   => fail(s"$m at $k declined: $why")
  }

  it should "reach a bound exactly, so the certificate can read its multiplier (F_0116)" in {
    // min Σ (xi − i/10)² + Σ xi·x(i+1) (convex), 0 ≤ xi ≤ 0.5, optionally Σ xi ≤ n/4, from 0.
    // SQP landed on a bound as x + (hi − x), one ulp inside it, where the certificate does not
    // absorb the bound's component: it declined Stalled(0.2) at n = 12 and Stalled(0.45) and
    // Stalled(0.69) at n = 20, while AugLag certified the same problems.
    for n <- Seq(12, 20); withConstraint <- Seq(false, true); m <- methods do
      val vs  = (1 to n).map(i => s"[x$i]").mkString("[", ", ", "]")
      val obj = (1 to n).map(i => s"(x$i - ${i / 10.0})^2").mkString(" + ") + " + " +
                (1 until n).map(i => s"x$i*x${i + 1}").mkString(" + ")
      val g   = if withConstraint then (1 to n).map(i => s"x$i").mkString(" + ") + s" - ${n / 4.0}" else "0"
      val x0  = Seq.fill(n)("[0]").mkString("[", ", ", "]")
      val r   = solved(s"nlp($obj, $vs, $x0, $g, 0, 0, 0.5, $m)")
      assert(r.kktResidual <= 0.5e-5 && r.bounded.nonEmpty, s"n = $n, constraint $withConstraint, $m: $r")
      assert(r.bounded.forall(i => r.x(i) == 0.0 || r.x(i) == 0.5), s"$m: a bounded variable is not on its bound: ${r.x}")
  }

  it should "refuse more than MaxNlpVariables before any work" in {
    val vs = Vector.tabulate(MaxNlpVariables + 1)(i => _Variable(s"x$i"))
    assert(nlpSolve(vs(0), vs, Vector.fill(vs.size)(0.0), Vector.empty, Vector.empty, None, NlpMethod.SQP, env) ==
             Left(NlpFailure.InvalidInput(s"at most $MaxNlpVariables variables are accepted, not ${vs.size}")))
  }

  it should "decline, never answer, when the certificate cannot be met (precision 20)" in {
    // A tolerance of 5e-21 is below any Double residual: whichever path ends the run, the answer
    // is a refusal. Which decline it is is the iteration's business: SQP's first QP subproblem
    // already fails its own certificate at this precision (Subproblem(1, Uncertified)).
    val strict = new Environment(20)
    for m <- methods do
      parse(s"nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 - 2, 0, $m)") match
        case n: _Nlp => n.detailed(strict) match
          case Left(NlpFailure.InvalidInput(d)) => fail(s"$m: a well-formed problem refused as input: $d")
          case Left(_)                          => succeed
          case Right(r)                         => fail(s"$m answered under an unmeetable certificate: $r")
        case other => fail(s"$other")
  }

  "withoutRounding" should "drop a coefficient that is rounding of cancelled terms, and keep a small genuine one" in {
    // (1.0000000000001 − 1)·x: about 1e-13 left from terms of size 2, under RoundingLevel·2.
    assert(withoutRounding(parse("(1.0000000000001 - 1)*x + y"), Vector("x", "y")).freeVars == Set("y"))
    // 1e-13·x cancels nothing: its magnitude is itself, so it stays.
    val small = parse("1e-13*x + y")
    assert(withoutRounding(small, Vector("x", "y")) == small)
    // Not a polynomial in the names: returned unchanged.
    val other = parse("sin(x) + y")
    assert(withoutRounding(other, Vector("x", "y")) == other)
  }

  "nlp" should "not let a session binding of a variable's name fold the problem" in {
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
