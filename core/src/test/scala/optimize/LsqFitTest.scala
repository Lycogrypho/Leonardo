package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.{abs, exp, sin, sqrt}


/** F_0063 — nonlinear least squares (MPC T2-05).
 *
 *  The certificate is recomputed here independently of the solver, from the closed-form
 *  Jacobian of `a·e^(b·t)`, so an answer is checked against the contract rather than against
 *  the code that produced it.
 */
class LsqFitTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private val (a, b, t) = (_Variable("a"), _Variable("b"), _Variable("t"))
  private val ts = Vector.tabulate(21)(_ * 0.5)                 // 0 .. 10
  private val ys = ts.map(x => 2 * exp(-0.5 * x))               // exact data, a = 2, b = -0.5
  private val inf = Double.PositiveInfinity

  private def fit(model: String, p0: Vector[Double], tv: Vector[Double] = ts, yv: Vector[Double] = ys,
                  vars: Vector[_Variable] = Vector(a, b),
                  bounds: Option[(Vector[Double], Vector[Double])] = None): Either[LsqFailure, LsqResult] =
    curveFit(parse(model), t, tv, yv, vars, p0, bounds, env)

  private def answer(r: Either[LsqFailure, LsqResult]): LsqResult =
    r.fold(why => fail(s"declined: $why"), identity)

  /** The certificate for `a·e^(b·t)` on `ys`: every free column's cosine with the residual within
   *  the precision's tolerance, or the residual at rounding level. */
  private def certified(p: Vector[Double], free: Set[Int] = Set(0, 1)): Boolean =
    val r    = ts.zip(ys).map((x, y) => y - p(0) * exp(p(1) * x))
    val cols = Vector(ts.map(x => -exp(p(1) * x)), ts.map(x => -p(0) * x * exp(p(1) * x)))
    val rn   = sqrt(r.map(v => v * v).sum)
    def norm(v: Vector[Double]) = sqrt(v.map(x => x * x).sum)
    rn <= 1e-12 * norm(ys) || free.forall(j => abs(cols(j).zip(r).map(_ * _).sum) <= 5e-6 * norm(cols(j)) * rn)

  "curveFit" should "recover a = 2, b = -0.5 from exact data within 1e-8 (T2-05's acceptance)" in
  {
    val r = answer(fit("a*exp(b*t)", Vector(1.0, 0.0)))
    assert(abs(r.p(0) - 2) <= 1e-8 && abs(r.p(1) + 0.5) <= 1e-8, s"answered ${r.p}")
  }

  it should "converge or decline from far starts, and every answer passes the certificate" in
  {
    for p0 <- List(Vector(100.0, 5.0), Vector(-3.0, 2.0), Vector(0.1, -3.0)) do
      fit("a*exp(b*t)", p0) match
        case Right(r) => assert(certified(r.p), s"from $p0 answered an uncertified ${r.p}")
        case Left(_)  => succeed
  }

  it should "give the same fit whatever the units of a parameter" in
  {
    val r = answer(fit("(1e6*a)*exp(b*t)", Vector(1e-6, 0.0)))
    assert(abs(r.p(0) - 2e-6) <= 1e-8 * 2e-6 && abs(r.p(1) + 0.5) <= 1e-8, s"answered ${r.p}")
    val s = answer(fit("a*exp((1e-3*b)*t)", Vector(1.0, 0.0)))
    assert(abs(s.p(0) - 2) <= 1e-8 && abs(s.p(1) + 500) <= 1e-5, s"answered ${s.p}")
    // Moré's scaling makes the iteration itself invariant, not only its end point: the same path,
    // so the same number of steps.  Plain damping (D = I) also converges here, by another path.
    val u = answer(fit("a*exp(b*t)", Vector(1.0, 0.0)))
    assert(r.iterations == u.iterations && s.iterations == u.iterations,
           s"iterations ${u.iterations} unscaled, ${r.iterations} and ${s.iterations} rescaled")
  }

  it should "equal linear regression on a linear residual, covariance included" in
  {
    val yv = ts.map(x => 1 + 2 * x + 0.1 * sin(7 * x))
    val r  = answer(fit("c0 + c1*t", Vector(0.0, 0.0), yv = yv, vars = Vector(_Variable("c0"), _Variable("c1"))))
    // The normal equations, written out independently for two columns (1, t).
    val (m, st, stt) = (ts.size.toDouble, ts.sum, ts.map(x => x * x).sum)
    val (sy, sty)    = (yv.sum, ts.zip(yv).map(_ * _).sum)
    val det          = m * stt - st * st
    val beta         = Vector((stt * sy - st * sty) / det, (m * sty - st * sy) / det)
    assert(abs(r.p(0) - beta(0)) <= 1e-9 && abs(r.p(1) - beta(1)) <= 1e-9, s"answered ${r.p}, regress $beta")
    val res    = ts.zip(yv).map((x, y) => y - beta(0) - beta(1) * x)
    val sigma2 = res.map(v => v * v).sum / (m - 2)
    val inv    = Vector(stt / det, -st / det, -st / det, m / det).map(_ * sigma2)
    r.covariance match
      case None    => fail("no covariance for m > n with no active bound")
      case Some(c) => assert(c.toVector.zip(inv).forall((u, v) => abs(u - v) <= 1e-9 * abs(v)), s"$c vs $inv")
  }

  it should "decline a fit whose parameters are not identifiable (Decision B)" in
  {
    assert(fit("(a + b)*t", Vector(1.0, 1.0)) == Left(LsqFailure.NotIdentifiable(1, 2)))
  }

  it should "hold a bound, certify the free parameters and give no covariance (Decision D)" in
  {
    val r = answer(fit("a*exp(b*t)", Vector(1.0, 0.0), bounds = Some((Vector(-inf, -inf), Vector(1.5, inf)))))
    assert(r.p(0) == 1.5 && r.active == Vector(0), s"answered ${r.p}, active ${r.active}")
    assert(certified(r.p, free = Set(1)) && r.covariance.isEmpty)
  }

  it should "give no covariance when there are no residual degrees of freedom" in
  {
    val r = answer(fit("a*exp(b*t)", Vector(1.0, 0.0), tv = ts.take(2), yv = ys.take(2)))
    assert(abs(r.p(0) - 2) <= 1e-8 && r.covariance.isEmpty)
  }

  it should "refuse malformed input" in
  {
    assert(fit("a*exp(b*t)", Vector(1.0)).left.exists(_.isInstanceOf[LsqFailure.InvalidInput]))
    assert(fit("a*exp(b*t)", Vector(1.0, 0.0), vars = Vector(a, a)).left.exists(_.isInstanceOf[LsqFailure.InvalidInput]))
  }

  it should "decline a problem past the work cap before any work" in
  {
    val m  = (MaxLsqWork / 4 + 1).toInt
    val tv = Vector.tabulate(m)(_.toDouble)
    assert(fit("a*exp(b*t)", Vector(1.0, 0.0), tv = tv, yv = tv) == Left(LsqFailure.TooLarge(m, 2)))
  }

  // --- the residual form and the language ---

  private val data = env.withBinding("T", _MatrixValue(21, 1, ts.toArray))
                        .withBinding("Y", _MatrixValue(21, 1, ys.toArray))

  "lsqfit" should "fit a residual column built with the tabulate idiom" in
  {
    val r = parse("transpose(tabulate(at(Y, k, 1) - a*exp(b*at(T, k, 1)), k, 1, 21))")
    val p = answer(lsqfit(r, Vector(a, b), Vector(1.0, 0.0), None, data)).p
    assert(abs(p(0) - 2) <= 1e-8 && abs(p(1) + 0.5) <= 1e-8, s"answered $p")
  }

  it should "not be disturbed by a session binding of a parameter's name" in
  {
    val r = parse("transpose(tabulate(at(Y, k, 1) - a*exp(b*at(T, k, 1)), k, 1, 21))")
    val p = answer(lsqfit(r, Vector(a, b), Vector(1.0, 0.0), None, data.withBinding("a", _Number(7)))).p
    assert(abs(p(0) - 2) <= 1e-8, s"answered $p")
  }

  it should "answer the column at the prompt, and the row [[p, C]] with cov" in
  {
    parse("lsqfit(a*exp(b*t), t, T, Y, [[a], [b]], [[1], [0]])").eval(data) match
      case Right(m: _MatrixValue) => assert(m.rows == 2 && m.cols == 1 && abs(m.toVector.head - 2) <= 1e-8)
      case other                  => fail(s"no column: $other")
    parse("lsqfit(a*exp(b*t), t, T, Y, [[a], [b]], [[1], [0]], cov)").eval(data) match
      case Left(_Matrix(1, 2, _)) => succeed
      case other                  => fail(s"no [[p, C]] row: $other")
  }

  it should "round-trip both forms through toString and reserve only its name" in
  {
    for s <- List("lsqfit([[y1 - a], [y2 - a*b]], [[a], [b]], [[1], [0]])",
                  "lsqfit([[y1 - a], [y2 - a*b]], [[a], [b]], [[1], [0]], [[0], [-inf]], [[5], [inf]], cov)",
                  "lsqfit(a*exp(b*t), t, T, Y, [[a], [b]], [[1], [0]])",
                  "lsqfit(a*exp(b*t), t, T, Y, [[a], [b]], [[1], [0]], 0, 10, cov)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"round-trip failed for $s: ${e.toString}")
    assert(Parser.ReservedWords.contains("lsqfit"))
    assert(!Parser.ReservedWords.contains("cov") && parse("cov + 1").freeVars == Set("cov"))
  }
