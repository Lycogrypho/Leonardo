package it.grypho.scala.leonardo
package statistics

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0057 — recursive least squares, `rls(theta, P, phi, y, lambda)` → `[[theta', P']]`.
 *
 *  The acceptance values are MPC requirement T1-06's: agreement with batch `regress` at `λ = 1`,
 *  tracking a drifting parameter at `λ = 0.98`, and `P` staying symmetric over `10⁴` updates.
 */
class RlsTest extends AnyFlatSpec:

  private val env = new Environment()

  private def dense(rows: Int, cols: Int, xs: Double*): _MatrixValue = _MatrixValue(rows, cols, xs.toArray)

  private def step(theta: _MatrixValue, p: _MatrixValue, phi: _MatrixValue, y: Double,
                   lambda: Double): (_MatrixValue, _MatrixValue) =
    rlsUpdate(theta, p, phi, _Number(y), _Number(lambda), env) match
      case Some((t: _MatrixValue, q: _MatrixValue)) => (t, q)
      case other                                    => fail(s"rls declined: $other")

  /** Runs RLS over every row of a design, from theta = 0 and P = p0·I. */
  private def run(rows: Vector[Vector[Double]], ys: Vector[Double], lambda: Double,
                  p0: Double = 1e6): (_MatrixValue, _MatrixValue) =
    val n = rows.head.size
    rows.zip(ys).foldLeft((dense(n, 1, Seq.fill(n)(0.0)*), _MatrixValue.identity(n).scale(p0))) {
      case ((t, p), (r, y)) => step(t, p, dense(n, 1, r*), y, lambda)
    }

  // A deterministic design: y = 1.5 + 2x − 0.3x² plus a bounded pseudo-noise.
  private val xs   = Vector.tabulate(60)(k => -3 + 0.1 * k)
  private val rows = xs.map(x => Vector(1.0, x, x * x))
  private val ys   = xs.zipWithIndex.map((x, k) => 1.5 + 2 * x - 0.3 * x * x + 0.05 * math.sin(7.0 * k))

  "rls" should "agree with batch regress at lambda = 1, from P0 = 1e6·I (acceptance)" in
  {
    val (theta, _) = run(rows, ys, 1.0)
    val design     = _MatrixValue(rows.size, 3, rows.flatten.toArray)
    val batch      = leastSquares(design, _MatrixValue(ys.size, 1, ys.toArray)).getOrElse(fail("regress failed"))
    for i <- 0 until 3 do
      assert(math.abs(theta(i, 0) - batch(i, 0)) < 1e-6, s"coefficient $i: ${theta(i, 0)} vs ${batch(i, 0)}")
  }

  it should "track a parameter that changes halfway at lambda = 0.98, where lambda = 1 averages" in
  {
    // y = a·x with a = 1 for 200 samples, then a = 3.
    val xs2   = Vector.tabulate(400)(k => math.sin(0.37 * k) + 1.2)
    val ys2   = xs2.zipWithIndex.map((x, k) => (if k < 200 then 1.0 else 3.0) * x)
    val rows2 = xs2.map(Vector(_))
    val forgetting = run(rows2, ys2, 0.98)._1(0, 0)
    val memory     = run(rows2, ys2, 1.0)._1(0, 0)
    // RLS with forgetting IS exponentially weighted least squares: sample k carries weight
    // λ^(N−1−k).  The old regime still weighs 0.98^200 ≈ 0.018 at the end, so the estimate sits
    // about 0.03 short of 3 — that is the method, and the weighted fit is the exact reference.
    val w        = Vector.tabulate(400)(k => math.pow(0.98, 399 - k))
    val weighted = xs2.indices.map(k => w(k) * xs2(k) * ys2(k)).sum / xs2.indices.map(k => w(k) * xs2(k) * xs2(k)).sum
    assert(math.abs(forgetting - weighted) < 1e-6, s"lambda = 0.98: $forgetting vs weighted fit $weighted")
    assert(math.abs(forgetting - 3) < 0.05, s"lambda = 0.98 should have tracked close to 3, got $forgetting")
    val wholeFit = leastSquares(_MatrixValue(400, 1, xs2.toArray), _MatrixValue(400, 1, ys2.toArray))
      .getOrElse(fail("regress failed"))(0, 0)
    assert(math.abs(memory - wholeFit) < 1e-4 && math.abs(memory - 3) > 0.5,
      s"lambda = 1 should be the whole-data fit $wholeFit, got $memory")
  }

  it should "keep P exactly symmetric and positive definite over 10⁴ updates" in
  {
    val many  = Vector.tabulate(10000)(k => Vector(1.0, math.sin(0.01 * k), math.cos(0.013 * k)))
    val ysMany = many.map(r => r(1) - 2 * r(2) + 0.5)
    val (_, p) = run(many, ysMany, 0.995, 100.0)
    for i <- 0 until 3; j <- 0 until 3 do assert(p(i, j) == p(j, i), s"P($i,$j) != P($j,$i)")
    assert(p.cholesky.isDefined, "P lost positive definiteness")
  }

  it should "update several outputs at once, each column as its own single-output update" in
  {
    val theta = dense(2, 2, 0.5, -1, 2, 0)
    val p     = dense(2, 2, 2, 0.5, 0.5, 1)
    val phi   = dense(2, 1, 1, 3)
    rlsUpdate(theta, p, phi, dense(2, 1, 4, -2), _Number(0.9), env) match
      case Some((t: _MatrixValue, q: _MatrixValue)) =>
        for c <- 0 until 2 do
          val (tc, qc) = step(dense(2, 1, theta(0, c), theta(1, c)), p, phi, if c == 0 then 4.0 else -2.0, 0.9)
          assert(math.abs(t(0, c) - tc(0, 0)) < 1e-12 && math.abs(t(1, c) - tc(1, 0)) < 1e-12, s"column $c")
          for i <- 0 until 2; j <- 0 until 2 do assert(math.abs(q(i, j) - qc(i, j)) < 1e-12)
      case other => fail(s"multi-output rls declined: $other")
  }

  it should "stay exact on exact input" in
  {
    // theta = 0, P = 1, phi = 2, y = 3, lambda = 1:  denominator 5, gain 2/5,
    // theta' = 6/5, P' = 1 − (2/5)·2 = 1/5.
    Parser.parse("rls([[0]], [[1]], [[2]], 3, 1)", Some(30)) match
      case Parser.Success(e, _) => e.eval(env) match
        case Left(row: _Matrix) =>
          def cell(m: _Expression): _Rational = m match
            case lit: _Matrix => lit.elems.head match
              case r: _Rational => r
              case other        => fail(s"not exact: $other")
            case other => fail(s"not an exact matrix: $other")
          assert(cell(row(0, 0)) == _Rational(6).divide(_Rational(5)).getOrElse(fail()))
          assert(cell(row(0, 1)) == _Rational(1).divide(_Rational(5)).getOrElse(fail()))
        case other => fail(s"expected [[theta', P']], got $other")
      case other => fail(s"$other")
  }

  private def declines(s: String): Unit =
    Parser.parse(s) match
      case Parser.Success(e, _) => e.eval(env) match
        case Left(_: _Rls) => ()
        case other         => fail(s"\"$s\" should decline, got $other")
      case other => fail(s"$other")

  it should "decline a forgetting factor outside (0, 1], disagreeing shapes, a non-positive denominator, and unknown values" in
  {
    declines("rls([[0]], [[1]], [[2]], 3, 0)")
    declines("rls([[0]], [[1]], [[2]], 3, 1.5)")
    declines("rls([[0], [0]], [[1]], [[2]], 3, 1)")       // P is 1x1 for a 2-parameter theta
    declines("rls([[0]], [[1]], [[2], [1]], 3, 1)")       // phi too long
    declines("rls([[0]], [[-1]], [[2]], 3, 1)")           // lambda + phi'P phi = 1 - 4 < 0
    declines("rls(theta, [[1]], [[2]], 3, 1)")
  }

  it should "round-trip and reserve its name" in
  {
    Parser.parse("rls(t, P, f, y, 0.98)") match
      case Parser.Success(e, _) => Parser.parse(e.toString) match
        case Parser.Success(e2, _) => assert(e2 == e)
        case other                 => fail(s"$other")
      case other => fail(s"$other")
    assert(Parser.ReservedWords.contains("rls"))
  }
