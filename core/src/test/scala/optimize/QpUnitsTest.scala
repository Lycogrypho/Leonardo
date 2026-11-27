package it.grypho.scala.leonardo
package optimize

import core.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** F_0377 and F_0378: `qpSolve` reads each variable in its own length (Jacobi, a flat variable's from its rows, the
 *  owner's decision), and its certificate's floors come from a variable's own cost and curvature and from the rows that
 *  bind at the point, never from a row's right-hand side alone.  Every case here was certified wrong before. */
class QpUnitsTest extends AnyFlatSpec:

  private val env  = Environment()
  private val Inf  = Double.PositiveInfinity
  private val free = Vector(-Inf, -Inf)
  private def mv(rows: Seq[Seq[Double]]): _MatrixValue = _MatrixValue(rows.size, rows.head.size, rows.flatten.toArray)
  private val i2 = mv(Seq(Seq(1.0, 0.0), Seq(0.0, 1.0)))

  private def answer(r: Either[QpFailure, QpResult]): QpResult = r match
    case Right(q)  => q
    case Left(why) => fail(s"declined: $why")

  private def near(a: Seq[Double], b: Seq[Double], tol: Double): Boolean =
    a.size == b.size && a.zip(b).forall((x, y) => math.abs(x - y) <= tol * math.max(1.0, math.abs(y)))

  "qp's certificate" should "see a row beside a far row in the same variables (F_0377)" in {
    val r = answer(qpSolve(i2, Vector(-10.0, -10.0), Some((mv(Seq(Seq(1.0, 1.0), Seq(1.0, 1.0))), Vector(1e12, 1.0))),
                           None, free, free.map(-_), Set.empty, env))
    assert(near(r.z, Seq(0.5, 0.5), 1e-9), r.z)
  }

  it should "see a bound beside a far row in the same variable (F_0377)" in {
    val r = answer(qpSolve(i2, Vector(-10.0, -10.0), Some((mv(Seq(Seq(1.0, 0.0))), Vector(1e12))), None,
                           free, Vector(1.0, 1.0), Set.empty, env))
    assert(near(r.z, Seq(1.0, 1.0), 1e-9), r.z)
  }

  it should "not give an uncoupled variable the rounding of a large one (F_0377)" in {
    // f1 = -1e10 gave every variable a floor of 14, and z2 >= 1e-9 was certified at z2 = -0.0
    val r = answer(qpSolve(i2, Vector(-1e10, 0.0), None, None, Vector(-Inf, 1e-9), Vector(Inf, Inf), Set.empty, env))
    assert(r.z(1) >= 1e-9 && near(r.z, Seq(1e10, 1e-9), 1e-9), r.z)
  }

  it should "see a row beside a large equality in a shared variable (F_0377)" in {
    // min 1/2 (z1 - big)^2 + 1/2 (z2 - 10)^2 + 1/2 z3^2, z1 + z3 = big, z2 + z3 <= 1: the optimum is (big + 3, 4, -3)
    for big <- Seq(1e9, 1e12) do
      val r = answer(qpSolve(mv(Seq(Seq(1.0, 0, 0), Seq(0, 1.0, 0), Seq(0, 0, 1.0))), Vector(-big, -10.0, 0.0),
                             Some((mv(Seq(Seq(0.0, 1.0, 1.0))), Vector(1.0))), Some((mv(Seq(Seq(1.0, 0.0, 1.0))), Vector(big))),
                             Vector.fill(3)(-Inf), Vector.fill(3)(Inf), Set.empty, env))
      assert(near(r.z, Seq(big + 3, 4.0, -3.0), 1e-9), s"big $big: ${r.z}")
  }

  it should "see a row beside rows whose normals are tiny, an MPC's late outputs (F_0377)" in {
    // |rhs|/|n| = 2e9 was every variable's size, under which z1 <= 0.1 violated by 9.9 read as 5e-9
    val r = answer(qpSolve(i2, Vector(-10.0, -10.0), Some((mv(Seq(Seq(1e-9, 1e-9), Seq(1.0, 0.0))), Vector(2.0, 0.1))),
                           None, free, free.map(-_), Set.empty, env))
    assert(near(r.z, Seq(0.1, 10.0), 1e-9), r.z)
  }

  it should "not start from a corner far rows call near (F_0377)" in {
    // min (sum z - 1)^2 with rows <= 1e12 and |z| <= 1e12: certified at a corner, objective 0 for -1
    val n = 10
    val a = _MatrixValue(10, n, Array.tabulate(10 * n)(i => if (i / n + i % n) % 3 == 0 then 1.0 else 0.0))
    val r = answer(qpSolve(_MatrixValue(n, n, Array.fill(n * n)(2.0)), Vector.fill(n)(-2.0), Some((a, Vector.fill(10)(1e12))),
                           None, Vector.fill(n)(-1e12), Vector.fill(n)(1e12), Set.empty, env))
    assert(math.abs(r.objective + 1.0) <= 1e-9, r.objective)
  }

  "qp" should "answer a problem whose variables' units differ (F_0378)" in {
    // H0 = [[1, 1/2], [1/2, 1]] in units 1e-4 and 1e4: the optimum is (6666.67, 6.6667e-5), objective -2/3
    val r = answer(qpSolve(mv(Seq(Seq(1e-8, 0.5), Seq(0.5, 1e8))), Vector(-1e-4, -1e4), None, None, free, free.map(-_), Set.empty, env))
    assert(near(r.z, Seq(20000.0 / 3, 2e-4 / 3), 1e-9) && math.abs(r.objective + 2.0 / 3) <= 1e-12, r.z)
  }

  it should "not call a strictly convex separable problem in mixed units Unbounded (F_0378)" in {
    val r = answer(qpSolve(mv(Seq(Seq(1e-6, 0, 0), Seq(0, 1.0, 0), Seq(0, 0, 1e6))), Vector(-1e-3, -1.0, -1e3), None, None,
                           Vector.fill(3)(-Inf), Vector.fill(3)(Inf), Set.empty, env))
    assert(near(r.z, Seq(1000.0, 1.0, 0.001), 1e-9), r.z)
  }

  it should "answer a linear program in mixed units at its optimum (F_0378)" in {
    // the Wyndor problem, (2, 6), with its variables in units 1e-4 and 1e4
    val d = Seq(1e-4, 1e4)
    val a = Seq(Seq(1.0, 0.0), Seq(0.0, 2.0), Seq(3.0, 2.0)).map(row => row.zip(d).map(_ * _))
    val r = answer(qpSolve(mv(Seq(Seq(0.0, 0.0), Seq(0.0, 0.0))), Vector(-3.0 * d(0), -5.0 * d(1)), Some((mv(a), Vector(4.0, 12.0, 18.0))),
                           None, Vector(0.0, 0.0), Vector(Inf, Inf), Set.empty, env))
    assert(near(r.z, Seq(2.0 / d(0), 6.0 / d(1)), 1e-9), r.z)
  }

  it should "answer random convex problems in mixed units at their known optimum, or decline (F_0378)" in {
    // Built with a KKT point, then the variables put in units 1e-4, 1, 1e4: 1178 of 3000 such problems were certified
    // away from their optimum before (the audit's probe); every answer must meet its rows and reach the optimum
    val rnd = new Random(20261126)
    var answered = 0
    for trial <- 0 until 150 do
      val n = 2 + rnd.nextInt(5)
      val semidefinite = trial % 2 == 1
      val rank = if semidefinite then rnd.nextInt(n) else n
      val bm = Array.fill(rank, n)(rnd.nextGaussian())
      val h0 = Array.tabulate(n, n)((i, j) => (0 until rank).map(k => bm(k)(i) * bm(k)(j)).sum + (if !semidefinite && i == j then 0.1 else 0.0))
      val m = 1 + rnd.nextInt(2 * n)
      val a0 = Array.fill(m, n)(if rnd.nextInt(3) == 0 then 0.0 else rnd.nextGaussian())
      val zStar = Array.fill(n)(3 * rnd.nextGaussian())
      val active = rnd.shuffle((0 until m).toList).take(rnd.nextInt(math.min(m, n) + 1)).toSet
      val mu = Array.tabulate(m)(i => if active(i) && rnd.nextInt(4) != 0 then 2 * rnd.nextDouble() else 0.0)
      val b = Vector.tabulate(m)(i => (0 until n).map(j => a0(i)(j) * zStar(j)).sum + (if active(i) then 0.0 else 0.1 + 3 * rnd.nextDouble()))
      val f0 = Array.tabulate(n)(i => -(0 until n).map(j => h0(i)(j) * zStar(j)).sum - (0 until m).map(k => a0(k)(i) * mu(k)).sum)
      val d = Array.tabulate(n)(i => math.pow(10.0, (i % 3 - 1) * 4.0))
      val h = _MatrixValue(n, n, Array.tabulate(n * n)(k => d(k / n) * h0(k / n)(k % n) * d(k % n)))
      val f = Vector.tabulate(n)(i => d(i) * f0(i))
      val a = _MatrixValue(m, n, Array.tabulate(m * n)(k => a0(k / n)(k % n) * d(k % n)))
      val box = Vector.tabulate(n)(i => 50.0 / d(i))
      def objective(z: Seq[Double]) = (0 until n).map(i => f(i) * z(i) + 0.5 * z(i) * (0 until n).map(j => h(i, j) * z(j)).sum).sum
      val want = zStar.indices.map(i => zStar(i) / d(i))
      qpSolve(h, f, Some((a, b)), None, box.map(-_), box, Set.empty, env) match
        case Right(r) =>
          answered += 1
          for k <- 0 until m do
            val lhs   = (0 until n).map(j => a(k, j) * r.z(j)).sum
            val terms = math.abs(b(k)) + (0 until n).map(j => math.abs(a(k, j) * r.z(j))).sum
            assert(lhs - b(k) <= 1e-8 * terms, s"trial $trial row $k violated")
          val scale = (0 until n).map(i => math.abs(f(i) * want(i))).sum + math.abs(objective(want))
          assert(objective(r.z) - objective(want) <= 1e-6 * math.max(scale, 1e-300), s"trial $trial not optimal")
        case Left(_) => ()
    assert(answered >= 140, s"only $answered of 150 answered")
  }
