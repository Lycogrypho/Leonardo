package it.grypho.scala.leonardo
package optimize

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** F_0074: `qp` with a positive SEMIdefinite `H`, by the primal active-set method from an
 *  exact LP feasible point; the definite path stays Goldfarb–Idnani. */
class SemidefiniteQPTest extends AnyFlatSpec:

  private val env = new Environment()
  private val Inf = Double.PositiveInfinity

  private def parse(s: String): _Expression =
    val r = Parser.parse(s)
    if r.successful then r.get else fail(s"$s did not parse: $r")

  private def solved(s: String): QpResult = parse(s) match
    case q: _QP => q.detailed(env).fold(why => fail(s"$s declined: $why"), identity)
    case other  => fail(s"$s parsed as $other")

  private def failure(s: String): QpFailure = parse(s) match
    case q: _QP => q.detailed(env).fold(identity, r => fail(s"$s solved: $r"))
    case other  => fail(s"$s parsed as $other")

  private def near(a: Seq[Double], b: Seq[Double], tol: Double = 1e-8): Boolean =
    a.size == b.size && a.zip(b).forall((p, q) => math.abs(p - q) <= tol * math.max(1.0, math.abs(q)))

  private def mv(rows: Seq[Seq[Double]]): _MatrixValue = _MatrixValue(rows.size, rows.head.size, rows.flatten.toArray)

  "qp" should "solve a linear program when H = 0 (the Wyndor problem, (2, 6))" in {
    val r = solved("qp(zeros(2), [[-3], [-5]], [[1, 0], [0, 2], [3, 2]], [[4], [12], [18]], 0, inf)")
    assert(near(r.z, Seq(2.0, 6.0)), r.z)
    assert(math.abs(r.objective + 36) < 1e-8)
  }

  it should "solve a mixed problem whose H has a zero eigenvalue" in {
    // min ½ x² − 2x − y on x + y ≤ 3, x, y ≥ 0: x − 2 + μ = 0 and −1 + μ = 0 give μ = 1, (1, 2).
    val r = solved("qp([[1, 0], [0, 0]], [[-2], [-1]], [[1, 1]], [[3]], 0, inf)")
    assert(near(r.z, Seq(1.0, 2.0)), r.z)
    assert(near(r.multipliers.inequality, Seq(1.0)), r.multipliers)
  }

  it should "report an unbounded direction as Unbounded, and contradictory constraints as Infeasible" in {
    assert(failure("qp([[1, 0], [0, 0]], [[0], [-1]], 0, 0, -inf, inf)") == QpFailure.Unbounded, "y falls freely")
    assert(failure("qp(zeros(1), -1, 0, 0, 0, inf)") == QpFailure.Unbounded, "an LP falling forever")
    assert(failure("qp(zeros(1), 1, [[1], [-1]], [[1], [-2]], -inf, inf)") == QpFailure.Infeasible)
  }

  it should "still decline an indefinite H as not convex" in {
    assert(failure("qp([[1, 0], [0, -1]], [[0], [0]], 0, 0, -inf, inf)") == QpFailure.NotConvex)
  }

  it should "reach the known optimal value of random semidefinite problems built to have one" in {
    // H = GᵀG with G k×n, k < n: semidefinite. z*, an active set S and λ > 0 on S are drawn, f
    // and b are built so z* is a KKT point, hence optimal: the optimal VALUE is f(z*), even where
    // the minimiser is not unique.
    val rnd = new Random(74)
    for trial <- 1 to 20 do
      val n = 2 + rnd.nextInt(5)
      val k = 1 + rnd.nextInt(n - 1)
      val m = n + rnd.nextInt(n)
      val g = Vector.fill(k, n)(rnd.nextGaussian())
      val h = Vector.tabulate(n, n)((i, j) => (0 until k).map(r => g(r)(i) * g(r)(j)).sum)
      val a = Vector.fill(m, n)(rnd.nextGaussian())
      val zs = Vector.fill(n)(rnd.nextGaussian())
      val active = rnd.shuffle((0 until m).toList).take(1 + rnd.nextInt(n)).toSet
      val lambda = Vector.tabulate(m)(i => if active(i) then 0.1 + rnd.nextDouble() else 0.0)
      val b = Vector.tabulate(m)(i => a(i).zip(zs).map(_ * _).sum + (if active(i) then 0.0 else 0.1 + rnd.nextDouble()))
      val f = Vector.tabulate(n)(i => -(0 until n).map(j => h(i)(j) * zs(j)).sum - (0 until m).map(r => a(r)(i) * lambda(r)).sum)
      def value(z: Seq[Double]): Double =
        0.5 * (0 until n).map(i => (0 until n).map(j => z(i) * h(i)(j) * z(j)).sum).sum + f.zip(z).map(_ * _).sum
      qpSolve(mv(h), f, Some((mv(a), b)), None, Vector.fill(n)(-Inf), Vector.fill(n)(Inf), Set.empty, env) match
        case Left(QpFailure.Unbounded) => fail(s"trial $trial: a problem with an optimum reported unbounded")
        case Left(why) => fail(s"trial $trial (n=$n, k=$k, m=$m) declined: $why")
        case Right(r)  =>
          assert(math.abs(value(r.z) - value(zs)) <= 1e-7 * math.max(1.0, math.abs(value(zs))), s"trial $trial: ${value(r.z)} vs ${value(zs)}")
          assert((0 until m).forall(i => a(i).zip(r.z).map(_ * _).sum <= b(i) + 1e-7 * math.max(1.0, math.abs(b(i)))), s"trial $trial infeasible")
  }

  it should "leave the definite path to Goldfarb–Idnani: the acceptance problem is unchanged" in {
    val r = solved("qp([[1, -1], [-1, 2]], [[-2], [-6]], [[1, 1], [-1, 2], [2, 1]], [[2], [2], [3]], 0, inf)")
    assert(near(r.z, Seq(2.0 / 3, 4.0 / 3)))
  }
