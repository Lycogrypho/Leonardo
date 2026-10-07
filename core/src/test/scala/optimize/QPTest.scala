package it.grypho.scala.leonardo
package optimize

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** F_0053: `qp`, the Goldfarb–Idnani dual active-set solver. */
class QPTest extends AnyFlatSpec:

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

  private def near(a: Seq[Double], b: Seq[Double], tol: Double = 1e-9): Boolean =
    a.size == b.size && a.zip(b).forall((p, q) => math.abs(p - q) <= tol * math.max(1.0, math.abs(q)))

  private def mv(rows: Seq[Seq[Double]]): _MatrixValue =
    _MatrixValue(rows.size, rows.head.size, rows.flatten.toArray)

  private val acceptance =
    "qp([[1, -1], [-1, 2]], [[-2], [-6]], [[1, 1], [-1, 2], [2, 1]], [[2], [2], [3]], 0, inf)"

  "qp" should "solve the requirement's acceptance problem" in {
    val r = solved(acceptance)
    assert(near(r.z, Seq(2.0 / 3, 4.0 / 3)), r.z)
    assert(math.abs(r.objective - (-74.0 / 9)) < 1e-9, r.objective)
    assert(r.active.toSet == Set(QpConstraint.Row(0), QpConstraint.Row(1)))
    assert(r.kktResidual <= 0.5e-5)
    parse(acceptance).eval(env) match
      case Right(m: _MatrixValue) => assert(m.rows == 2 && m.cols == 1 && near(m.toVector, r.z))
      case other                  => fail(s"not a column: $other")
  }

  it should "equal inv(H) * -f when nothing constrains it" in {
    // inv([[1, -1], [-1, 2]]) = [[2, 1], [1, 1]], so -inv(H) f = (10, 8).
    val r = solved("qp([[1, -1], [-1, 2]], [[-2], [-6]], 0, 0, -inf, inf)")
    assert(near(r.z, Seq(10.0, 8.0)) && r.active.isEmpty && r.iterations == 0)
  }

  it should "honour equality constraints and bounds together" in {
    // min ½|z|² on z1 + z2 = 1: (0.5, 0.5); with z1 ≤ 0.2 the bound binds: (0.2, 0.8).
    assert(near(solved("qp(eye(2), [[0], [0]], 0, 0, [[1, 1]], [[1]], -inf, inf)").z, Seq(0.5, 0.5)))
    val r = solved("qp(eye(2), [[0], [0]], 0, 0, [[1, 1]], [[1]], -inf, [[0.2], [inf]])")
    assert(near(r.z, Seq(0.2, 0.8)) && r.active == Vector(QpConstraint.Upper(0)))
  }

  it should "decline lb > ub, contradictory rows and contradictory equalities as infeasible" in {
    assert(failure("qp(eye(2), [[0], [0]], 0, 0, [[1], [0]], [[0], [0]])") == QpFailure.Infeasible)
    // z1 ≤ -1 and -z1 ≤ -1: found by the iteration, not by a pre-check.
    assert(failure("qp(eye(2), [[0], [0]], [[1, 0], [-1, 0]], [[-1], [-1]], -inf, inf)") == QpFailure.Infeasible)
    // z1 + z2 = 1 and 2z1 + 2z2 = 3: dependent rows that disagree.
    assert(failure("qp(eye(2), [[0], [0]], 0, 0, [[1, 1], [2, 2]], [[1], [3]], -inf, inf)") == QpFailure.Infeasible)
  }

  it should "decline an indefinite H as not convex and a singular one as not strictly convex" in {
    assert(failure("qp([[1, 0], [0, -1]], [[0], [0]], 0, 0, -inf, inf)") == QpFailure.NotConvex)
    assert(failure("qp([[1, 0], [0, 0]], [[0], [0]], 0, 0, -inf, inf)") == QpFailure.NotStrictlyConvex)
  }

  it should "survive degenerate constraints: a duplicate row, a scaled copy, a redundant one through the optimum" in {
    val degenerate =
      "qp([[1, -1], [-1, 2]], [[-2], [-6]], [[1, 1], [-1, 2], [2, 1], [1, 1], [3, 3], [0, 3]], " +
      "[[2], [2], [3], [2], [6], [4]], 0, inf)"
    val r = solved(degenerate)
    assert(near(r.z, Seq(2.0 / 3, 4.0 / 3)), r.z)
    assert(math.abs(r.objective - (-74.0 / 9)) < 1e-9)
  }

  it should "keep a dependent but consistent equality" in {
    assert(near(solved("qp(eye(2), [[0], [0]], 0, 0, [[1, 1], [2, 2]], [[1], [2]], -inf, inf)").z, Seq(0.5, 0.5)))
  }

  it should "find the known optimum of random problems built to have it" in {
    // z*, an active set S and multipliers λ > 0 on S are drawn first; then f = -Hz* - Aᵀλ and
    // b makes S exactly active and the rest strictly slack, so z* IS the unique minimiser.
    val rnd = new Random(53)
    for trial <- 1 to 25 do
      val n = 2 + rnd.nextInt(9)
      val m = n + rnd.nextInt(2 * n)
      val g = Vector.fill(n, n)(rnd.nextGaussian())
      val h = Vector.tabulate(n, n)((i, j) => (0 until n).map(k => g(k)(i) * g(k)(j)).sum + (if i == j then 0.5 else 0.0))
      val a = Vector.fill(m, n)(rnd.nextGaussian())
      val zs = Vector.fill(n)(rnd.nextGaussian())
      val active = rnd.shuffle((0 until m).toList).take(rnd.nextInt(n + 1)).toSet
      val lambda = Vector.tabulate(m)(i => if active(i) then 0.1 + rnd.nextDouble() else 0.0)
      val b = Vector.tabulate(m)(i => a(i).zip(zs).map(_ * _).sum + (if active(i) then 0.0 else 0.1 + rnd.nextDouble()))
      val f = Vector.tabulate(n)(i => -(0 until n).map(j => h(i)(j) * zs(j)).sum - (0 until m).map(k => a(k)(i) * lambda(k)).sum)
      qpSolve(mv(h), f, Some((mv(a), b)), None, Vector.fill(n)(-Inf), Vector.fill(n)(Inf), Set.empty, env) match
        case Left(why) => fail(s"trial $trial (n=$n, m=$m) declined: $why")
        case Right(r)  =>
          assert(near(r.z, zs, 1e-7), s"trial $trial: ${r.z} vs $zs")
          assert(r.active.toSet == active.map(QpConstraint.Row(_)), s"trial $trial active set")
  }

  it should "answer the same point when the problem is rescaled (the units rule)" in {
    val base = qpSolve(mv(Seq(Seq(1, -1), Seq(-1, 2))), Vector(-2, -6), Some((mv(Seq(Seq(1, 1), Seq(-1, 2), Seq(2, 1))), Vector(2, 2, 3))),
                       None, Vector(0, 0), Vector(Inf, Inf), Set.empty, env)
    val big  = qpSolve(mv(Seq(Seq(1e6, -1e6), Seq(-1e6, 2e6))), Vector(-2e6, -6e6),
                       Some((mv(Seq(Seq(1e-3, 1e-3), Seq(-1e-3, 2e-3), Seq(2e-3, 1e-3))), Vector(2e-3, 2e-3, 3e-3))),
                       None, Vector(0, 0), Vector(Inf, Inf), Set.empty, env)
    (base, big) match
      case (Right(p), Right(q)) => assert(near(p.z, q.z) && p.active.toSet == q.active.toSet)
      case other                => fail(s"$other")
  }

  it should "reach a perturbed problem in fewer iterations from the previous active set" in {
    val h = mv(Seq(Seq(1, -1), Seq(-1, 2)))
    val g = Some((mv(Seq(Seq(1, 1), Seq(-1, 2), Seq(2, 1))), Vector(2.0, 2, 3)))
    def solve(f: Vector[Double], warm: Set[QpConstraint]) =
      qpSolve(h, f, g, None, Vector(0.0, 0), Vector(Inf, Inf), warm, env).fold(w => fail(s"$w"), identity)
    val first = solve(Vector(-2, -6), Set.empty)
    val cold  = solve(Vector(-2.1, -5.9), Set.empty)
    val warm  = solve(Vector(-2.1, -5.9), first.active.toSet)
    assert(near(cold.z, warm.z))
    assert(warm.iterations < cold.iterations, s"warm ${warm.iterations}, cold ${cold.iterations}")
  }

  it should "start from a warm set that is wrong and still answer correctly" in {
    val h = mv(Seq(Seq(1, -1), Seq(-1, 2)))
    val g = Some((mv(Seq(Seq(1, 1), Seq(-1, 2), Seq(2, 1))), Vector(2.0, 2, 3)))
    val wrong = Set[QpConstraint](QpConstraint.Row(2), QpConstraint.Lower(0), QpConstraint.Lower(1))
    qpSolve(h, Vector(-2, -6), g, None, Vector(0.0, 0), Vector(Inf, Inf), wrong, env) match
      case Right(r) => assert(near(r.z, Seq(2.0 / 3, 4.0 / 3)))
      case Left(w)  => fail(s"$w")
  }

  it should "say which operand is wrong, and stay symbolic on free names" in {
    failure("qp([[1, 2, 3]], [[0]], 0, 0, -inf, inf)") match
      case QpFailure.InvalidInput(d) => assert(d.contains("square"))
      case other                     => fail(s"$other")
    failure("qp(eye(2), [[0]], 0, 0, -inf, inf)") match
      case QpFailure.InvalidInput(d) => assert(d.contains("f must"))
      case other                     => fail(s"$other")
    failure("qp([[1, 2], [0, 1]], [[0], [0]], 0, 0, -inf, inf)") match
      case QpFailure.InvalidInput(d) => assert(d.contains("symmetric"))
      case other                     => fail(s"$other")
    assert(parse("qp(eye(2), [[a], [0]], 0, 0, -inf, inf)").eval(env).isLeft)
  }

  it should "accept exact input, demoting it" in {
    val exact = Parser.parse("qp([[1, -1], [-1, 2]], [[-2], [-6]], 0, 0, -inf, inf)", Some(30)).get
    exact.eval(env) match
      case Right(m: _MatrixValue) => assert(near(m.toVector, Seq(10.0, 8.0)))
      case other                  => fail(s"$other")
  }

  it should "round-trip both forms and reserve its name" in {
    for s <- Seq(acceptance, "qp(eye(2), [[0], [0]], 0, 0, [[1, 1]], [[1]], -inf, inf)") do
      val e = parse(s)
      assert(parse(e.toString) == e, e.toString)
    assert(!Parser.parse("qp + 1").successful)
  }
