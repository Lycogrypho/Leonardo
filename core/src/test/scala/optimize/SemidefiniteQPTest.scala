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

  it should "report the exact LP's own decline as FeasibleStart, not as the user's input (F_0106)" in {
    // 41 variables: inside qp's caps (500), past the LP phase's (40).  Since F_0388 a feasible one answers from the
    // smallest point, a definite QP (the minimum is z = 0); only an infeasible one still needs the LP, to prove it
    assert(solved("qp(zeros(41), ones(41, 1), 0, 0, 0, inf)").z.forall(_ == 0.0))
    failure("qp(zeros(41), ones(41, 1), ones(1, 41), [[-1]], 0, inf)") match
      case QpFailure.FeasibleStart(LpFailure.InvalidInput(detail)) => assert(detail.contains(s"$MaxLpVariables"), detail)
      case other => fail(s"expected FeasibleStart(InvalidInput), got $other")
    // The definite path never reaches the LP, so the same size is answered there.
    assert(solved("qp(eye(41), ones(41, 1), 0, 0, 0, inf)").z.forall(_ == 0.0))
  }

  it should "answer at 40 variables with a rank-20 Hessian, whose reduced Hessian needs a full SVD (F_0117)" in {
    // Box bounds only, so the exact LP phase is trivial; the primal active-set method's first face
    // is all of R^40 and its reduced Hessian is H itself, rank 20. Its SVD declined (an unpivoted
    // basis completion), which qp reported as Uncertified(Infinity).
    val rnd = new Random(108)
    val (n, k) = (40, 20)
    val g = Vector.fill(k, n)((rnd.nextInt(5) - 2).toDouble)
    val h = Vector.tabulate(n, n)((i, j) => (0 until k).map(r => g(r)(i) * g(r)(j)).sum)
    val f = Vector.fill(n)((rnd.nextInt(11) - 5).toDouble)
    qpSolve(mv(h), f, None, None, Vector.fill(n)(-5.0), Vector.fill(n)(5.0), Set.empty, env) match
      case Right(r) => assert(r.kktResidual <= 0.5e-5 && r.z.forall(z => z >= -5 && z <= 5), r)
      case Left(why) => fail(s"declined: $why")
  }

  it should "take a warm start without changing the answer, and with no more iterations (F_0113)" in {
    // The mixed problem: its active set is row 0 of A. A warm start from it, from a wrong guess,
    // and from nothing must answer the same point; the right one should not cost more.
    val (h, f) = (mv(Seq(Seq(1, 0), Seq(0, 0))), Vector(-2.0, -1.0))
    val a = Some((mv(Seq(Seq(1, 1))), Vector(3.0)))
    def run(warm: Set[QpConstraint]) =
      qpSolve(h, f, a, None, Vector(0.0, 0.0), Vector(Inf, Inf), warm, env).fold(w => fail(s"$warm: $w"), identity)
    val cold = run(Set.empty)
    val hot  = run(cold.active.toSet)
    val off  = run(Set(QpConstraint.Lower(1)))
    for r <- Seq(hot, off) do assert(near(r.z, cold.z), s"${r.z} vs ${cold.z}")
    assert(hot.iterations <= cold.iterations, s"warm ${hot.iterations} vs cold ${cold.iterations}")
  }

  it should "certify an optimum whose gradient is zero, its curved component reached to the iteration's tolerance (F_0358)" in {
    // min u1^2 over the slabs 2 ± 2e-10 of u1 + u2 and u2 + u3: the iteration stops with u1 ~ 1e-10, and the
    // gradient 2·u1 read against its own terms was 100 % of its size, declined Uncertified(1/3)
    val h = mv(Seq(Seq(2.0, 0.0, 0.0), Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, 0.0)))
    val a = mv(Seq(Seq(1.0, 1.0, 0.0), Seq(-1.0, -1.0, 0.0), Seq(0.0, 1.0, 1.0), Seq(0.0, -1.0, -1.0)))
    val b = Vector(2.0000000002, -1.9999999998, 2.0000000002, -1.9999999998)
    val free = Vector.fill(3)(Double.NegativeInfinity)
    qpSolve(h, Vector(0.0, 0.0, 0.0), Some((a, b)), None, free, free.map(-_), Set.empty, env) match
      case Right(r) => assert(math.abs(r.z(0)) < 1e-8 && math.abs(r.z(0) + r.z(1) - 2.0) < 1e-8, s"${r.z}")
      case Left(why) => fail(s"$why")
  }

  it should "answer a bounded problem under equalities, not take its optimum's rounding for a ray (F_0359)" in {
    // a target-layer level: a rank-one cost (H = 2w·FᵀF, symmetric to an ulp) under three equalities, Unbounded
    // while the ray test read a null direction's share of the vanishing gradient against that gradient's norm
    val hv = Seq(1.650023308830626, -0.49677112621327213, -1.139576269455828, 0.0, 0.0, -0.49677112621327224, 0.1495624640685211,
                 0.34309126650137634, 0.0, 0.0, -1.139576269455828, 0.3430912665013763, 0.7870398357143245, 0.0, 0.0) ++ Seq.fill(10)(0.0)
    val e = Seq(-0.4999021145799545, -0.02229092058112854, -0.01624065263815933, -0.28451368016552536, -0.4380436973469921,
                0.061159784478232386, -0.24381830314178654, -0.042571764907324276, 2.313006328968705, 1.7213222203655203,
                -0.0019294488557940184, 0.06419795774144459, -0.23351152956409393, -0.9260519909771605, 1.8875580262360896)
    val free = Vector.fill(5)(Double.NegativeInfinity)
    val r = qpSolve(_MatrixValue(5, 5, hv.toArray), Vector(1.7400124235137355, -0.5238640730879108, -1.2017265791232032, 0.0, 0.0), None,
                    Some((_MatrixValue(3, 5, e.toArray), Vector(-0.05249067764310096, -0.24176425724620756, -0.08882810900763119))),
                    free, free.map(-_), Set.empty, env)
    assert(r.isRight, s"$r")
  }

  it should "answer an unconstrained rank-one problem, not take its optimum's rounding for a ray (F_0359)" in {
    // min w·(g·u − c)²: at the optimum the gradient is rounding, and a null direction's share of it read as a ray
    val g = Seq(0.50951, 0.55139, -0.13523)
    val h = mv(Seq.tabulate(3)(i => Seq.tabulate(3)(j => 2.0 * g(i) * g(j))))
    val f = Vector.tabulate(3)(i => 2.0 * (-0.8) * g(i))
    val free = Vector.fill(3)(Double.NegativeInfinity)
    qpSolve(h, f, None, None, free, free.map(-_), Set.empty, env) match
      case Right(r) => assert(math.abs(g.indices.map(i => g(i) * r.z(i)).sum - 0.8) < 1e-8, s"${r.z}")
      case Left(why) => fail(s"$why")
  }

  it should "not cross a working-set bound along a face that is flat to rounding (F_0359)" in {
    // min 4.27(x − 1.2043)² under x ≤ −0.0476 and one equality in (x, u1, u2): the face left by the bound
    // and the equality is flat, its basis carrying x only to 4e-17; read against its own rounding that
    // curvature took a step of 3e16 in u and put x at 1.2043, through the bound, and certified it
    val h = mv(Seq(Seq(8.537605828033643, 0.0, 0.0), Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, 0.0)))
    val a = mv(Seq(Seq(-0.2748547184880934, 0.0, 0.0), Seq(0.2748547184880934, 0.0, 0.0)))
    val e = mv(Seq(Seq(-0.601100726743524, -0.6704497241870666, 0.288080976176568)))
    val free = Vector.fill(3)(Double.NegativeInfinity)
    qpSolve(h, Vector(-10.281864520109403, 0.0, 0.0), Some((a, Vector(0.8938755367590219, -0.013092361820549016))),
            Some((e, Vector(0.0780227028635841))), free, free.map(-_), Set.empty, env) match
      case Right(r) =>
        assert(math.abs(r.z(0) + 0.047633753179013194) < 1e-9, s"${r.z}")
        assert(r.z.forall(v => math.abs(v) < 1e3), s"${r.z}")
      case Left(why) => fail(s"$why")
  }

  it should "refuse a point violating a bound, however large the variables the bound does not involve (F_0361)" in {
    // F_0359's crossing point: x at its unconstrained minimum 1.2043, through x <= -0.0476 by 1.25, and the two
    // free variables at 1e16 holding the equality.  Against ||z||inf the violation read as 4.5e-17 and passed.
    val h = mv(Seq(Seq(8.537605828033643, 0.0, 0.0), Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, 0.0)))
    val f = Array(-10.281864520109403, 0.0, 0.0)
    val e = Array(-0.601100726743524, -0.6704497241870666, 0.288080976176568)
    val x = 10.281864520109403 / 8.537605828033643
    val u1 = -1.18209e16
    val z = Array(x, u1, (0.0780227028635841 - e(0) * x - e(1) * u1) / e(2))
    val rows = Vector(QpRow(e, 0.0780227028635841, None),
                      QpRow(Array(0.2748547184880934, 0.0, 0.0), -0.8938755367590219, Some(QpConstraint.Row(0))),
                      QpRow(Array(-0.2748547184880934, 0.0, 0.0), 0.013092361820549016, Some(QpConstraint.Row(1))))
    qpCertificate(QpProblem(3, h, f, rows, 8.537605828033643), z, Vector.empty, Array.emptyDoubleArray, 1, 0.5e-5) match
      case Left(QpFailure.Uncertified(r)) => assert(r > 0.1, s"$r")
      case other                          => fail(s"certified: $other")
  }

  it should "refuse a point that is not stationary, however large a variable H does not touch (F_0361)" in {
    // min (x - 1)^2 + 0*y: x = 1.001 leaves a gradient of 2e-3, which y = 1e16 hid against ||z||inf (2e-19)
    val prob = QpProblem(2, mv(Seq(Seq(2.0, 0.0), Seq(0.0, 0.0))), Array(-2.0, 0.0), Vector.empty, 2.0)
    qpCertificate(prob, Array(1.001, 1e16), Vector.empty, Array.emptyDoubleArray, 1, 0.5e-5) match
      case Left(QpFailure.Uncertified(r)) => assert(r > 1e-4, s"$r")
      case other                          => fail(s"certified: $other")
    // and the optimum itself still passes, the far variable included
    assert(qpCertificate(prob, Array(1.0, 1e16), Vector.empty, Array.emptyDoubleArray, 1, 0.5e-5).isRight)
  }

  it should "answer under a far bound, which sets no variable's size (F_0361, F_0362)" in {
    // min (x - 1)^2 on x + y = 3, |x|, |y| <= 1e6: a bound says how large a variable may be, not how large it is
    // (the problem-wide zScale that read it as one was removed by F_0362), and the answer is (1, 2)
    val r = solved("qp([[2, 0], [0, 0]], [[-2], [0]], 0, 0, [[1, 1]], [[3]], -1e6, 1e6)")
    assert(math.abs(r.z(0) - 1.0) < 1e-9 && math.abs(r.z(1) - 2.0) < 1e-9, s"${r.z}")
  }

  it should "meet every constraint it answers under, each against its own terms, on random semidefinite problems (F_0359)" in {
    // a row's slack is certified against ‖z‖∞ over every variable, so a step that blows up unrelated variables
    // can hide a violated row; this reads each row against its own terms instead
    val rnd = new java.util.Random(359)
    var answered = 0
    for _ <- 0 until 200 do
      val n = 2 + rnd.nextInt(6)
      val k = 1 + rnd.nextInt(n - 1)
      val gm = Seq.fill(k)(Seq.fill(n)(rnd.nextGaussian()))
      val h = mv(Seq.tabulate(n)(i => Seq.tabulate(n)(j => gm.map(r => r(i) * r(j)).sum)))
      val f = Vector.fill(n)(rnd.nextGaussian())
      val m = rnd.nextInt(2 * n + 1)
      val a = Seq.fill(m)(Seq.fill(n)(rnd.nextGaussian()))
      val b = Vector.fill(m)(rnd.nextGaussian())
      val lb = Vector.fill(n)(if rnd.nextBoolean() then -2.0 else Double.NegativeInfinity)
      val ub = Vector.fill(n)(if rnd.nextBoolean() then 2.0 else Double.PositiveInfinity)
      qpSolve(h, f, Option.when(m > 0)((mv(a), b)), None, lb, ub, Set.empty, env).foreach { r =>
        answered += 1
        a.indices.foreach { i =>
          val terms = a(i).indices.map(j => math.abs(a(i)(j) * r.z(j))).sum + math.abs(b(i))
          assert(a(i).indices.map(j => a(i)(j) * r.z(j)).sum - b(i) <= 1e-7 * terms, s"row $i of ${r.z}")
        }
        r.z.indices.foreach(j => assert(r.z(j) >= lb(j) - 1e-9 * (1 + math.abs(lb(j))) && r.z(j) <= ub(j) + 1e-9 * (1 + math.abs(ub(j)))))
      }
    assert(answered > 50, s"only $answered answered")
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

  it should "answer the same point when its variables are rescaled below unit scale (F_0102)" in {
    // The mixed problem above with x = 1e-12·x': its answer (1, 2) becomes (1e-12, 2e-12).
    // Against `1 +` floors every step of that size read as zero.
    val s = 1e-12
    qpSolve(mv(Seq(Seq(1 / (s * s), 0), Seq(0, 0))), Vector(-2 / s, -1 / s), Some((mv(Seq(Seq(1 / s, 1 / s))), Vector(3.0))),
            None, Vector(0.0, 0), Vector(Inf, Inf), Set.empty, env) match
      case Right(r) =>
        assert(r.z.zip(Seq(1 * s, 2 * s)).forall((p, q) => math.abs(p - q) <= 1e-8 * math.abs(q)), r.z)
      case Left(why) => fail(s"declined: $why")
  }

  it should "not stop at the far corner of wide bounds (F_0362)" in {
    // min (z1 + z2 - 1)^2 under |z| <= big: the exact LP's zero-cost vertex is the bounds' corner, where a step of
    // size 1 is below the step test and a multiplier of -2 below the gradient's terms (4e12); (1e12, -1e12) was
    // certified with objective 0 for a minimum of -1
    val h = mv(Seq(Seq(2.0, 2.0), Seq(2.0, 2.0)))
    for big <- Seq(1e6, 1e9, 1e12) do
      qpSolve(h, Vector(-2.0, -2.0), None, None, Vector(-big, -big), Vector(big, big), Set.empty, env) match
        case Right(r) =>
          assert(math.abs(r.z(0) + r.z(1) - 1.0) < 1e-9 && math.abs(r.objective + 1.0) < 1e-9, s"$big: ${r.z}")
        case Left(why) => fail(s"$big: $why")
  }

  it should "refuse a multiplier of the wrong sign at a far point, however large the gradient's terms (F_0362)" in {
    // the far point itself, handed to the certificate: z1 at its upper bound, z2 at its lower one, the lower
    // multiplier -2 against terms of 4e12 -- 5e-13 relative, and well above the rounding of those terms
    val big = 1e12
    val prob = QpProblem(2, mv(Seq(Seq(2.0, 2.0), Seq(2.0, 2.0))), Array(-2.0, -2.0),
                         Vector(QpRow(Array(0.0, 1.0), -big, Some(QpConstraint.Lower(1))),
                                QpRow(Array(-1.0, 0.0), -big, Some(QpConstraint.Upper(0)))), 2.0)
    qpCertificate(prob, Array(big, -big), Vector(0, 1), Array(-2.0, 2.0), 2, 0.5e-5) match
      case Left(QpFailure.Uncertified(r)) => assert(r > 0.5e-5, s"$r")
      case other                          => fail(s"certified: $other")
  }

  it should "leave the definite path to Goldfarb–Idnani: the acceptance problem is unchanged" in {
    val r = solved("qp([[1, -1], [-1, 2]], [[-2], [-6]], [[1, 1], [-1, 2], [2, 1]], [[2], [2], [3]], 0, inf)")
    assert(near(r.z, Seq(2.0 / 3, 4.0 / 3)))
  }

  it should "not answer Infeasible from the dual method when the exact LP finds a point (F_0379)" in {
    // H of rank 2 to rounding, four rows all slack at z = 0: the dual method reported Infeasible at every box
    val h = mv(Seq(Seq(1.2630448895738053, -0.7090994485990942, 0.29046431094391956),
                   Seq(-0.7090994485990942, 0.39810586749780513, -0.16232008154200897),
                   Seq(0.29046431094391956, -0.16232008154200897, 0.2681087069698576)))
    val f = Vector(-14.629057169011205, 8.209976779566023, -4.187513774416011)
    val a = Seq(Seq(-0.7521154674968699, -1.1296354486385696, -0.6930483745380793), Seq(0.0, 0.0, 0.6488587843537224),
                Seq(1.1527546947472311, -0.001291523444851007, -0.021677455864169543), Seq(0.28494652117603686, 0.0, -0.44223713981566154))
    val b = Vector(0.05080598104520062, 3.1487228816683297, 9.852132521064581, 2.85451149919626)
    for box <- Seq(50.0, 1e12, Inf) do
      qpSolve(h, f, Some((mv(a), b)), None, Vector.fill(3)(-box), Vector.fill(3)(box), Set.empty, env) match
        case Right(r) =>
          for (row, k) <- a.zipWithIndex do
            val lhs = row.indices.map(j => row(j) * r.z(j)).sum
            assert(lhs <= b(k) + 1e-9 * (math.abs(b(k)) + row.indices.map(j => math.abs(row(j) * r.z(j))).sum), s"row $k at $box")
        case Left(QpFailure.Infeasible) => fail(s"box $box: Infeasible, though z = 0 is strictly feasible")
        case Left(_)                    => succeed
  }

  it should "answer past 20 variables from the smallest feasible point, a definite QP (F_0388)" in {
    // min (sum z - 1)^2 over |z| <= 1e12: the zero-cost vertex is a corner 1e12 away, and the smallest point was an
    // exact LP over doubled variables, which declined past 20 variables
    for n <- Seq(21, 30, 60) do
      qpSolve(_MatrixValue(n, n, Array.fill(n * n)(2.0)), Vector.fill(n)(-2.0), None, None,
              Vector.fill(n)(-1e12), Vector.fill(n)(1e12), Set.empty, env) match
        case Right(r) => assert(math.abs(r.objective + 1.0) <= 1e-9 && math.abs(r.z.sum - 1.0) <= 1e-9, s"n=$n: ${r.objective}")
        case Left(why) => fail(s"n=$n declined: $why")
    // and an infeasible one is still proved so
    assert(qpSolve(_MatrixValue(2, 2, Array(1.0, 0.0, 0.0, 0.0)), Vector(0.0, -1.0), Some((mv(Seq(Seq(1.0, 1.0), Seq(-1.0, -1.0))), Vector(1.0, -2.0))),
                   None, Vector.fill(2)(-1e12), Vector.fill(2)(1e12), Set.empty, env) == Left(QpFailure.Infeasible))
  }

  it should "not start from a far vertex it cannot replace by the smallest point (F_0380)" in {
    // min (sum z - 1)^2 over |z| <= 1e12, 30 variables: past the smallest-point LP's caps the zero-cost vertex
    // is a corner 1e12 away, and a wrong-signed multiplier there read as rounding: certified at objective 0
    val n = 30
    qpSolve(_MatrixValue(n, n, Array.fill(n * n)(2.0)), Vector.fill(n)(-2.0), None, None,
            Vector.fill(n)(-1e12), Vector.fill(n)(1e12), Set.empty, env) match
      case Right(r) => assert(math.abs(r.objective + 1.0) <= 1e-9, s"certified at objective ${r.objective}")
      case Left(_)  => succeed
  }
