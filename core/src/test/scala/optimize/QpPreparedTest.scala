package it.grypho.scala.leonardo
package optimize

import core.*
import org.scalatest.flatspec.AnyFlatSpec


/** F_0357 (AIACE L_0076): a QP prepared once and solved with new data is the one-shot `qpSolve`. */
class QpPreparedTest extends AnyFlatSpec:

  private val env = new Environment()
  private def mat(r: Int, c: Int, f: (Int, Int) => Double) = _MatrixValue(r, c, Array.tabulate(r * c)(i => f(i / c, i % c)))
  private def bits(d: Double) = java.lang.Double.doubleToRawLongBits(d)
  private def same(a: Vector[Double], b: Vector[Double]) = a.size == b.size && a.indices.forall(i => bits(a(i)) == bits(b(i)))

  /** Equal results to the last bit, or the same failure. */
  private def identical(a: Either[QpFailure, QpResult], b: Either[QpFailure, QpResult]): Boolean = (a, b) match
    case (Right(x), Right(y)) =>
      val (m, n) = (x.multipliers, y.multipliers)
      same(x.z, y.z) && bits(x.objective) == bits(y.objective) && x.iterations == y.iterations &&
        bits(x.kktResidual) == bits(y.kktResidual) && x.active == y.active &&
        same(m.inequality ++ m.equality ++ m.lower ++ m.upper, n.inequality ++ n.equality ++ n.lower ++ n.upper)
    case (Left(x), Left(y)) => x == y
    case _ => false

  "a prepared QP" should "solve to the bits of qpSolve for every linear term and right-hand side (F_0357)" in {
    val rnd = new java.util.Random(357)
    var answered = 0
    for _ <- 0 until 60 do
      val n = 1 + rnd.nextInt(6)
      val k = rnd.nextInt(2 * n + 1)
      val l = mat(n, n, (i, j) => if j <= i then rnd.nextGaussian() else 0.0)
      val h = l.multiply(l.transpose).add(mat(n, n, (i, j) => if i == j then 0.1 else 0.0))
      val a = Option.when(k > 0)(mat(k, n, (_, _) => rnd.nextGaussian()))
      val aeq = Option.when(n > 1 && rnd.nextInt(3) == 0)(mat(1, n, (_, _) => rnd.nextGaussian()))
      val lowerAt = Vector.fill(n)(rnd.nextBoolean())
      val upperAt = Vector.fill(n)(rnd.nextBoolean())
      def bounds(width: Double) = (lowerAt.map(f => if f then -width else Double.NegativeInfinity),
                                   upperAt.map(f => if f then width else Double.PositiveInfinity))
      val (lb0, ub0) = bounds(1.0)
      val prepared = qpPrepare(h, a, aeq, lb0, ub0)
      var warm = Set.empty[QpConstraint]
      for _ <- 0 until 4 do
        val f   = Vector.fill(n)(rnd.nextGaussian())
        val b   = a.map(m => Vector.fill(m.rows)(rnd.nextGaussian()))
        val beq = aeq.map(_ => Vector(rnd.nextGaussian()))
        val (lb, ub) = bounds(0.5 + rnd.nextDouble())
        val once  = qpSolve(h, f, a.zip(b), aeq.zip(beq), lb, ub, warm, env)
        val again = prepared.solve(f, b, beq, lb, ub, warm, env)
        assert(identical(once, again), s"$once\n$again")
        once.foreach { r => warm = r.active.toSet; answered += 1 }
    assert(answered > 100, s"only $answered answered: the problems test too little")
  }

  it should "refuse bounds finite at other entries than the prepared ones" in {
    val h = mat(2, 2, (i, j) => if i == j then 1.0 else 0.0)
    val prepared = qpPrepare(h, None, None, Vector(-1.0, Double.NegativeInfinity), Vector.fill(2)(Double.PositiveInfinity))
    assert(prepared.solve(Vector(1.0, 1.0), None, None, Vector(-2.0, Double.NegativeInfinity), Vector.fill(2)(Double.PositiveInfinity),
                          Set.empty, env).isRight)
    assert(prepared.solve(Vector(1.0, 1.0), None, None, Vector(-1.0, -1.0), Vector.fill(2)(Double.PositiveInfinity), Set.empty, env)
      == Left(QpFailure.InvalidInput("the bounds' finite entries differ from the prepared ones")))
  }

  it should "report qpSolve's checks with qpSolve's messages" in {
    val h = mat(2, 2, (i, j) => if i == j then 1.0 else 0.5 * (i + 1))
    val free = Vector.fill(2)(Double.NegativeInfinity)
    val none = Vector.fill(2)(Double.PositiveInfinity)
    assert(qpSolve(h, Vector(1.0, 1.0), None, None, free, none, Set.empty, env) ==
      Left(QpFailure.InvalidInput("H must be symmetric")))
    assert(qpSolve(h, Vector(1.0), None, None, free, none, Set.empty, env) ==
      Left(QpFailure.InvalidInput("f, lb and ub must each have 2 entries")))
    val a = mat(1, 2, (_, _) => 1.0)
    assert(qpSolve(mat(2, 2, (i, j) => if i == j then 1.0 else 0.0), Vector(1.0, 1.0), Some((a, Vector(1.0, 2.0))), None,
                   free, none, Set.empty, env) ==
      Left(QpFailure.InvalidInput("A must have 2 columns and as many rows as its right-hand side has entries")))
  }
