package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0124 phase B: the `matrix`, SVD, `control`, `ode` and `probability` budgets are `Limit`s read
 *  from the environment, or from a `Limits` overload where a published function takes none. The
 *  default is the old value, so every other suite is the evidence the default path is unchanged;
 *  each case here shows a budget is actually read, by lowering it (or raising it) and watching the
 *  answer change. */
class LimitsPhaseBTest extends AnyFlatSpec:

  private val env = new Environment()
  private def parse(s: String): _Expression = Parser.parse(s).get
  private def m(rows: Int, cols: Int, xs: Double*): _MatrixValue = _MatrixValue(rows, cols, xs.toArray)
  private def lowered(l: Limit, v: Long): Environment = env.withLimit(l, v)

  "the phase B limits" should "default to the published constants" in {
    assert(Limits.Default(Limit.MaxMatrixCells) == matrix.MaxMatrixCells)
    assert(Limits.Default(Limit.MaxSvdWork) == _MatrixValue.MaxSvdWork)
    assert(Limits.Default(Limit.MaxDlsimWork) == control.MaxDlsimWork)
    assert(Limits.Default(Limit.MaxMpcHorizon) == control.MaxMpcHorizon)
    assert(Limits.Default(Limit.MaxRiccatiOrder) == control.MaxRiccatiOrder)
    assert(Limits.Default(Limit.MaxOdeSteps) == ode.MaxOdeSteps)
    assert(Limits.Default(Limit.MaxTrajectoryRows) == ode.MaxTrajectoryRows)
    assert(Limits.Default(Limit.MaxDrawCount) == probability.MaxDrawCount)
    assert(Limits.Default(Limit.MaxPrbsLength) == probability.MaxPrbsLength)
  }

  "block matrices" should "read the cell cap from the environment" in {
    val e = parse("repmat([[1]], 2, 2)")
    assert(e.eval(env).isRight)
    assert(e.eval(lowered(Limit.MaxMatrixCells, 3)).isLeft)
    assert(parse("hcat([[1, 2]], [[3]])").eval(lowered(Limit.MaxMatrixCells, 2)).isLeft)
  }

  "the dense constructors" should "read the dimension cap from the environment" in {
    assert(parse("eye(3)").eval(env).isRight)
    assert(parse("eye(3)").eval(lowered(Limit.MaxDenseDim, 2)).isLeft)
    assert(parse("zeros(3, 1)").eval(lowered(Limit.MaxDenseDim, 2)).isLeft)
    assert(parse("ones(1, 3)").eval(lowered(Limit.MaxDenseDim, 2)).isLeft)
  }

  "a symbolic determinant and inverse" should "read the cofactor cap from the environment" in {
    val det = parse("det([[a, b], [c, d]])")
    assert(!det.eval(env).fold(identity, identity).isInstanceOf[matrix.Determinant])
    assert(det.eval(lowered(Limit.MaxSymbolicDim, 1)).fold(identity, identity).isInstanceOf[matrix.Determinant])
    val inv = parse("inv([[a, b], [c, d]])")
    assert(!inv.eval(env).fold(identity, identity).isInstanceOf[matrix.Inverse])
    assert(inv.eval(lowered(Limit.MaxSymbolicDim, 1)).fold(identity, identity).isInstanceOf[matrix.Inverse])
  }

  "the SVD" should "read its work cap from the environment, and say so when declining" in {
    val a = m(2, 2, 1, 2, 3, 4)
    assert(a.svd.isDefined && a.svd(Limits.Default.withLimit(Limit.MaxSvdWork, 7)).isEmpty)
    val rank = parse("rank([[1, 2], [3, 4]])")
    assert(rank.eval(env) == Right(_Number(2)))
    val tight = lowered(Limit.MaxSvdWork, 7)
    assert(rank.eval(tight).isLeft)
    rank match
      case r: matrix._Rank => assert(r.declineReason(tight).exists(_.contains("at most 7")), r.declineReason(tight))
      case other           => fail(s"$other")
  }

  "dlsim and arxRegressors" should "read their caps through the Limits overload" in {
    val one = m(1, 1, 0.5)
    val u   = m(1, 4, 1, 1, 1, 1)
    assert(control.dlsim(one, m(1, 1, 1), m(1, 1, 1), m(1, 1, 0), u, m(1, 1, 0)).isRight)
    assert(control.dlsim(one, m(1, 1, 1), m(1, 1, 1), m(1, 1, 0), u, m(1, 1, 0),
                         Limits.Default.withLimit(Limit.MaxDlsimWork, 3)).left.exists(_.isInstanceOf[control.SimFailure.TooLarge]))
    assert(control.dlsim(one, m(1, 1, 1), m(1, 1, 1), m(1, 1, 0), u, m(1, 1, 0),
                         Limits.Default.withLimit(Limit.MaxMatrixCells, 3)).left.exists(_.isInstanceOf[control.SimFailure.TooLarge]))
    val y = m(1, 6, 1, 2, 3, 4, 5, 6)
    assert(control.arxRegressors(y, y, 1, 1, 1).isRight)
    assert(control.arxRegressors(y, y, 1, 1, 1, Limits.Default.withLimit(Limit.MaxMatrixCells, 3))
             .left.exists(_.isInstanceOf[control.ArxFailure.TooLarge]))
  }

  "mpcMatrices and the Riccati family" should "read their caps from the environment" in {
    val (a, b, c) = (_Number(0.5), _Number(1), _Number(1))
    assert(control.mpcMatrices(a, b, c, 2, 1, env).isDefined)
    assert(control.mpcMatrices(a, b, c, 2, 1, lowered(Limit.MaxMpcHorizon, 1)).isEmpty)
    assert(control.mpcMatrices(a, b, c, 2, 1, lowered(Limit.MaxMatrixCells, 1)).isEmpty)

    val half = m(2, 2, 0.5, 0, 0, 0.5)
    val id   = m(2, 2, 1, 0, 0, 1)
    assert(control.dlyap(half, id, env).isRight)
    assert(control.dlyap(half, id, lowered(Limit.MaxRiccatiOrder, 1)) == Left(control.RiccatiFailure.TooLarge(2)))
    assert(control.dlqr(half, id, id, id, lowered(Limit.MaxRiccatiOrder, 1)) == Left(control.RiccatiFailure.TooLarge(2)))
    val z = m(2, 1, 0, 0)
    val step = (l: Limits) => control.kalmanStep(z, id, m(2, 1, 0, 0), z, half, id, id, id, id, id, l)
    assert(step(Limits.Default).isRight)
    assert(step(Limits.Default.withLimit(Limit.MaxRiccatiOrder, 1)) == Left(control.RiccatiFailure.TooLarge(2)))
  }

  "the ODE tiers" should "read the step and trajectory caps from the environment" in {
    val (rhs, y, t) = (parse("-y"), _Variable("y"), _Variable("t"))
    val state = ode.OdeState.Components(Vector(y))
    assert(ode.odeStep(rhs, state, t, 0, Vector(1.0), 1, ode.OdeMethod.RK45, env).isRight)
    assert(ode.odeStep(rhs, state, t, 0, Vector(1.0), 1, ode.OdeMethod.RK45, lowered(Limit.MaxOdeSteps, 1)) ==
           Left(ode.OdeFailure.MaxSteps(1)))
    assert(ode.odeSolve(rhs, state, t, 0, Vector(1.0), 1, 0.1, ode.OdeMethod.RK4, env).isRight)
    assert(ode.odeSolve(rhs, state, t, 0, Vector(1.0), 1, 0.1, ode.OdeMethod.RK4, lowered(Limit.MaxTrajectoryRows, 5))
             .left.exists(_.isInstanceOf[ode.OdeFailure.TooManyRows]))
    // The scalar RK4 shares the ceiling: with 4 steps instead of 1000 it is still an answer, a
    // coarser one, which is what raising or lowering a step budget means.
    val exact  = math.exp(-1)
    val coarse = ode.solveODE(rhs, y, t, 0, 1, 1, lowered(Limit.MaxOdeSteps, 4)).getOrElse(fail("declined"))
    val fine   = ode.solveODE(rhs, y, t, 0, 1, 1, env).getOrElse(fail("declined"))
    assert(math.abs(fine - exact) < math.abs(coarse - exact) && math.abs(coarse - exact) > 1e-6, (fine, coarse))
  }

  "draw and prbs" should "read their count caps from the environment and the Limits overloads" in {
    val n01 = probability._Distribution.of(probability.DistKind.Normal, Vector(0.0, 1.0)).getOrElse(fail("normal"))
    assert(probability.drawSamples(n01, 5, 1L).isRight)
    assert(probability.drawSamples(n01, 5, 1L, 0L, Limits.Default.withLimit(Limit.MaxDrawCount, 4)) ==
           Left(probability.SamplingFailure.InvalidCount(5, 4)))
    assert(probability.prbsSequence(3, 5, 1L, Limits.Default.withLimit(Limit.MaxPrbsLength, 4)) ==
           Left(probability.SamplingFailure.InvalidCount(5, 4)))
    assert(parse("draw(normal(0, 1), 5, 1)").eval(env).isRight)
    assert(parse("draw(normal(0, 1), 5, 1)").eval(lowered(Limit.MaxDrawCount, 4)).isLeft)
    assert(parse("prbs(3, 5, 1)").eval(lowered(Limit.MaxPrbsLength, 4)).isLeft)
  }
