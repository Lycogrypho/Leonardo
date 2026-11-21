package it.grypho.scala.leonardo
package control

import core.*
import optimize.QpConstraint
import org.scalatest.flatspec.AnyFlatSpec


/** F_0357 (AIACE L_0076): an MPC programme prepared once per model and tuning, then formed each cycle. */
class MpcPreparedTest extends AnyFlatSpec:

  private val env = new Environment()
  private def mat(r: Int, c: Int, f: (Int, Int) => Double) = _MatrixValue(r, c, Array.tabulate(r * c)(i => f(i / c, i % c)))
  private def eye(k: Int, d: Double) = mat(k, k, (i, j) => if i == j then d else 0.0)
  private def bits(d: Double) = java.lang.Double.doubleToRawLongBits(d)
  private def same(a: Vector[Double], b: Vector[Double]) = a.size == b.size && a.indices.forall(i => bits(a(i)) == bits(b(i)))

  /** The same programme, cell by cell to the bit. */
  private def sameQp(a: MpcQp, b: MpcQp): Boolean =
    same(a.h.toVector, b.h.toVector) && same(a.f, b.f) && same(a.lb, b.lb) && same(a.ub, b.ub) &&
      a.ineq.map(_._1.toVector).zip(b.ineq.map(_._1.toVector)).forall(same) && a.ineq.isDefined == b.ineq.isDefined &&
      a.ineq.map(_._2).zip(b.ineq.map(_._2)).forall(same)

  private def sameSolution(a: MpcSolution, b: MpcSolution): Boolean =
    same(a.input, b.input) && same(a.qp.z, b.qp.z) && a.qp.active == b.qp.active && a.qp.iterations == b.qp.iterations

  // Two states, one input, one output, a stable plant.
  private val (a, b, c) = (mat(2, 2, (i, j) => Seq(0.8, 0.1, -0.2, 0.7)(2 * i + j)), mat(2, 1, (i, _) => Seq(0.5, 1.0)(i)),
                           mat(1, 2, (_, j) => Seq(1.0, 0.0)(j)))
  private val (np, nc) = (10, 3)
  private def predictions: Seq[MpcPredictionMatrices] = Seq(
    mpcPrediction(a, b, c, np, nc, Limits.Default), mpcIncrementalPrediction(a, b, c, np, nc, Limits.Default),
    mpcAccumulatedPrediction(a, b, c, np, nc, Limits.Default)).map(_.fold(f => fail(s"$f"), identity))
  private val weights = MpcWeights(Vector(eye(1, 1.0)), Vector(eye(1, 0.1)))
  private def shaped(width: Double) = MpcBounds(u = Some((Vector(-width), Vector(width))), du = Some((Vector(-0.3 * width), Vector(0.3 * width))),
                                                y = Some((Vector(Double.NegativeInfinity), Vector(1.2 * width))),
                                                soft = Some(MpcSoftOutputs(Vector(10.0), Vector(100.0))))

  "a prepared programme" should "give, every cycle, the programme and solution of mpcQp built afresh (F_0357)" in {
    for pr <- predictions do
      val prepared = mpcQpPrepared(pr, weights, shaped(1.0), Limits.Default).fold(f => fail(s"$f"), identity)
      val rnd  = new java.util.Random(76)
      var x    = Vector(1.0, -0.5)
      var u    = Vector(0.0)
      var warm = Set.empty[QpConstraint]
      for k <- 0 until 12 do
        val ref    = Vector(if k < 6 then 1.0 else -0.4)
        val bounds = shaped(0.8 + 0.4 * rnd.nextDouble())          // new values, the same shape
        val fresh  = mpcQp(pr, weights, ref, x, u, bounds, Limits.Default).fold(f => fail(s"$f"), identity)
        val cycle  = prepared.at(ref, x, u, bounds).fold(f => fail(s"$f"), identity)
        assert(sameQp(fresh, cycle), s"cycle $k")
        assert(fresh == cycle)
        (fresh.solve(warm, env), cycle.solve(warm, env)) match
          case (Right(s1), Right(s2)) =>
            assert(sameSolution(s1, s2), s"cycle $k: $s1 vs $s2")
            warm = s2.qp.active.toSet
            u = s2.input
            x = a.multiply(_MatrixValue(2, 1, x.toArray)).add(b.multiply(_MatrixValue(1, 1, u.toArray))).toVector
          case other => fail(s"cycle $k: $other")
  }

  it should "take the cycle's targets and give mpcQp's programme with those targets in its weights (F_0360)" in {
    // AIACE moves x_s and u_s every cycle (a target layer's output): the targets enter only the linear term
    val terminals = Seq(mpcTerminalPrediction(a, b, np, nc, false, Limits.Default), mpcTerminalPrediction(a, b, np, nc, true, Limits.Default),
                        mpcAccumulatedTerminalPrediction(a, b, np, nc, Limits.Default)).map(_.fold(f => fail(s"$f"), identity))
    for (pr, form) <- predictions.zipWithIndex do
      val absolute = form == 0
      def tuned(xs: Vector[Double], us: Vector[Double]) = weights.copy(
        terminal       = Some(MpcTerminal(eye(2, 0.5), terminals(form), Some(xs))),
        inputTarget    = Option.when(absolute)(us),
        positionTarget = Option.when(!absolute)(MpcPositionTarget(Vector(eye(1, 0.2)), us)))
      val prepared = mpcQpPrepared(pr, tuned(Vector(0.0, 0.0), Vector(0.0)), shaped(1.0), Limits.Default).fold(f => fail(s"$f"), identity)
      val rnd  = new java.util.Random(80)
      var x    = Vector(1.0, -0.5)
      var u    = Vector(0.0)
      var warm = Set.empty[QpConstraint]
      for k <- 0 until 12 do
        val xs = Vector(rnd.nextGaussian(), rnd.nextGaussian())
        // held (m entries) on even cycles, stacked (Nc·m) on odd ones
        val us = if k % 2 == 0 then Vector(rnd.nextGaussian()) else Vector.fill(nc)(rnd.nextGaussian())
        val bounds = shaped(1.0)
        val fresh  = mpcQp(pr, tuned(xs, us), Vector(0.5), x, u, bounds, Limits.Default).fold(f => fail(s"$f"), identity)
        val cycle  = (if absolute then prepared.at(Vector(0.5), x, u, bounds, inputTarget = Some(us), terminalTarget = Some(xs))
                      else prepared.at(Vector(0.5), x, u, bounds, positionTarget = Some(us), terminalTarget = Some(xs)))
                       .fold(f => fail(s"$f"), identity)
        assert(sameQp(fresh, cycle), s"form $form cycle $k")
        (fresh.solve(warm, env), cycle.solve(warm, env)) match
          case (Right(s1), Right(s2)) =>
            assert(sameSolution(s1, s2), s"form $form cycle $k: $s1 vs $s2")
            warm = s2.qp.active.toSet
            u = s2.input
            x = a.multiply(_MatrixValue(2, 1, x.toArray)).add(b.multiply(_MatrixValue(1, 1, u.toArray))).toVector
          case other => fail(s"form $form cycle $k: $other")
      // a target left out keeps the prepared one
      assert(prepared.at(Vector(0.5), x, u, shaped(1.0)) == mpcQp(pr, tuned(Vector(0.0, 0.0), Vector(0.0)), Vector(0.5), x, u, shaped(1.0), Limits.Default))
  }

  it should "refuse a cycle's target whose cost the prepared weights do not carry, or of the wrong size (F_0360)" in {
    val (abs, inc) = (predictions.head, predictions(1))
    def prep(pr: MpcPredictionMatrices) = mpcQpPrepared(pr, weights, MpcBounds(), Limits.Default).fold(f => fail(s"$f"), identity)
    def why(r: Either[MpcFailure, MpcQp]) = r match
      case Left(MpcFailure.InvalidInput(w)) => w
      case other                            => fail(s"$other")
    val (x, u, ref) = (Vector(0.0, 0.0), Vector(0.0), Vector(1.0))
    assert(why(prep(inc).at(ref, x, u, MpcBounds(), inputTarget = Some(Vector(1.0)))) == "an input target has no meaning for increments")
    assert(why(prep(abs).at(ref, x, u, MpcBounds(), inputTarget = Some(Vector(1.0, 2.0)))) == "the input target needs 1 or 3 finite entries")
    assert(why(prep(inc).at(ref, x, u, MpcBounds(), positionTarget = Some(Vector(1.0)))) == "the prepared weights have no position cost")
    assert(why(prep(abs).at(ref, x, u, MpcBounds(), terminalTarget = Some(x))) == "the prepared weights have no terminal cost")
    // the absolute form's R always weighs u − u_s, so a cycle's input target needs no prepared one
    assert(prep(abs).at(ref, x, u, MpcBounds(), inputTarget = Some(Vector(1.0))) ==
      mpcQp(abs, weights.copy(inputTarget = Some(Vector(1.0))), ref, x, u, MpcBounds(), Limits.Default))
  }

  "at" should "refuse bounds of another shape, and take new finite values" in {
    val pr = predictions.head
    val prepared = mpcQpPrepared(pr, weights, shaped(1.0), Limits.Default).fold(f => fail(s"$f"), identity)
    def at(bounds: MpcBounds) = prepared.at(Vector(1.0), Vector(0.0, 0.0), Vector(0.0), bounds)
    assert(at(shaped(2.0)).isRight)
    val refused = Seq(
      shaped(1.0).copy(y = Some((Vector(-1.0), Vector(1.2)))),                    // a bound turned finite
      shaped(1.0).copy(u = Some((Vector(-1.0), Vector(Double.PositiveInfinity)))), // and one turned infinite
      shaped(1.0).copy(du = None),                                                // a group removed
      shaped(1.0).copy(soft = Some(MpcSoftOutputs(Vector(10.0), Vector(50.0))))) // other soft weights
    for bounds <- refused do
      at(bounds) match
        case Left(MpcFailure.InvalidInput(why)) => assert(why.startsWith("the bounds must have the prepared shape"), why)
        case other                              => fail(s"$bounds: $other")
  }

  it should "report the cycle's own checks with mpcQp's messages" in {
    val pr = predictions.head
    val prepared = mpcQpPrepared(pr, weights, MpcBounds(), Limits.Default).fold(f => fail(s"$f"), identity)
    assert(prepared.at(Vector(1.0), Vector(0.0), Vector(0.0), MpcBounds()) ==
      Left(MpcFailure.InvalidInput("the state has 1 entries for 2 states")))
    assert(mpcQp(pr, weights, Vector(1.0), Vector(0.0), Vector(0.0), MpcBounds(), Limits.Default) ==
      Left(MpcFailure.InvalidInput("the state has 1 entries for 2 states")))
    assert(prepared.at(Vector(1.0, 2.0), Vector(0.0, 0.0), Vector(0.0), MpcBounds()) ==
      Left(MpcFailure.InvalidInput("the reference must have 1 or 10 entries")))
    assert(prepared.at(Vector(Double.NaN), Vector(0.0, 0.0), Vector(0.0), MpcBounds()) ==
      Left(MpcFailure.InvalidInput("every entry must be finite")))
  }
