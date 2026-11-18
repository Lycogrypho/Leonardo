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
