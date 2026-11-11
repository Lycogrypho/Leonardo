package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec


/** F_0298 (AIACE L_0001): the numeric MPC prediction matrices, absolute and incremental, checked by
 *  simulating the plant over the horizon. */
class MpcNumericTest extends AnyFlatSpec:

  private def mat(rows: Int, cols: Int, xs: Double*): _MatrixValue = _MatrixValue(rows, cols, xs.toArray)

  // Two states, two inputs, two outputs: a lightly coupled stable plant.
  private val a = mat(2, 2, 0.9, 0.1, -0.2, 0.8)
  private val b = mat(2, 2, 0.5, 0.0, 0.1, 0.3)
  private val c = mat(2, 2, 1.0, 0.0, 0.5, 1.0)

  private def mul(m: _MatrixValue, v: Vector[Double]): Vector[Double] =
    Vector.tabulate(m.rows)(i => (0 until m.cols).map(j => m(i, j) * v(j)).sum)
  private def plus(u: Vector[Double], v: Vector[Double]): Vector[Double] = u.zip(v).map(_ + _)

  /** The outputs y(k+1) .. y(k+np) of the plant from x under the inputs us (held after the last). */
  private def simulate(x0: Vector[Double], us: Vector[Vector[Double]], np: Int): Vector[Double] =
    (0 until np).foldLeft((x0, Vector.empty[Double])) { case ((x, ys), i) =>
      val u  = us(math.min(i, us.size - 1))
      val x1 = plus(mul(a, x), mul(b, u))
      (x1, ys ++ mul(c, x1))
    }._2

  private def near(got: Vector[Double], want: Vector[Double]): Unit =
    assert(got.size == want.size, s"sizes ${got.size} and ${want.size}")
    got.zip(want).foreach((g, w) => assert(math.abs(g - w) <= 1e-12 * math.max(1.0, math.abs(w)), s"$got vs $want"))

  private val x0 = Vector(1.0, -2.0)
  private val moves = Vector(Vector(0.3, -0.1), Vector(-0.5, 0.2), Vector(0.1, 0.4))

  "the absolute prediction" should "reproduce a simulation of the plant (F_0298)" in {
    val (np, nc) = (6, 3)
    mpcPrediction(a, b, c, np, nc, Limits.Default) match
      case Left(why) => fail(s"$why")
      case Right(p) =>
        assert(p.phi.rows == np * 2 && p.phi.cols == 2 && p.gamma.rows == np * 2 && p.gamma.cols == nc * 2)
        near(plus(mul(p.phi, x0), mul(p.gamma, moves.flatten)), simulate(x0, moves, np))
  }

  it should "agree with the expression form mpcMatrices" in {
    val env = new Environment()
    (mpcPrediction(a, b, c, 5, 2, Limits.Default), mpcMatrices(a, b, c, 5, 2, env)) match
      case (Right(p), Some((phi: _MatrixValue, gamma: _MatrixValue))) =>
        near(p.phi.toVector, phi.toVector)
        near(p.gamma.toVector, gamma.toVector)
      case other => fail(s"$other")
  }

  "the incremental prediction" should "reproduce a simulation under u(k-1) and the moves' increments" in {
    val (np, nc) = (7, 3)
    val uPrev = Vector(0.2, -0.4)
    val inputs = moves.scanLeft(uPrev)(plus).tail         // u(k + j) = u(k-1) + sum of increments
    mpcIncrementalPrediction(a, b, c, np, nc, Limits.Default) match
      case Left(why) => fail(s"$why")
      case Right(p) =>
        assert(p.psi.rows == np * 2 && p.psi.cols == 2 && p.theta.cols == nc * 2)
        val y = plus(plus(mul(p.phi, x0), mul(p.psi, uPrev)), mul(p.theta, moves.flatten))
        near(y, simulate(x0, inputs, np))
  }

  "a malformed request" should "be declined with the reason" in {
    assert(mpcPrediction(a, b, c, 3, 4, Limits.Default).isLeft)                        // Nc > Np
    assert(mpcPrediction(a, mat(1, 2, 1, 0), c, 3, 2, Limits.Default).isLeft)           // B rows
    assert(mpcIncrementalPrediction(a, b, mat(1, 3, 1, 0, 0), 3, 2, Limits.Default).isLeft)
    assert(mpcPrediction(a, b, c, 2000, 2, Limits.Default).isLeft)                     // past the cap
    assert(mpcPrediction(mat(1, 1, Double.NaN), mat(1, 1, 1), mat(1, 1, 1), 2, 1, Limits.Default).isLeft)
  }
