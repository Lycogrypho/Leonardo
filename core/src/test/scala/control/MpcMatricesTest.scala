package it.grypho.scala.leonardo
package control

import core.*
import matrix.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0055 — `mpcMatrices(A, B, C, Np, Nc)`: the prediction matrices `Y = Φ·x + Γ·U`.
 *
 *  The acceptance values are MPC requirement T1-03's.  The decisive test is not a table but a
 *  **simulation**: `Φ·x + Γ·U` must equal the outputs of the plant stepped forward with the
 *  same inputs, the last move held past `Nc` — which checks the convention as well as the
 *  arithmetic, where a transcribed table would only check the arithmetic.
 */
class MpcMatricesTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def pair(s: String, exact: Option[Int] = None): (_Expression, _Expression) =
    parse(s, exact).eval(env) match
      case Left(m: _Matrix) if m.rows == 1 && m.cols == 2 => (m(0, 0), m(0, 1))
      case other                                           => fail(s"\"$s\" gave no [[Phi, Gamma]]: $other")

  private def dense(e: _Expression): _MatrixValue = e match
    case m: _MatrixValue => m
    case other           => fail(s"expected a dense matrix, got $other")

  private def rowsOf(m: _MatrixValue): Vector[Vector[Double]] = m.toVector.grouped(m.cols).toVector

  "mpcMatrices" should "give the acceptance Φ and Γ for a scalar plant (Decision C)" in
  {
    val (phi, gamma) = pair("mpcMatrices(0.5, 1, 1, 3, 2)")
    assert(rowsOf(dense(phi)) == Vector(Vector(0.5), Vector(0.25), Vector(0.125)))
    assert(rowsOf(dense(gamma)) == Vector(Vector(1.0, 0), Vector(0.5, 1), Vector(0.25, 1.5)))
  }

  it should "predict exactly what the plant does, the last move held past Nc" in
  {
    // A double integrator with a two-row output, Np = 4, Nc = 2.
    val (a, b, c) = ("[[1, 0.1], [0, 1]]", "[[0.005], [0.1]]", "[[1, 0], [0, 1]]")
    val (phi, gamma) = pair(s"mpcMatrices($a, $b, $c, 4, 2)")
    val am = _MatrixValue(2, 2, Array(1, 0.1, 0, 1))
    val bm = _MatrixValue(2, 1, Array(0.005, 0.1))
    val x0 = _MatrixValue(2, 1, Array(0.3, -0.7))
    val u  = Vector(1.5, -2.0)
    // Simulate: x(k+1) = A x(k) + B u(k), y = C x = x; moves beyond Nc repeat the last one.
    val states = (0 until 4).scanLeft(x0)((x, k) => am.multiply(x).add(bm.scale(u(math.min(k, 1)))))
    val simulated = states.tail.flatMap(_.toVector).toVector
    val predicted = dense(phi).multiply(x0).add(dense(gamma).multiply(_MatrixValue(2, 1, u.toArray))).toVector
    assert(predicted.zip(simulated).forall((p, s) => math.abs(p - s) < 1e-12), s"$predicted vs $simulated")
  }

  it should "stay exact on exact input" in
  {
    val (phi, gamma) = pair("mpcMatrices(1/2, 1, 1, 3, 2)", Some(30))
    for m <- List(phi, gamma) do m match
      case lit: _Matrix => assert(lit.elems.forall(_.isInstanceOf[_Rational]), s"not exact: $lit")
      case other        => fail(s"expected an exact matrix, got $other")
  }

  it should "decline a bad horizon, a non-conforming plant, and an operand of unknown shape" in
  {
    for s <- List("mpcMatrices(0.5, 1, 1, 2, 3)",        // Nc > Np
                  "mpcMatrices(0.5, 1, 1, 0, 0)",        // empty horizon
                  "mpcMatrices(0.5, 1, 1, 3, 1.5)",      // non-integer
                  "mpcMatrices([[1, 0], [0, 1]], [[1]], [[1, 0]], 3, 2)",   // B has the wrong rows
                  "mpcMatrices(A, 1, 1, 3, 2)") do       // A free: its shape is unknown
      parse(s).eval(env) match
        case Left(_: _MpcMatrices) => succeed
        case other                 => fail(s"\"$s\" should decline, got $other")
  }

  it should "round-trip and reserve its name" in
  {
    val e = parse("mpcMatrices(A, B, C, 10, 3)")
    assert(parse(e.toString) == e)
    assert(Parser.ReservedWords.contains("mpcMatrices"))
  }
