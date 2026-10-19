package it.grypho.scala.leonardo
package control

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `acker(A, B, poles)` (F_0138): single-input pole placement by Ackermann's formula, in the
 *  `u = −K·x` convention, certified by the eigenvalues of `A − B·K`. */
class AckerTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  private def gain(s: String): Vector[Double] = ev(s) match
    case Right(k: _MatrixValue) if k.rows == 1 => k.toVector
    case other                                => fail(s"$s gave $other")

  private def close(a: Vector[Double], b: Vector[Double], tol: Double = 1e-9): Boolean =
    a.size == b.size && a.zip(b).forall((x, y) => math.abs(x - y) <= tol * math.max(1.0, math.abs(y)))

  private val dblInt = "[[0, 1], [0, 0]], [[0], [1]]"

  "acker" should "place real poles: the double integrator at -1 and -2 needs K = [2, 3]" in {
    assert(close(gain(s"acker($dblInt, [[-1, -2]])"), Vector(2.0, 3.0)))
  }

  it should "place a complex conjugate pair: -1 ± i needs K = [2, 2]" in {
    assert(close(gain(s"acker($dblInt, [[-1 + i, -1 - i]])"), Vector(2.0, 2.0)))
  }

  it should "leave A - B*K with exactly the requested eigenvalues on a three-state plant" in {
    val a = "[[0, 1, 0], [0, 0, 1], [-1, -2, -3]]"
    val k = gain(s"acker($a, [[0], [0], [1]], [[-2, -3, -4]])")
    ev(s"eigen($a - [[0], [0], [1]] * [[${k.mkString(", ")}]])") match
      case Left(m) =>
        val eig = m.children.collect { case _Number(d) => d }.sorted
        assert(close(eig.toVector, Vector(-4.0, -3.0, -2.0), 1e-8), eig)
      case other => fail(s"got $other")
  }

  it should "read a single pole written as a bare number, for a scalar plant (F_0150)" in {
    // x' = 2x + u, pole at -3: 2 - K = -3, so K = 5.
    assert(close(gain("acker(2, 1, -3)"), Vector(5.0)))
  }

  it should "stay unevaluated for an uncontrollable pair" in {
    assert(ev("acker([[1, 0], [0, 2]], [[1], [0]], [[-1, -2]])").isLeft)
  }

  it should "stay unevaluated for poles that are not closed under conjugation, or the wrong number of them" in {
    assert(ev(s"acker($dblInt, [[-1 + i, -2]])").isLeft)
    assert(ev(s"acker($dblInt, [[-1, -2, -3]])").isLeft)
  }

  it should "stay unevaluated for a multi-input plant: Ackermann's formula is single-input" in {
    assert(ev("acker([[0, 1], [0, 0]], [[1, 0], [0, 1]], [[-1, -2]])").isLeft)
  }

  it should "round-trip and reserve its word" in {
    val e = Parser.parse("acker(A, B, p)").get
    assert(Parser.parse(e.toString).get == e)
    assert(Parser.ReservedWords.contains("acker") && !Parser.ReservedWords.contains("place"))
  }
