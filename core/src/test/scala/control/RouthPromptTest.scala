package it.grypho.scala.leonardo
package control

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `routhTable(G, s)` at the prompt (F_0139): the library's Routh array as a matrix, each row
 *  padded with zeros to the width of the first. The prompt keeps the Scala name, since `routh`
 *  shipped as a free variable name (ControlTest pins that it still is). */
class RouthPromptTest extends AnyFlatSpec:

  private def parsed(s: String): _Expression = Parser.parse(s).get
  private val s = _Variable("s")

  /** The library table, padded into the matrix the prompt answers. */
  private def padded(g: String): Option[_MatrixValue] =
    routhTable(parsed(g), s).map { rows =>
      val w = rows.map(_.size).max
      _MatrixValue(rows.size, w, rows.flatMap(r => r.padTo(w, 0.0)).toArray)
    }

  "routhTable(G, s)" should "answer the library's Routh array, padded into a matrix" in {
    val g = "1/(s^3 + 2*s^2 + 3*s + 4)"
    val expected = padded(g)
    assert(expected.isDefined)
    assert(parsed(s"routhTable($g, s)").eval(new Environment()) == Right(expected.getOrElse(fail())))
  }

  it should "answer the hand-computed array, not only what the library computes (F_0150)" in {
    // s^3 + 2s^2 + 3s + 4: rows [1, 3], [2, 4], [(2*3 - 1*4)/2, 0] = [1, 0], [4, 0]. The case
    // above rebuilds its expectation with the code under test, so it could not catch the code.
    assert(parsed("routhTable(1/(s^3 + 2*s^2 + 3*s + 4), s)").eval(new Environment()) ==
      Right(_MatrixValue(4, 2, Array(1.0, 3.0, 2.0, 4.0, 1.0, 0.0, 4.0, 0.0))))
  }

  it should "show a sign change in the first column for an unstable plant" in {
    parsed("routhTable(1/(s^3 + s^2 + 2*s + 8), s)").eval(new Environment()) match
      case Right(m: _MatrixValue) =>
        val firstColumn = (0 until m.rows).map(r => m(r, 0))
        assert(firstColumn.sliding(2).exists(p => math.signum(p(0)) != math.signum(p(1))), firstColumn)
      case other => fail(s"expected a matrix, got $other")
  }

  it should "use the session's parameters but never the frequency variable's binding" in {
    val env = new Environment().withBinding("a", _Number(2)).withBinding("s", _Number(3))
    assert(parsed("routhTable(1/(s + a), s)").eval(env) == Right(padded("1/(s + 2)").getOrElse(fail())))
  }

  it should "stay unevaluated for a non-rational argument" in {
    assert(parsed("routhTable(exp(-s)/(s + 1), s)").eval(new Environment()).isLeft)
  }

  it should "round-trip through toString" in {
    val e = parsed("routhTable(1/(s^2 + s + 1), s)")
    assert(parsed(e.toString) == e)
  }

  "the prompt name" should "be reserved, while routh itself stays a free variable name" in {
    assert(Parser.ReservedWords.contains("routhTable") && !Parser.ReservedWords.contains("routh"))
  }
