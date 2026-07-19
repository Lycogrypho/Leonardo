package it.grypho.scala.leonardo
package matrix

import cli.Session
import core.*
import scalar.*
import matrix.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.{exp, log, sin, cos, tan, atan}


/** Issue 1.3: a scalar function applied to a matrix distributes element-wise, so
 *  sin(A), exp(A), â€¦ reduce to a dense _MatrixValue of the per-element results.
 */
class MatrixFunctionTest extends AnyFlatSpec:

  val env = new Environment()

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def evalMatrix(e: _Expression): _MatrixValue =
    e.eval(env) match
      case Right(m: _MatrixValue) => m
      case other                  => fail(s"expected a dense matrix value but got: $other")

  // --- direct node evaluation ---

  "sin of a dense matrix" should "apply element-wise" in
  {
    val m = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(Sin(m)) == dense(2, 2, sin(1), sin(2), sin(3), sin(4)))
  }

  "cos of a dense matrix" should "apply element-wise" in
  {
    val m = dense(1, 3, 0, 1, 2)
    assert(evalMatrix(Cos(m)) == dense(1, 3, cos(0), cos(1), cos(2)))
  }

  "exp of a dense matrix" should "apply element-wise" in
  {
    val m = dense(2, 1, 0, 1)
    assert(evalMatrix(Exp(m)) == dense(2, 1, exp(0), exp(1)))
  }

  "tan of a dense matrix" should "apply element-wise" in
  {
    val m = dense(1, 2, 0.5, 1.0)
    assert(evalMatrix(Tg(m)) == dense(1, 2, tan(0.5), tan(1.0)))
  }

  "atan of a dense matrix" should "apply element-wise" in
  {
    val m = dense(1, 2, 1, 2)
    assert(evalMatrix(Atan(m)) == dense(1, 2, atan(1), atan(2)))
  }

  "ln of a positive dense matrix" should "apply element-wise" in
  {
    val m = dense(1, 3, 1, 2, 3)
    assert(evalMatrix(Ln(m)) == dense(1, 3, log(1), log(2), log(3)))
  }

  "log base 10 of a dense matrix" should "apply element-wise" in
  {
    // LogBase(m, 10) â€” log(x)/log(10) per element.
    val m = dense(1, 2, 10, 100)
    val result = evalMatrix(LogBase(m, _Number(10)))
    assert(math.abs(result(0, 0) - 1.0) < 1e-9)
    assert(math.abs(result(0, 1) - 2.0) < 1e-9)
  }

  // --- out-of-domain element leaves the node symbolic ---

  "ln of a matrix with a non-positive element" should "stay symbolic" in
  {
    // ln(0) and ln(-1) are non-real; the dense carrier cannot hold them, so the
    // whole application stays symbolic rather than emitting a matrix with holes.
    val m = dense(1, 2, 1, -1)
    assert(Ln(m).eval(env).isLeft)
  }

  "asin of a matrix with an out-of-range element" should "stay symbolic" in
  {
    val m = dense(1, 2, 0.5, 2.0)   // asin(2.0) is NaN
    assert(Asin(m).eval(env).isLeft)
  }

  // --- through the parser / REPL, matching the issue report ---

  "eval of a function defined over a matrix" should "reduce element-wise (issue 1.3)" in
  {
    val s = new Session()
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    s.execute("S_A := sin(A)")
    val result = s.execute("eval S_A")
    val expected = dense(3, 3, sin(1), sin(2), sin(3), sin(3), sin(2), sin(1), sin(1), sin(1), sin(1))
    assert(result == expected.display(5),
      s"expected element-wise sin, got: $result")
  }

  "sin applied to a matrix literal via the parser" should "evaluate element-wise" in
  {
    parse("sin([[0, 1], [2, 3]])").eval(env) match
      case Right(m: _MatrixValue) => assert(m == dense(2, 2, sin(0), sin(1), sin(2), sin(3)))
      case other                  => fail(s"expected a dense matrix value but got: $other")
  }
