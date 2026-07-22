package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 4.8: a square dense matrix raised to an integer power via the `^` operator
 *  and `pow(M, n)`. A^n by binary exponentiation, A^0 = I, A^-n = (A⁻¹)^n; non-square
 *  bases and non-integer exponents stay symbolic.
 */
class MatrixPowerTest extends AnyFlatSpec:

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

  "A^2" should "equal A·A" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(Power(a, _Number(2))) == a.multiply(a))
  }

  "A^3" should "equal A·A·A" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(Power(a, _Number(3))) == a.multiply(a).multiply(a))
  }

  "A^1" should "equal A" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(Power(a, _Number(1))) == a)
  }

  "A^0" should "equal the identity matrix" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(Power(a, _Number(0))) == _MatrixValue.identity(2))
  }

  "A^-1" should "equal inv(A)" in
  {
    val a   = dense(2, 2, 1, 2, 3, 4)
    val inv = a.inverse match
      case Some(m) => m
      case None    => fail("expected an invertible matrix")
    assert(evalMatrix(Power(a, _Number(-1))) == inv)
  }

  "A^-2" should "equal (A⁻¹)²" in
  {
    val a   = dense(2, 2, 1, 2, 3, 4)
    val inv = a.inverse match
      case Some(m) => m
      case None    => fail("expected an invertible matrix")
    assert(evalMatrix(Power(a, _Number(-2))) == inv.multiply(inv))
  }

  // --- degenerate / symbolic cases ---

  "a non-square matrix power" should "stay symbolic" in
  {
    val a = dense(2, 3, 1, 2, 3, 4, 5, 6)
    Power(a, _Number(2)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected a symbolic result but got: $v")
  }

  "a non-integer matrix power" should "stay symbolic" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    Power(a, _Number(0.5)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected a symbolic result but got: $v")
  }

  "a negative power of a singular matrix" should "stay symbolic" in
  {
    // [[1, 2], [2, 4]] is singular (det = 0), so it has no inverse.
    val a = dense(2, 2, 1, 2, 2, 4)
    Power(a, _Number(-1)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected a symbolic result but got: $v")
  }

  // --- parser dispatch: M^2 and pow(M, 2) build Power over the matrix ---

  "the parsed expression [[1,2],[3,4]]^2" should "reduce to A·A" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(parse("[[1, 2], [3, 4]]^2")) == a.multiply(a))
  }

  "pow([[1,2],[3,4]], 3)" should "reduce to A·A·A" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(evalMatrix(parse("pow([[1, 2], [3, 4]], 3)")) == a.multiply(a).multiply(a))
  }
