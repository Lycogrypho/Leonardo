package it.grypho.scala.leonardo
package matrix

import cli.Session
import core.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.sin

/** Element-wise distribution of a scalar function over a matrix, seen through the REPL —
 *  the two sessions reported in issues 1.3 and 4.6.
 *
 *  Split out of `MatrixFunctionTest` by issue 5.2 phase 1.1.
 */
class MatrixFunctionReplTest extends AnyFlatSpec:

  /** Row-major dense matrix, mirroring the helper in `MatrixFunctionTest`. */
  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  "eval of a function defined over a matrix" should "reduce element-wise (issue 1.3)" in
  {
    val s = Session()
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    s.execute("S_A := sin(A)")
    val result = s.execute("eval S_A")
    val expected = dense(3, 3, sin(1), sin(2), sin(3), sin(3), sin(2), sin(1), sin(1), sin(1), sin(1))
    assert(result == expected.display(5),
      s"expected element-wise sin, got: $result")
  }

  "exp of a symbolic matrix via the REPL (the reported bug)" should "propagate element-wise" in
  {
    val s = Session()
    s.execute("A := [[x, 2*x], [3*y, 1]]")
    s.execute("B := exp(A)")
    val out = s.execute("eval B")
    assert(out.contains("exp(x)"),         s"expected exp(x) cell; got: $out")
    assert(out.contains("exp((2.0 * x))"), s"expected exp(2x) cell; got: $out")
    assert(out.contains("2.71828"),        s"expected exp(1) folded to a number; got: $out")
    assert(!out.startsWith("exp("),        s"must not stay a single exp(matrix); got: $out")
  }
