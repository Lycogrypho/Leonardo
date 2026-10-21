package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0173 — `size(A)` and `size(A, k)`, a matrix's shape at the prompt.
 *
 *  The shape is read off the evaluated operand on all three carriers — dense, exact and
 *  symbolic — and answered in the operand's tier, as `rank` answers.  A number is a `1×1` block
 *  (Decision 2, the `hcat` convention); an operand whose shape is not known, a free name
 *  included, stays unevaluated as the call written.
 */
class SizeTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def number(s: String, e: Environment = env): Double = parse(s).eval(e) match
    case Right(_Number(d)) => d
    case other             => fail(s"\"$s\" gave no number: $other")

  private def dense(s: String, e: Environment = env): (Int, Int, Vector[Double]) = parse(s).eval(e) match
    case Right(m: _MatrixValue) => (m.rows, m.cols, m.toVector)
    case other                  => fail(s"\"$s\" gave no dense matrix: $other")

  private def declines(s: String, e: Environment = env): Unit =
    parse(s).eval(e) match
      case Left(r) if r.toString.startsWith("size(") => ()
      case other => fail(s"\"$s\" should stay unevaluated, got $other")

  "size" should "answer the row [[rows, cols]] of a dense matrix" in {
    assert(dense("size([[1, 2, 3], [4, 5, 6]])") == (1, 2, Vector(2.0, 3.0)))
    assert(dense("size(eye(4))") == (1, 2, Vector(4.0, 4.0)))
  }

  it should "answer one dimension with a second argument" in {
    assert(number("size([[1, 2, 3], [4, 5, 6]], 1)") == 2.0)
    assert(number("size([[1, 2, 3], [4, 5, 6]], 2)") == 3.0)
  }

  it should "compose as a scalar with one dimension and as a matrix without" in {
    assert(number("size(zeros(3, 5), 2) + 1") == 6.0)
    assert(dense("2 * size(zeros(3, 5))") == (1, 2, Vector(6.0, 10.0)))
    assert(dense("zeros(size(eye(2), 1), 1)") == (2, 1, Vector(0.0, 0.0)))
  }

  it should "answer exactly on an exact matrix" in {
    parse("size([[1/2, 1], [1, 2], [3, 4]], 1)", Some(30)).eval(env) match
      case Right(r: _Rational) => assert(r == _Rational(3))
      case other               => fail(s"$other")
    parse("size([[1/2, 1], [1, 2], [3, 4]])", Some(30)).eval(env) match
      case Left(m: _Matrix) => assert(m.rows == 1 && m.cols == 2 && m.elems == Vector(_Rational(3), _Rational(2)))
      case other            => fail(s"$other")
  }

  it should "read the shape of a symbolic matrix" in {
    assert(dense("size([[a, b], [c, d], [e1, f]])") == (1, 2, Vector(3.0, 2.0)))
  }

  it should "read the shape of a computed operand and a bound matrix" in {
    assert(dense("size([[1, 2]] * [[1, 0, 0], [0, 1, 0]])") == (1, 2, Vector(1.0, 3.0)))
    val a = env.withBinding("A", _MatrixValue(2, 4, Array.fill(8)(1.0)))
    assert(number("size(A, 2)", a) == 4.0)
  }

  it should "read a number as 1x1" in {
    assert(dense("size(5)") == (1, 2, Vector(1.0, 1.0)))
    assert(number("size(5, 2)") == 1.0)
    assert(number("size(1 + 2i, 1)") == 1.0)
  }

  it should "stay unevaluated for an unknown shape or a bad dimension" in {
    declines("size(x)")
    declines("size(x, 1)")
    declines("size(inv([[1, 2], [2, 4]]))")
    declines("size([[1, 2]], 3)")
    declines("size([[1, 2]], 0)")
    declines("size([[1, 2]], 1.5)")
  }

  it should "print as written, so it round-trips" in {
    assert(parse("size(A)").toString == "size(A)")
    assert(parse("size(A, 2)").toString == "size(A, 2.0)")
  }
