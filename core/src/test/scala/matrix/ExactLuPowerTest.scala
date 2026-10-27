package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0226: `lu` of an exact square matrix is exact, a singular one included.
 *  F_0227: `A ^ n` of an exact square matrix is exact, held to `MaxExactPowerBits`. */
class ExactLuPowerTest extends AnyFlatSpec:

  private val env = Environment()
  private def exact(s: String): Either[_Expression, _Value] = Parser.parse(s, Some(30)).get.eval(env)
  private def q(n: Int, d: Int = 1): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail("zero denominator"))
  private def rationalMatrix(r: Int, c: Int, cells: _Rational*): _Matrix = _Matrix(r, c, cells.toVector)

  private def factors(s: String): (_Matrix, _Matrix, _Matrix) = exact(s) match
    case Left(_Matrix(1, 3, Vector(l: _Matrix, u: _Matrix, p: _Matrix))) => (l, u, p)
    case other                                                            => fail(s"no exact factors: $other")

  private def product(a: _Expression, b: _Expression): _Expression = MatProduct(a, b).eval(env) match
    case Left(m) => m
    case Right(v) => v

  "lu of an exact matrix" should "be exact, with the dense kernel's pivoting" in {
    val (l, u, p) = factors("lu([[1, 2], [3, 4]])")
    assert(l == rationalMatrix(2, 2, q(1), q(0), q(1, 3), q(1)))
    assert(u == rationalMatrix(2, 2, q(3), q(4), q(0), q(2, 3)))
    assert(p == rationalMatrix(2, 2, q(0), q(1), q(1), q(0)))
  }

  "lu of an exact singular matrix" should "factor it, with a zero on U's diagonal" in {
    val a = "[[1, 2, 3], [2, 4, 6], [1, 1, 1]]"
    val (l, u, p) = factors(s"lu($a)")
    val lit = Parser.parse(a, Some(30)).get.eval(env).fold(identity, identity)
    assert(product(p, lit) == product(l, u))
    assert((0 until 3).exists(i => u(i, i) == q(0)))
    assert((0 until 3).forall(i => (0 until i).forall(j => u(i, j) == q(0))))
  }

  "lu of an inexact matrix" should "keep the dense kernel" in {
    Parser.parse("lu([[1, 2], [3, 4]])").get.eval(env) match
      case Left(_Matrix(1, 3, Vector(_: _MatrixValue, _: _MatrixValue, _: _MatrixValue))) => succeed
      case other => fail(s"unexpected: $other")
  }

  "an exact matrix power" should "be exact" in {
    assert(exact("[[2, 1], [1, 2]]^10") == Left(rationalMatrix(2, 2, q(29525), q(29524), q(29524), q(29525))))
    assert(exact("pow([[2, 1], [1, 2]], 3)") == Left(rationalMatrix(2, 2, q(14), q(13), q(13), q(14))))
  }

  it should "stay exact through a product with a vector" in {
    assert(exact("[[2, 1], [1, 2]]^10 * [[1], [0]]") == Left(rationalMatrix(2, 1, q(29525), q(29524))))
  }

  it should "invert for a negative power, and give the identity at zero" in {
    assert(exact("[[2, 1], [1, 2]]^(-1)") == Left(rationalMatrix(2, 2, q(2, 3), q(-1, 3), q(-1, 3), q(2, 3))))
    assert(exact("[[2, 1], [1, 2]]^0") == Left(rationalMatrix(2, 2, q(1), q(0), q(0), q(1))))
  }

  it should "stay unevaluated for a singular base to a negative power" in {
    assert(exact("[[1, 2], [2, 4]]^(-1)") match
      case Left(_: _Matrix) => false
      case _                => true)
  }

  it should "decline past MaxExactPowerBits rather than demote" in {
    exact("[[2, 1], [1, 2]]^1000000") match
      case Left(_: _Matrix) | Right(_) => fail("answered past the cap")
      case Left(_)                     => succeed
  }
