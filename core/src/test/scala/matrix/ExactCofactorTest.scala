package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0169 — the cofactor expansion builds its constants in the matrix's tier.
 *
 *  `symbolicDet` and `symbolicInverse` signed each cofactor with an invented `_Number(-1)`, so in
 *  exact mode `det([[1, k], [k, 4]])` came back as `4 + (-1.0 * k * k)` and its roots as `±2.0`.
 *  A matrix with no exact cell keeps its `Double` constants.
 */
class ExactCofactorTest extends AnyFlatSpec:

  private val env = new Environment()

  private def eval(s: String, exact: Boolean = true): _Expression =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x, _) => x.eval(env).toExpression
      case other                => fail(s"parse failed for \"$s\": $other")

  /** Every numeric leaf that is not exact. */
  private def inexact(e: _Expression): List[_Expression] = e match
    case _: _Rational => Nil
    case n: _Number   => List(n)
    case other        => other.children.flatMap(inexact)

  "det" should "expand an exact symbolic matrix with exact constants" in {
    val d = eval("det([[1, k], [k, 4]])")
    assert(inexact(d).isEmpty, d)
    assert(inexact(eval("det([[1, k, 0], [k, 1, k], [0, k, 1]])")).isEmpty)
  }

  it should "still give the same value" in {
    val d = eval("det([[1, k], [k, 4]])")
    assert(d.eval(env.withBinding("k", _Rational(3))) == Right(_Rational(-5)))
  }

  "inv" should "invert an exact symbolic matrix with exact constants" in {
    val m = eval("inv([[1, k], [k, 4]])")
    assert(inexact(m).isEmpty, m)
    assert(inexact(eval("inv([[2*k]])")).isEmpty)
  }

  "a matrix with no exact cell" should "keep its Double constants" in {
    assert(inexact(eval("det([[1, k], [k, 4]])", exact = false)).nonEmpty)
  }
