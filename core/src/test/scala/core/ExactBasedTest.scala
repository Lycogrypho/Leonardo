package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0323 (LT L_0080) — a based literal is exact in exact mode.  Before the fix `0b1010 + 0b0110`
 *  under `exact on` answered `16.0`, and `tobase(10, 2)` an inexact value. */
class ExactBasedTest extends AnyFlatSpec:

  private val env = new Environment(workingPrecision = 30, exactMode = true)

  private def exact(src: String): Either[_Expression, _Value] = Parser.parse(src, Some(30)) match
    case Parser.Success(e, _) => e.eval(env)
    case other                => fail(s"parse failed for '$src': $other")

  private def plain(src: String): Either[_Expression, _Value] = Parser.parse(src) match
    case Parser.Success(e, _) => e.eval(new Environment())
    case other                => fail(s"parse failed for '$src': $other")

  "a based literal in exact mode" should "be exact and add exactly" in {
    assert(exact("0b1010").exists { case b: _Based => b.isExact; case _ => false })
    assert(exact("0b1010 + 0b0110") == Right(_Rational(16)))
    assert(exact("0xFF * 0o17") == Right(_Rational(255 * 15)))
    assert(exact("0t1TT + 0") == Right(_Rational(5)))
    assert(exact("0b1010 / 4") == Right(_Rational.of(BigInt(5), BigInt(2)).getOrElse(fail("5/2"))))
  }

  it should "still display in its base" in {
    assert(exact("0b1010").map(_.toString) == Right("0b1010"))
    assert(exact("0xff").map(_.toString) == Right("0xFF"))
  }

  it should "put its expression in the exact tier" in {
    val e = Parser.parse("0b1010 + x", Some(30)).getOrElse(fail("parse"))
    assert(_Rational.containsExact(e))
  }

  "tobase in exact mode" should "answer an exact based value, or an exact integer for base 10" in {
    assert(exact("tobase(10, 2)").exists { case b: _Based => b.isExact && b.toString == "0b1010"; case _ => false })
    assert(exact("tobase(41, 10)") == Right(_Rational(41)))
    assert(exact("balanced(5)").exists { case b: _Based => b.isExact && b.toString == "0t1TT"; case _ => false })
  }

  // ── F_0336: beyond the four operators ────────────────────────────────────────────────────────────

  private def isExactMatrix(r: Either[_Expression, _Value], cells: Seq[Int]): Boolean = r match
    case Left(m: _MatrixShaped) => m.children == cells.map(_Rational(_))
    case _                      => false

  "an exact based literal in a matrix" should "stay exact (F_0336)" in {
    assert(isExactMatrix(exact("[[0b1, 0b10]]"), Seq(1, 2)), exact("[[0b1, 0b10]]").toString)
    assert(exact("det([[0b1, 0b10], [0b11, 0b100]])") == Right(_Rational(-2)))
    assert(exact("max([[0b11, 0b101]])") == Right(_Rational(5)))
    assert(isExactMatrix(exact("2 * [[0b1, 0b10]]"), Seq(2, 4)), exact("2 * [[0b1, 0b10]]").toString)
  }

  "a negated exact based literal" should "be an exact integer (F_0336)" in {
    assert(exact("-0b101") == Right(_Rational(-5)))
    assert(exact("2 * -0b11") == Right(_Rational(-6)))
    assert(exact("-0b11 + 0") == Right(_Rational(-3)))
    assert(exact("-0xF*x").map(_.toString) == exact("-15*x").map(_.toString))
  }

  "a two-argument function of exact based literals" should "take the exact arm (F_0336)" in {
    assert(exact("binom(0b101, 2)") == Right(_Rational(10)))
    assert(exact("binom(5, 0b10)") == Right(_Rational(10)))
    assert(exact("mod(0b111, 0b11)") == Right(_Rational(1)))
  }

  "outside exact mode" should "be unchanged" in {
    assert(plain("0b1010").exists { case b: _Based => !b.isExact; case _ => false })
    assert(plain("0b1010 + 0b0110") == Right(_Number(16.0)))
    assert(plain("tobase(10, 2)").exists { case b: _Based => !b.isExact; case _ => false })
    assert(plain("[[0b1, 0b10]]") == Right(_MatrixValue(1, 2, Array(1.0, 2.0))))
    assert(plain("-0b101") == Right(_Number(-5.0)))
    assert(plain("binom(0b101, 2)") == Right(_Number(10.0)))
  }
