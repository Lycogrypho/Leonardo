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

  "outside exact mode" should "be unchanged" in {
    assert(plain("0b1010").exists { case b: _Based => !b.isExact; case _ => false })
    assert(plain("0b1010 + 0b0110") == Right(_Number(16.0)))
    assert(plain("tobase(10, 2)").exists { case b: _Based => !b.isExact; case _ => false })
  }
