package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0296: a power whose exponent does not depend on the variable takes the constant-power rule
 *  `k*u^(k-1)*u'`, however the exponent is written, never the general rule `u^k*(k'*ln u + k*u'/u)`. */
class DeriveConstantPowerTest extends AnyFlatSpec:

  private val x = _Variable("x")
  private def p(s: String, exact: Boolean = false): _Expression = Parser.parse(s, Option.when(exact)(30)).get

  private def hasLn(e: _Expression): Boolean = e match
    case _: Ln => true
    case other => other.children.exists(hasLn)

  "an unfolded fractional exponent" should "take the constant-power rule (F_0296)" in {
    val d = derive(p("x^(1/3)"), x)
    assert(!hasLn(d), s"$d")
    val at8 = d.eval(new Environment().withBinding("x", _Number(8)))
    assert(at8.fold(_ => false, { case _Number(v) => math.abs(v - 1.0 / 12) < 1e-12; case _ => false }), s"$at8")
  }

  it should "do so in exact mode, exactly" in {
    val d = derive(p("x^(1/3)", exact = true), x)
    assert(!hasLn(d), s"$d")
  }

  "a symbolic exponent free of the variable" should "take it too" in {
    val d = derive(p("x^n"), x)
    assert(!hasLn(d), s"$d")
    val v = d.eval(new Environment().withBinding("x", _Number(2)).withBinding("n", _Number(3)))
    assert(v == Right(_Number(12)), s"$v")
  }

  "the limit F_0276 had to decline" should "still not answer 0" in {
    // h^(1/3)/h at 0+: the derivative at 0 is no longer 0 * undefined.
    val r = p("limit(h^(1/3)/h, h, 0, +)").eval(new Environment())
    assert(r != Right(_Number(0)), s"$r")
  }
