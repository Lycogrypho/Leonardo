package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.abs


/** F_0071 — `derive` builds every constant it invents in the tier of the expression it is
 *  differentiating (the numeric tier rule in CLAUDE.md).
 *
 *  Before the fix the literal-exponent power rule read an exact exponent through the widening
 *  extractor and rebuilt it as a `Double`, so `derive(x^2, x)` was `2.0 * x` in exact mode and
 *  every exact computation downstream of a derivative fell out of the exact tier.  The checks
 *  are structural (no inexact leaf anywhere in the result) and numerical (the same value as
 *  the `Double`-mode derivative), and the `Double` path is pinned unchanged.
 */
class DeriveExactTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def parse(s: String, exact: Boolean): _Expression =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  /** Every leaf that is an INEXACT number -- a type test, not the widening extractor. */
  private def inexactLeaves(e: _Expression): List[_Expression] = e match
    case n: _Number => List(n)
    case other      => other.children.flatMap(inexactLeaves)

  private def valueAt(e: _Expression, at: Double): Double =
    e.eval(env.withBinding("x", _Number(at))) match
      case Right(_Number(d)) => d
      case other             => fail(s"$e did not evaluate at x = $at: $other")

  // Every case CONTAINS an exact value: the tier is read from the expression, and one with
  // no exact value (`cos(x)`, bare `x`) is differentiated in the Double tier by design --
  // see the second case below.
  private val cases = List("x^2", "x^3 + 2*x", "3*x^2 - x + 7", "2/x", "x^(1/2)",
                           "cos(2*x)", "sin(x)^2", "asin(x/2)", "acos(x/2)", "atan(3*x)",
                           "atanh(x/2)", "csc(2*x)", "cot(2*x)", "5")

  "derive in exact mode" should "invent no inexact constant" in
  {
    // derive's OWN output: `simplifyFully` has the same defect (it folds `x*x` into `x^2.0`),
    // filed separately as F_0073, so passing the result through it would test that instead.
    for s <- cases do
      val d = derive(parse(s, exact = true), x)
      assert(inexactLeaves(d).isEmpty, s"d/dx $s = $d carries ${inexactLeaves(d)}")
  }

  it should "take its tier from the expression, so a bare variable stays in the Double tier" in
  {
    // `x` holds no exact value even in exact mode -- the tier is decided by what the
    // expression contains, which is what keeps the Double path byte-identical.
    assert(derive(parse("x", exact = true), x) == _Number(1))
    assert(derive(parse("x + 2", exact = true), x) == _Rational(1))
  }

  it should "have the same value as the Double-mode derivative" in
  {
    for s <- cases; at <- List(0.3, 0.7) do
      val exact   = valueAt(derive(parse(s, exact = true), x), at)
      val inexact = valueAt(derive(parse(s, exact = false), x), at)
      assert(abs(exact - inexact) < 1e-12, s"d/dx $s at $at: exact $exact, Double $inexact")
  }

  it should "answer the textbook cases in the exact tier" in
  {
    assert(derive(parse("5", exact = true), x) == _Rational(0))
    // d/dx x^3 at x = 1/3 is 3 * (1/3)^2 = 1/3, and stays a fraction
    derive(parse("x^3", exact = true), x).eval(env.withBinding("x", _Rational(1).divide(_Rational(3))
        .getOrElse(fail("1/3")))) match
      case Right(r: _Rational) => assert(r == _Rational(1).divide(_Rational(3)).getOrElse(fail("1/3")))
      case other               => fail(s"expected an exact 1/3, got $other")
  }

  "derive in Double mode" should "be unchanged" in
  {
    // No expression without an exact value may gain one: the Double path is byte-identical.
    for s <- cases do
      val d = derive(parse(s, exact = false), x)
      assert(!_Rational.containsExact(d), s"d/dx $s = $d gained an exact constant")
    assert(derive(parse("x^2", exact = false), x).toString == "(2.0 * x)")
  }
