package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0073 — `simplify` builds every constant it invents in the tier of the expression it is
 *  simplifying, as F_0071 made `derive` do; and the callers that test a simplified value for
 *  zero or one compare by VALUE, so an exact zero is recognised.
 */
class SimplifyExactTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Boolean): _Expression =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  /** Every INEXACT number in the tree -- a type test, not the widening extractor. */
  private def inexactLeaves(e: _Expression): List[_Expression] = e match
    case n: _Number => List(n)
    case other      => other.children.flatMap(inexactLeaves)

  private def half: _Rational = _Rational(1).divide(_Rational(2)).getOrElse(fail("1/2"))

  "simplifyFully in exact mode" should "invent no inexact constant (the defect's own table)" in
  {
    for s <- List("x*x + 1/3", "x*x*x*2", "x + x + 1/3", "x - x + 1/2", "cos(0*x) + 1/2",
                  "exp(0*x) * (1/2)", "x^0 + 1/2", "ln(1) + x/2") do
      val r = simplifyFully(parse(s, exact = true))
      assert(inexactLeaves(r).isEmpty, s"simplify($s) = $r carries ${inexactLeaves(r)}")
  }

  it should "keep x/x + 1/2 an exact 3/2, not the Double 1.5" in
  {
    simplifyFully(parse("x/x + 1/2", exact = true)) match
      case r: _Rational => assert(r == _Rational(3).divide(_Rational(2)).getOrElse(fail("3/2")))
      case other        => fail(s"expected an exact 3/2, got $other")
  }

  it should "not read 0/0 as 0 when the zero denominator is exact" in
  {
    // The zero-numerator rule tested its denominator with `!= _Number(0)`, an identity
    // comparison an exact zero passes -- so 0/0 simplified to 0.
    val r = simplifyFully(parse("0/(1/2 - 1/2)", exact = true))
    assert(r match { case _Number(0.0) => false; case _ => true }, s"0/0 simplified to $r")
  }

  "simplifyFully in Double mode" should "be unchanged" in
  {
    assert(simplifyFully(parse("x*x", exact = false)).toString == "(x ^ 2.0)")
    assert(simplifyFully(parse("x + x", exact = false)).toString == "(2.0 * x)")
    for s <- List("x*x + 1", "x - x + 2", "cos(0*x)", "x/x") do
      assert(!_Rational.containsExact(simplifyFully(parse(s, exact = false))), s)
  }

  "a caller testing a simplified value for zero" should "recognise an exact zero" in
  {
    // solveSystem's symbolic path: the second pivot is a - a, an exact zero in exact mode.
    // An identity comparison missed it and the elimination divided by zero.
    parse("solveSystem([[x + a*y = 1], [x + a*y = 2]], x, y)", exact = true).eval(env) match
      case Left(_: equation._SolveSystem) => succeed
      case other                          => fail(s"a singular system must stay symbolic, got $other")
    // ode's constant-coefficient tier: y' = 3/2 has a = 0, read as zero only by value.
    parse("ode(3/2, y, t, 0, 1, 2)", exact = true).eval(env) match
      case Right(_Number(d)) => assert(math.abs(d - 4.0) < 1e-12, s"y(2) = $d")
      case other             => fail(s"y' = 3/2, y(0) = 1 must give y(2) = 4, got $other")
  }
