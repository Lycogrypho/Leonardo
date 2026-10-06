package it.grypho.scala.leonardo
package parser

import core.*
import scalar.{Sum, Product}
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.abs


/** F_0070 — a leading sign belongs to the FIRST TERM, never to the whole sum.
 *
 *  Until 3.8.2 `expr ::= ["+" | "-"] simpleExpr` applied the sign after the sum was folded,
 *  so `-1 - 1` evaluated to `0` and `-x + y` to `-(x + y)`.  A single leading term was
 *  unaffected, which is why no test caught it: none had the shape `-a ± b`.  These cases pin
 *  the shape at the parse AND the value level, and pin that every single-term form keeps the
 *  exact tree it had — the fix must change nothing that was already right.
 */
class LeadingSignTest extends AnyFlatSpec:

  private val env = new Environment()
  private val xy  = env.withBinding("x", _Number(1)).withBinding("y", _Number(1))

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def value(s: String, e: Environment = env): Double = parse(s).eval(e) match
    case Right(_Number(d)) => d
    case other             => fail(s"\"$s\" did not reduce to a number: $other")

  "a leading minus" should "negate only the first term (the defect's own table)" in
  {
    val cases = List(
      ("-1 - 1", env, -2.0),
      ("-x - y", xy, -2.0),
      ("-x + y", xy, 0.0),
      ("-2*x - 3", xy, -5.0),
      ("-x - y - 1", xy, -3.0),
      ("-(x + y) - 1", xy, -3.0),
      ("-x^2 + 4", env.withBinding("x", _Number(2)), 0.0),
      ("-3 + 3", env, 0.0),
      ("+x - y", xy, 0.0))
    for (s, e, expected) <- cases do
      assert(abs(value(s, e) - expected) < 1e-12, s"\"$s\" gave ${value(s, e)}, expected $expected")
  }

  it should "agree with the same sum written without a leading sign" in
  {
    for (signed, explicit) <- List("-x - y" -> "0 - x - y", "-x + y" -> "0 - x + y",
                                   "-2*x - 3" -> "0 - 2*x - 3") do
      assert(value(signed, xy) == value(explicit, xy), s"$signed vs $explicit")
  }

  it should "hold wherever an expression is read: arguments, parentheses, equations" in
  {
    assert(abs(value("sin(-x + y)", xy)) < 1e-12)
    assert(abs(value("2 * (-x + y)", xy)) < 1e-12)
    assert(parse("-x + 1 = 0").eval(env.withBinding("x", _Number(1))) == Right(_Bool(true)))
    // solve reads the equation it is given: -x + 1 = 0 is x = 1, not x = -1
    parse("solve(-x + 1 = 0, x)").eval(env) match
      case Left(equation._Equation(_, _Number(root))) => assert(abs(root - 1.0) < 1e-12, s"root $root")
      case other                                     => fail(s"unexpected solve result $other")
  }

  it should "stay exact in exact mode" in
  {
    parse("-1/2 + 1/3", Some(30)).eval(env) match
      case Right(r: _Rational) => assert(r == _Rational(-1).divide(_Rational(6)).getOrElse(fail("1/6")))
      case other               => fail(s"expected an exact -1/6, got $other")
  }

  it should "apply to a matrix as the first term" in
  {
    parse("-[[1, 2]] + [[1, 2]]").eval(env) match
      case Right(m: _MatrixValue) => assert(m.toVector == Vector(0.0, 0.0))
      case other                  => fail(s"expected the zero row, got $other")
  }

  "a single leading term" should "keep exactly the tree it had before the fix" in
  {
    // These were right before 3.8.2; the fix must not move them.
    assert(parse("-3k").toString == "(-3.0 * k)")
    assert(parse("-x").toString == "(-1.0 * x)")
    assert(parse("-(a + b)").toString == "(-1.0 * (a + b))")
    assert(parse("-x*y").toString == parse("-(x*y)").toString)
    assert(value("-2^2") == -4.0)
    assert(value("2 - -3") == 5.0)
    assert(value("-2") == -2.0)
  }

  "a sum the library itself prints" should "read back as the same sum" in
  {
    // `simplify`, `consolidate` and the exact tier can produce a sum whose first term is a
    // negative number, and `toString` prints it as `(-3.0 + x)`.  Before 3.8.2 that text read
    // back as `-(3 + x)`, so a session Leonardo had saved itself could reload as a different
    // one -- the round-trip invariant, broken by the parser rather than by the printer.
    val x = _Variable("x")
    for e <- List(Sum(_Number(-3), x), Sum(_Number(-0.5), Product(_Number(2), x))) do
      val back = parse(e.toString)
      assert(back == e, s"${e.toString} read back as $back")
      assert(back.eval(env.withBinding("x", _Number(1))) == e.eval(env.withBinding("x", _Number(1))))
  }

  "every case" should "round-trip through toString" in
  {
    for s <- List("-1 - 1", "-x - y", "-x + y", "-2*x - 3", "-x^2 + 4", "-[[1, 2]] + [[1, 2]]") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"round-trip failed for $s: ${e.toString}")
  }
