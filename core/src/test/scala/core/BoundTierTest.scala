package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0171 phase A — a body written in letters bound to exact values computes exactly (V3).
 *
 *  The tier of an invented constant was read from content alone, and `a/t` holds no number:
 *  with `a := 1/3`, `t := 2` in exact mode, `derive(a/t, t)` answered `-0.08333`.  The tier is now
 *  read from the content **and** the bound values of its free names.
 */
class BoundTierTest extends AnyFlatSpec:

  private def rat(n: Int, d: Int): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail(s"$n/$d"))

  /** An environment binding each name to an exact value. */
  private def exactEnv(bindings: (String, _Rational)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, kv._2))

  private def eval(s: String, env: Environment, exact: Boolean = true): Either[_Expression, _Value] =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x, _) => x.eval(env)
      case other                => fail(s"parse failed for \"$s\": $other")

  /** Every numeric leaf that is not exact. */
  private def inexact(e: _Expression): List[_Expression] = e match
    case _: _Rational => Nil
    case n: _Number   => List(n)
    case other        => other.children.flatMap(inexact)

  private val env = exactEnv("a" -> rat(1, 3), "b" -> rat(2, 7), "c" -> rat(1, 5), "d" -> rat(3, 1), "t" -> rat(2, 1))

  "derive" should "stay exact on a body of letters bound to exact values" in {
    assert(eval("derive(a/t, t)", env) == Right(rat(-1, 12)))
  }

  "solve" should "stay exact on letters bound to exact values" in {
    assert(eval("solve(a*x + b = 0, x)", env) == Left(equation._Equation(_Variable("x"), rat(-6, 7))))
  }

  it should "solve an inequality exactly" in {
    eval("solve(a*x > b, x)", env) match
      case Left(r) => assert(inexact(r).isEmpty && r.toString.contains("6/7"), r)
      case other   => fail(s"$other")
  }

  "solveSystem" should "stay exact on letters bound to exact values" in {
    eval("solveSystem([[a*x + y = c], [x - y = d]], x, y)", env) match
      case Left(m: matrix._Matrix) =>
        assert(m.elems == Vector(equation._Equation(_Variable("x"), rat(12, 5)),
                                 equation._Equation(_Variable("y"), rat(-3, 5))), m)
      case other => fail(s"$other")
  }

  "sum" should "bind its index exactly when its body's letters are exact" in {
    assert(eval("sum(a*k, k, 1, 3)", env) == Right(rat(2, 1)))
  }

  "taylor" should "expand with exact coefficients when its body's letters are exact" in {
    eval("taylor(exp(a*x), x, 0, 2)", env) match
      case Left(p) => assert(inexact(p).isEmpty, p)
      case other   => fail(s"$other")
  }

  "stationary" should "find the stationary point exactly when the coefficients are exact" in {
    eval("stationary(a*x^2 + b*x, x)", env) match
      case Left(r) => assert(inexact(r).isEmpty && r.toString.contains("3/7"), r)
      case other   => fail(s"$other")
  }

  "a Double binding" should "still demote, by float contagion" in {
    eval("derive(a/t, t)", env.withBinding("a", _Number(0.5))) match
      case Right(_: _Rational) => fail("a Double binding came back exact")
      case Right(_Number(d))   => assert(d == -0.125)
      case other               => fail(s"$other")
  }

  "a session with no exact value" should "compute exactly as before" in {
    val doubles = new Environment().withBinding("a", _Number(0.5)).withBinding("t", _Number(2))
    assert(eval("derive(a/t, t)", doubles, exact = false) == Right(_Number(-0.125)))
    eval("derive(a/t, t)", new Environment(), exact = false) match
      case Left(r) => assert(inexact(r).nonEmpty, r)
      case other   => fail(s"$other")
  }
