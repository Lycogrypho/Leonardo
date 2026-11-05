package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0291 (LT L_0058): a `sum` with a symbolic bound in closed form, checked by binding the bound and
 *  comparing with the term-by-term sum. */
class ClosedSumTest extends AnyFlatSpec:

  private def parse(s: String, exact: Boolean = false): _Expression =
    val p = Parser.parse(s, if exact then Some(30) else None)
    assert(p.successful, s"parse failed: $s")
    p.get

  private val env = new Environment()

  /** The closed form of `src`, verified at `n` against the same sum with `n` bound. */
  private def closesAt(src: String, bound: String, at: Int*): _Expression =
    val closed = parse(src).eval(env) match
      case Left(r: _Reduction) => fail(s"$src stayed unevaluated: $r")
      case other               => other.toExpression
    assert(!closed.toString.contains("sum("), s"$src: $closed")
    at.foreach { n =>
      val bound_ = env.withBinding(bound, _Number(n))
      val want = parse(src).eval(bound_) match
        case Right(_Number(d)) => d
        case other             => fail(s"$src at $n: $other")
      closed.eval(bound_) match
        case Right(_Number(d)) => assert(math.abs(d - want) <= 1e-9 * math.max(1.0, math.abs(want)), s"$src at $n: $d, want $want")
        case other             => fail(s"$closed at $n: $other")
    }
    closed

  "a polynomial summand" should "sum in closed form over a symbolic bound (F_0291)" in {
    closesAt("sum(k, k, 1, n)", "n", 1, 4, 10)
    closesAt("sum(k^2, k, 1, n)", "n", 1, 4, 10)
    closesAt("sum(k^5 - 2*k^3 + 7, k, 1, n)", "n", 0, 3, 9)
    closesAt("sum(3*k + 2, k, 2, m)", "m", 2, 5, 11)
    closesAt("sum(1, k, 1, n)", "n", 0, 6)
  }

  it should "carry factors free of the index (a Riemann sum)" in {
    closesAt("sum((k/n)^2/n, k, 1, n)", "n", 1, 10, 100)
    closesAt("sum(a*k^2, k, 1, n)", "n") // checked symbolically only
  }

  it should "leave a Riemann sum whose limit the library can take" in {
    val l = parse("limit(sum((k/n)^2/n, k, 1, n), n, inf)").eval(env)
    l match
      case Right(_Number(d)) => assert(math.abs(d - 1.0 / 3) < 1e-9, s"$d")
      case other             => fail(s"$other")
  }

  "a geometric summand" should "sum in closed form when the ratio is not 1" in {
    closesAt("sum(2^k, k, 0, n)", "n", 0, 3, 10)
    closesAt("sum((1/2)^k, k, 1, n)", "n", 1, 3, 20)
    closesAt("sum(3*2^(k + 1), k, 1, n)", "n", 1, 5)
    closesAt("sum(exp(-k), k, 0, n)", "n", 0, 4)
  }

  it should "stay unevaluated when the ratio may be 1" in {
    assert(parse("sum(x^k, k, 0, n)").eval(env).toExpression.isInstanceOf[_Reduction])
  }

  "anything else" should "stay unevaluated" in {
    assert(parse("sum(1/k, k, 1, n)").eval(env).toExpression.isInstanceOf[_Reduction])
    assert(parse("sum(sin(k), k, 1, n)").eval(env).toExpression.isInstanceOf[_Reduction])
    assert(parse("product(k, k, 1, n)").eval(env).toExpression.isInstanceOf[_Reduction])
  }

  "an exact sum" should "keep exact coefficients" in {
    val closed = parse("sum(k^2, k, 1, n)", exact = true).eval(env.withExactMode(true)).toExpression
    def hasDouble(e: _Expression): Boolean = e match
      case _: _Number => true
      case other      => other.children.exists(hasDouble)
    assert(!closed.isInstanceOf[_Reduction] && !hasDouble(closed), s"$closed")
  }

  "numeric bounds" should "sum term by term as before" in {
    assert(parse("sum(k, k, 1, 10)").eval(env) == Right(_Number(55)))
  }
