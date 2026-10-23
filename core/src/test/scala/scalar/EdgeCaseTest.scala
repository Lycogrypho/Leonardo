package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the scalar layer: each case failed before its fix. */
class EdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)
  private def number(r: Either[_Expression, _Value]): Double = r match
    case Right(_Number(d)) => d
    case other             => fail(s"not a number: $other")

  "laurent" should "decline a pole order past the cap instead of overflowing the stack (F_0180)" in {
    val e = Ratio(_Number(1), Power(x, _Number(1000)))
    laurentDetailed(e, x, _Number(0), 1, 0, env) match
      case Left(why) => assert(why.contains("at most"), why)
      case Right(s)  => fail(s"answered $s")
    assert(ev("laurent(1/x^1000, x, 0, 1, 0)").isLeft)
  }

  "rootsOfSquareFree" should "keep four real roots near 1e-11, not read the factor as even (F_0193)" in {
    val roots = Vector(1e-11, 2e-11, 3e-11, 4e-11)
    val poly  = roots.foldLeft(Vector(1.0))((p, r) => polyMul(p, Vector(-r, 1.0)))
    val found = rootsOfSquareFree(poly).getOrElse(fail("no roots"))
    assert(found.size == 4)
    assert(found.forall((_, im) => math.abs(im) <= 1e-9 * 4e-11), found)
    assert(roots.forall(r => found.exists((re, _) => math.abs(re - r) <= 1e-6 * r)), found)
  }

  it should "solve a quadratic without cancellation in the small root (F_0193)" in {
    val found = rootsOfSquareFree(Vector(1.0, 1e8, 1.0)).getOrElse(fail("no roots")).map(_._1)
    assert(found.exists(r => math.abs(r - -1e-8) <= 1e-12 * 1e-8), found)
    assert(found.exists(r => math.abs(r - -1e8) <= 1e-12 * 1e8), found)
  }

  "singularities" should "find the four small-scale poles (F_0193)" in {
    ev("singularities(1/((x - 1e-11)*(x - 2e-11)*(x - 3e-11)*(x - 4e-11)), x)") match
      case Right(m: _MatrixValue)  => assert(m.cols == 4)
      case Left(m: _MatrixShaped)  => assert(m.cols == 4)
      case other                   => fail(s"got $other")
  }

  "limit" should "not read the value at a jump (F_0192)" in {
    assert(number(ev("limit(step(x), x, 0, -)")) == 0.0)
    assert(number(ev("limit(step(x), x, 0, +)")) == 1.0)
    assert(ev("limit(step(x), x, 0)").isLeft)
    assert(number(ev("limit(step(x - 1), x, 0)")) == 0.0)
  }

  "simplify" should "not turn a symbolic matrix to the power 0 into the scalar 1 (F_0198)" in {
    assert(!simplifyFully(Parser.parse("[[x, 1], [0, x]]^0").get).isInstanceOf[_Number])
    assert(simplifyFully(Parser.parse("x^0").get) == _Number(1))
  }

  "domain" should "keep a large-magnitude cut point's side (F_0194)" in {
    val d = ev("domain(ln(x - 1e20), x)")
    assert(!d.contains(_Bool(false)), d)
    assert(d.fold(_.toString, _.toString).contains(">"), d)
  }

  "derive" should "differentiate a constant with no rule of its own to zero (F_0204)" in {
    assert(math.abs(number(derive(Parser.parse("Beta(2, 3)*x").get, x).eval(env)) - 1.0 / 12) < 1e-12)
    assert(number(derive(Parser.parse("fib(5)*x").get, x).eval(env)) == 5.0)
  }

  "a matrix power" should "decline an exponent past the integer range (F_0197)" in {
    assert(ev("[[1, 1], [0, 1]]^(-1e19)").isLeft)
    assert(ev("[[1, 1], [0, 1]]^(-3)") == ev("[[1, -3], [0, 1]]"))
  }

  "an exact literal" should "not build a power of ten past the exact power cap (F_0181)" in {
    assert(_Rational.fromDecimalString("1e400000").isEmpty)
    assert(_Rational.fromDecimalString("1e-400000").isEmpty)
    assert(_Rational.fromDecimalString("1e300").isDefined)
    assert(_Rational.fromDecimalString("0.1").map(r => r.add(r).add(r)) == _Rational.fromDecimalString("0.3"))
  }
