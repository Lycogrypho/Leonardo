package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0242 (LT L_0046) -- `solveSystem` with more equations than unknowns.
 *
 *  "Is `b` a combination of these `k` vectors of `R^n`?" is a system of `n` equations in `k`
 *  unknowns with `n > k`, and `solveSystem` declined every such system because it required a
 *  square one.  A consistent over-determined system is now solved; an inconsistent one, a
 *  rank-deficient one and an under-determined one still decline, and say which they are.
 */
class OverdeterminedSystemTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Boolean): _SolveSystem =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x: _SolveSystem, _) => x
      case other                              => fail(s"parse failed for \"$s\": $other")

  private def eval(s: String, exact: Boolean = true): Either[_Expression, _Value] = parse(s, exact).eval(env)
  private def reason(s: String, exact: Boolean = true): Option[String] = parse(s, exact).declineReason(env)

  private def rat(n: Int): _Rational = _Rational(n)
  private val (u, v, m, n) = (_Variable("u"), _Variable("v"), _Variable("m"), _Variable("n"))
  private def declined(r: Either[_Expression, _Value]): Boolean = r.fold(_.isInstanceOf[_SolveSystem], _ => false)

  private val Consistent   = "solveSystem([[u = 1], [v = 2], [u + v = 3]], u, v)"
  private val Inconsistent = "solveSystem([[u = 1], [v = 2], [u + v = 4]], u, v)"

  "an over-determined exact system" should "be solved exactly when it is consistent" in {
    assert(eval(Consistent) == Left(matrix._Matrix(1, 2, Vector(_Equation(u, rat(1)), _Equation(v, rat(2))))))
    // Consistent although no equation is a copy of another: m = 0, n = 1 satisfies all three.
    assert(eval("solveSystem([[2*m + n = 1], [m + 2*n = 2], [m + n = 1]], m, n)") ==
      Left(matrix._Matrix(1, 2, Vector(_Equation(m, rat(0)), _Equation(n, rat(1))))))
  }

  it should "accept the equations as a column, the form that reads like a system on paper" in {
    assert(eval("solveSystem([[u = 1], [v = 2], [u + v = 3]], u, v)") == eval("solveSystem([[u = 1, v = 2, u + v = 3]], u, v)"))
  }

  it should "decline an inconsistent one, naming the first equation that contradicts the ones before it" in {
    assert(declined(eval(Inconsistent)))
    val r = reason(Inconsistent).getOrElse(fail("no reason"))
    assert(r.startsWith("inconsistent") && r.contains("equation 3"), r)
    // The contradiction is found where it arises, not at the end: equation 2 against equation 1.
    val early = reason("solveSystem([[u + v = 1], [u + v = 2], [u = 0]], u, v)").getOrElse(fail("no reason"))
    assert(early.contains("equation 2"), early)
  }

  it should "decline a consistent one that does not determine every unknown" in {
    val s = "solveSystem([[u + v = 1], [2*u + 2*v = 2], [3*u + 3*v = 3]], u, v)"
    assert(declined(eval(s)))
    assert(reason(s).exists(_.startsWith("no unique solution")), reason(s))
  }

  "an over-determined inexact system" should "be solved when it is consistent" in {
    eval(Consistent, exact = false) match
      case Left(matrix._Matrix(1, 2, Vector(_Equation(`u`, x: _Number), _Equation(`v`, y: _Number)))) =>
        assert(math.abs(x.d - 1) < 1e-12 && math.abs(y.d - 2) < 1e-12, (x, y))
      case other => fail(s"$other")
  }

  it should "decline an inconsistent one and say it is inconsistent" in {
    assert(declined(eval(Inconsistent, exact = false)))
    assert(reason(Inconsistent, exact = false).exists(_.startsWith("inconsistent")), reason(Inconsistent, exact = false))
  }

  it should "decide consistency whatever the units of the coefficients" in {
    // Scaled by 1e-8, the inconsistency is 1e-8 in absolute terms: an absolute `=` tolerance would
    // call it consistent.  Measured against each equation's own terms it is still a third.
    assert(declined(eval("solveSystem([[1e-8*u = 1e-8], [1e-8*v = 2e-8], [1e-8*u + 1e-8*v = 4e-8]], u, v)", exact = false)))
    eval("solveSystem([[1e-8*u = 1e-8], [1e-8*v = 2e-8], [1e-8*u + 1e-8*v = 3e-8]], u, v)", exact = false) match
      case Left(matrix._Matrix(1, 2, Vector(_Equation(`u`, x: _Number), _Equation(`v`, y: _Number)))) =>
        assert(math.abs(x.d - 1) < 1e-9 && math.abs(y.d - 2) < 1e-9, (x, y))
      case other => fail(s"$other")
    assert(declined(eval("solveSystem([[1e8*u = 1e8], [1e8*v = 2e8], [1e8*u + 1e8*v = 4e8]], u, v)", exact = false)))
  }

  it should "decline a rank-deficient one" in {
    val s = "solveSystem([[u + v = 1], [2*u + 2*v = 2], [3*u + 3*v = 3]], u, v)"
    assert(declined(eval(s, exact = false)))
    assert(reason(s, exact = false).exists(_.startsWith("no unique solution")), reason(s, exact = false))
  }

  "an under-determined system" should "decline and count its equations and unknowns" in {
    val s = "solveSystem([[u + v + m = 1], [u - v = 0]], u, v, m)"
    assert(declined(eval(s)))
    assert(reason(s).contains("under-determined: 2 equations for 3 unknowns"), reason(s))
  }

  "an over-determined symbolic system" should "decline and say why" in {
    val s = "solveSystem([[u = a], [v = b], [u + v = a + b]], u, v)"
    assert(declined(eval(s)))
    assert(reason(s).exists(_.contains("symbolic")), reason(s))
  }

  "a square system" should "answer as before, and give a reason when singular" in {
    assert(eval("solveSystem([[2*u + v = 5], [u + 3*v = 10]], u, v)", exact = false) ==
      Left(matrix._Matrix(1, 2, Vector(_Equation(u, _Number(1)), _Equation(v, _Number(3))))))
    assert(reason("solveSystem([[2*u + v = 5], [u + 3*v = 10]], u, v)").isEmpty)
    assert(reason("solveSystem([[u + v = 1], [2*u + 2*v = 2]], u, v)").exists(_.startsWith("no unique solution")))
    assert(reason("solveSystem([[u + v = 1], [u + v = 2]], u, v)").exists(_.startsWith("inconsistent")))
  }

  "an exact system with a zero right side" should "stay exact" in {
    // Found on the way: `collect` invented the unit coefficient of a bare `u` in the tier of `u`
    // alone, which holds nothing exact, so `u = 0` sent the whole system down the `Double` path,
    // square systems included.
    assert(eval("solveSystem([[u = 0], [v = 1]], u, v)") ==
      Left(matrix._Matrix(1, 2, Vector(_Equation(u, rat(0)), _Equation(v, rat(1))))))
  }

  "an over-determined inexact system with a zero solution component" should "not read rounding as inconsistency" in {
    // u is 0 exactly; least squares gives it as ~5e-16, a 100% miss of its own term.
    eval("solveSystem([[u + v = 1.5], [u = 0], [v = 1.5]], u, v)", exact = false) match
      case Left(matrix._Matrix(1, 2, Vector(_Equation(`u`, x: _Number), _Equation(`v`, y: _Number)))) =>
        assert(math.abs(x.d) < 1e-12 && math.abs(y.d - 1.5) < 1e-12, (x, y))
      case other => fail(s"$other")
    assert(declined(eval("solveSystem([[u + v = 1], [u + v = 2], [u = 0]], u, v)", exact = false)))
  }

  "a non-linear system" should "say so" in {
    assert(reason("solveSystem([[u*v = 1], [u + v = 2]], u, v)").contains("not linear in the unknowns"))
  }
