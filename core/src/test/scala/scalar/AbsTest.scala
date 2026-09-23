package it.grypho.scala.leonardo
package scalar

import org.scalatest.flatspec.AnyFlatSpec

import core.*
import parser.Parser

/** Issue F_0036 — the absolute value, which the library simply did not have.
 *
 *  Before it, `abs(x)` was a free variable times `x` (the grammar has no unknown-identifier
 *  error), so the editor's `|x|` could only end in a parse error or a silent product.  The
 *  node follows the `Sin` template with one distinction worth pinning: **`abs` is closed over
 *  the rationals**, so the exact arm answers exactly rather than approximating at the working
 *  precision — negating a numerator loses nothing.
 */
class AbsTest extends AnyFlatSpec:

  private def evalOf(src: String, vars: Map[String, _Value] = Map.empty,
                     exact: Option[Int] = None): Either[_Expression, _Value] =
    val p = Parser.parse(src, exact)
    assert(p.successful, s"'$src' must parse: $p")
    p.get.eval(new Environment(5, vars))

  "abs" should "be the absolute value of a real" in
  {
    assert(evalOf("abs(-3)") == Right(_Number(3.0)))
    assert(evalOf("abs(3.5)") == Right(_Number(3.5)))
    assert(evalOf("abs(0)") == Right(_Number(0.0)))
  }

  it should "be the modulus of a complex value" in
  {
    // The entry's other half: |2+3i| is the same notation and the same node.
    assert(evalOf("abs(3 + 4i)") == Right(_Number(5.0)))
  }

  it should "distribute element-wise over a dense matrix, like every _Function" in
  {
    evalOf("abs([[-1, 2], [3, -4]])") match
      case Right(m: _MatrixValue) => assert(m.toVector == Vector(1.0, 2.0, 3.0, 4.0))
      case other                  => fail(s"expected a dense matrix, got $other")
  }

  it should "wrap the cells of a symbolic matrix, each degrading independently" in
  {
    val out = evalOf("abs([[x, -2]])")
    assert(out.isLeft, out)
    val text = out.toExpression.toString
    assert(text.contains("abs(x)") && text.contains("2.0"), text)
  }

  it should "stay EXACT on an exact argument, unlike the transcendentals" in
  {
    // Closed over the rationals: no viaExact round trip, no working-precision approximation.
    evalOf("abs(-1/3)", exact = Some(30)) match
      case Right(r: _Rational) => assert(r.exact == "1/3", r.exact)
      case other               => fail(s"the exact tier was lost: $other")
  }

  it should "stay symbolic on a free variable, and round-trip" in
  {
    val e = Parser.parse("abs(x)")
    assert(e.successful && e.get.toString == "abs(x)")
    assert(Parser.parse(e.get.toString).successful, "the round-trip invariant")
  }

  it should "be a reserved word, so `abs(x)` can never be a silent product again" in
  {
    assert(Parser.ReservedWords.contains("abs"))
  }

  "derive(abs(u), x)" should "be u*u'/abs(u): the sign where defined, a refusal at zero" in
  {
    // The rule folds to ±1 wherever abs is differentiable; at u = 0 it evaluates to 0/0,
    // which stays symbolic -- the undifferentiable point refuses itself, no gate needed.
    assert(evalOf("derive(abs(x), x)", Map("x" -> _Number(2.0))) == Right(_Number(1.0)))
    assert(evalOf("derive(abs(x), x)", Map("x" -> _Number(-2.0))) == Right(_Number(-1.0)))
    assert(evalOf("derive(abs(x), x)", Map("x" -> _Number(0.0))).isLeft,
      "abs is not differentiable at 0, so nothing may fold there")
    // Chain rule: d/dx |x^2 - 4| at x = 1 is (x^2-4)*2x/|x^2-4| = -2.
    assert(evalOf("derive(abs(x^2 - 4), x)", Map("x" -> _Number(1.0))) == Right(_Number(-2.0)))
  }

  "the differentiability analysis" should "exclude zero, via the derivative's own denominator" in
  {
    // differentiableDomainOf collects the constraints of derive(e, v) too, and the rule's
    // shape u*u'/abs(u) puts abs(u) in a Ratio denominator -- so the NonZero constraint
    // arrives from the existing Ratio rule with no abs-specific arm in Domain.scala.
    val e  = Parser.parse("abs(x)").get
    val cs = differentiableDomainOf(e, _Variable("x"), DomainKind.Real,
                                    new Environment(5, Map.empty)).constraints
    assert(cs.exists(c => c.req == Requirement.NonZero && c.arg.toString == "abs(x)"),
      s"expected an 'abs(x) != 0' constraint, got: $cs")
  }
