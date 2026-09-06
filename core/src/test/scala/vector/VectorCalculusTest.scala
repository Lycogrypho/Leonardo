package it.grypho.scala.leonardo
package vector

import core.*
import scalar.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.24 — vector calculus: `grad`, `div`, `curl`, `laplacian`, `jacobian`, `hessian`.
 *
 *  The operators are compositions of `derive`, so the interesting content is **the shape
 *  rules and the refusals**, plus the coordinate-free identities, which are the strong
 *  property tests: `curl(grad f) = 0` and `div(curl F) = 0` hold for every field, so they
 *  catch a sign or index slip that a single worked example would not.
 */
class VectorCalculusTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")
  private val y   = _Variable("y")
  private val z   = _Variable("z")

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  /** Evaluates and returns the resulting expression (matrix results stay `Left`). */
  private def evaluated(src: String): _Expression = parse(src).eval(env).toExpression

  /** Every cell of an evaluated matrix result, at the given point. */
  private def cellsAt(src: String, at: (String, Double)*): Vector[Double] =
    val en = new Environment(variables = at.map((n, d) => n -> _Number(d)).toMap)
    parse(src).eval(en).toExpression match
      case m: _Matrix =>
        m.elems.map(_.eval(en) match
          case Right(_Number(d)) => d
          case other             => fail(s"cell did not fold to a number: $other"))
      case other => fail(s"'$src' did not evaluate to a matrix: $other")

  private def scalarAt(src: String, at: (String, Double)*): Double =
    val en = new Environment(variables = at.map((n, d) => n -> _Number(d)).toMap)
    parse(src).eval(en) match
      case Right(_Number(d)) => d
      case other             => fail(s"'$src' did not fold to a number: $other")

  private def staysSymbolic(src: String): Unit =
    val r = parse(src).eval(env).toExpression
    assert(r.isInstanceOf[_Grad] || r.isInstanceOf[_Div] || r.isInstanceOf[_Curl] ||
           r.isInstanceOf[_Laplacian] || r.isInstanceOf[_Jacobian] || r.isInstanceOf[_Hessian],
           s"'$src' should stay symbolic, got $r")

  // ── grad ────────────────────────────────────────────────────────────────────

  "grad" should "give the column vector of partial derivatives" in
  {
    // grad(x^2*y) = [2xy, x^2]^T ; at (3, 5) -> [30, 9]
    assert(cellsAt("grad(x^2*y, x, y)", "x" -> 3.0, "y" -> 5.0) == Vector(30.0, 9.0))
  }

  it should "have the shape n x 1" in
  {
    evaluated("grad(x*y*z, x, y, z)") match
      case m: _Matrix => assert(m.rows == 3 && m.cols == 1, s"got ${m.rows}x${m.cols}")
      case other      => fail(s"expected a 3x1 matrix, got $other")
  }

  // ── div ─────────────────────────────────────────────────────────────────────

  "div" should "sum the diagonal partials of a column field" in
  {
    // div([x^2, y^3]) = 2x + 3y^2 ; at (2, 3) -> 4 + 27 = 31
    assert(scalarAt("div([[x^2], [y^3]], x, y)", "x" -> 2.0, "y" -> 3.0) == 31.0)
  }

  it should "refuse a component/coordinate count mismatch" in
  {
    staysSymbolic("div([[x], [y], [z]], x, y)")
  }

  // ── curl ────────────────────────────────────────────────────────────────────

  "curl" should "compute the standard 3-D curl" in
  {
    // F = [-y, x, 0] is the rigid rotation; curl F = [0, 0, 2]
    assert(cellsAt("curl([[-y], [x], [0]], x, y, z)", "x" -> 1.0, "y" -> 1.0, "z" -> 1.0)
             == Vector(0.0, 0.0, 2.0))
  }

  it should "be refused outside three dimensions" in
  {
    // in 2-D the natural object is a SCALAR curl -- a different result type, so refuse
    staysSymbolic("curl([[-y], [x]], x, y)")
    staysSymbolic("curl([[x], [y], [z], [x]], x, y, z, w)")
  }

  // ── the coordinate-free identities ──────────────────────────────────────────

  "curl(grad f)" should "vanish identically" in
  {
    val vals = cellsAt("curl(grad(x^2*y + y*z^3 + x*z, x, y, z), x, y, z)",
                       "x" -> 1.7, "y" -> -0.4, "z" -> 2.3)
    assert(vals.forall(v => math.abs(v) < 1e-9), s"curl(grad f) should be 0, got $vals")
  }

  "div(curl F)" should "vanish identically" in
  {
    val v = scalarAt("div(curl([[x*y], [y*z^2], [x^2*z]], x, y, z), x, y, z)",
                     "x" -> 1.3, "y" -> 0.7, "z" -> -1.1)
    assert(math.abs(v) < 1e-9, s"div(curl F) should be 0, got $v")
  }

  "laplacian" should "agree with div(grad f)" in
  {
    val pt = Seq("x" -> 1.4, "y" -> -0.6, "z" -> 0.9)
    val direct = scalarAt("laplacian(x^3*y + y^2*z, x, y, z)", pt*)
    val viaDiv = scalarAt("div(grad(x^3*y + y^2*z, x, y, z), x, y, z)", pt*)
    assert(math.abs(direct - viaDiv) < 1e-9, s"laplacian $direct vs div(grad) $viaDiv")
    // and the closed form: laplacian(x^3 y + y^2 z) = 6xy + 2z
    assert(math.abs(direct - (6 * 1.4 * -0.6 + 2 * 0.9)) < 1e-9)
  }

  // ── jacobian / hessian ──────────────────────────────────────────────────────

  "jacobian" should "be m x n with dFi/dxj in row-major order" in
  {
    // F = [x*y, y^2]; J = [[y, x], [0, 2y]] ; at (3, 5) -> [5, 3, 0, 10]
    assert(cellsAt("jacobian([[x*y], [y^2]], x, y)", "x" -> 3.0, "y" -> 5.0)
             == Vector(5.0, 3.0, 0.0, 10.0))
  }

  "hessian" should "be n x n and symmetric (Clairaut)" in
  {
    // f = x^2*y^3 ; H = [[2y^3, 6xy^2], [6xy^2, 6x^2 y]]
    val h = cellsAt("hessian(x^2*y^3, x, y)", "x" -> 2.0, "y" -> 3.0)
    assert(h == Vector(2 * 27.0, 6 * 2 * 9.0, 6 * 2 * 9.0, 6 * 4 * 3.0), s"got $h")
    assert(h(1) == h(2), "the mixed partials must agree")
  }

  // ── refusals ────────────────────────────────────────────────────────────────

  "a duplicate coordinate" should "be refused" in
  {
    staysSymbolic("grad(x*y, x, x)")
  }

  "a non-matrix field" should "keep div symbolic" in
  {
    staysSymbolic("div(x^2, x, y)")
  }

  "a row-vector field" should "be refused by div (a field is n x 1)" in
  {
    staysSymbolic("div([[x, y]], x, y)")
  }

  // ── round-trip ──────────────────────────────────────────────────────────────

  "every operator" should "round-trip through toString" in
  {
    for src <- List("grad(x, x, y)", "div(F, x, y)", "curl(F, x, y, z)",
                    "laplacian(x, x, y)", "jacobian(F, x, y)", "hessian(x, x, y)") do
      assert(parse(src).toString == src, s"round-trip failed for $src")
  }

  "the operator names" should "be reserved words" in
  {
    for name <- List("grad", "div", "curl", "laplacian", "jacobian", "hessian") do
      assert(Parser.parse(name).isInstanceOf[Parser.NoSuccess], s"$name should be reserved")
    // but a name merely starting with one stays an ordinary variable
    assert(parse("gradient") == _Variable("gradient"))
    assert(parse("divisor") == _Variable("divisor"))
  }

  // ── iterated integration, which already works and must keep working ─────────
  // Pinned here because it is emergent, not designed: _DefIntegral's LIMITS are children
  // while its variable is a binder, so an inner limit referring to the outer variable is an
  // ordinary free occurrence, bound per sample by the outer Simpson pass.

  "an iterated integral with a variable inner limit" should "evaluate" in
  {
    // int_0^1 int_0^x x*y dy dx = int_0^1 x^3/2 dx = 1/8
    assert(math.abs(scalarAt("integral(integral(x*y, y, 0, x), x, 0, 1)") - 0.125) < 1e-6)
  }

  it should "evaluate with constant limits too" in
  {
    // int_0^1 int_0^2 x*y dy dx = 1
    assert(math.abs(scalarAt("integral(integral(x*y, y, 0, 2), x, 0, 1)") - 1.0) < 1e-6)
  }
