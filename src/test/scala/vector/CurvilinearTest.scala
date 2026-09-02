package it.grypho.scala.leonardo
package vector

import core.*
import scalar.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.26 — cylindrical and spherical coordinates for the vector operators.
 *
 *  One general orthogonal-curvilinear evaluator parameterised by the scale factors
 *  `(h₁, h₂, h₃)`, with Cartesian the `(1, 1, 1)` special case — so this suite tests the
 *  *generalisation*, while `VectorCalculusTest` continues to prove Cartesian is unchanged.
 *
 *  The classic closed forms make the sharpest tests, because each is zero for a
 *  non-obvious reason: `∇²(1/r) = 0` in spherical and `∇·(r̂/r²) = 0` are the statements
 *  that a point source is source-free away from the origin, and they fail loudly if a single
 *  scale factor is wrong.
 */
class CurvilinearTest extends AnyFlatSpec:

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  /** A point well away from the coordinate singularities (`r = 0`, `sin θ = 0`). */
  private val point = Seq("r" -> 2.0, "t" -> 0.7, "z" -> 1.3, "p" -> 0.4)

  private def envAt(at: Seq[(String, Double)]): Environment =
    new Environment(variables = at.map((n, d) => n -> _Number(d)).toMap)

  private def scalarAt(src: String, at: Seq[(String, Double)] = point): Double =
    parse(src).eval(envAt(at)) match
      case Right(_Number(d)) => d
      case other             => fail(s"'$src' did not fold to a number: $other")

  private def cellsAt(src: String, at: Seq[(String, Double)] = point): Vector[Double] =
    val en = envAt(at)
    parse(src).eval(en).toExpression match
      case m: _Matrix =>
        m.elems.map(_.eval(en) match
          case Right(_Number(d)) => d
          case other             => fail(s"cell did not fold to a number: $other"))
      case other => fail(s"'$src' did not evaluate to a matrix: $other")

  private def staysSymbolic(src: String): Unit =
    val r = parse(src).eval(new Environment()).toExpression
    assert(r.isInstanceOf[_VectorOperator], s"'$src' should stay symbolic, got $r")

  private def near(got: Double, want: Double, tol: Double = 1e-9): Unit =
    assert(math.abs(got - want) < tol, s"got $got, want $want")

  // ── cylindrical (r, θ, z), scale factors (1, r, 1) ──────────────────────────

  "cylindrical grad" should "divide the angular component by r" in
  {
    // grad(f) = (∂f/∂r, (1/r)·∂f/∂θ, ∂f/∂z);  f = r²·θ  at r=2, θ=0.7
    val g = cellsAt("grad(r^2*t, r, t, z, cylindrical)")
    near(g(0), 2 * 2.0 * 0.7)      // 2rθ
    near(g(1), 2.0)                // (1/r)·r² = r
    near(g(2), 0.0)
  }

  "cylindrical div" should "use the 1/r ∂(r·F_r)/∂r form" in
  {
    // F = (r, 0, 0):  div = (1/r)·∂(r·r)/∂r = 2
    near(scalarAt("div([[r], [0], [0]], r, t, z, cylindrical)"), 2.0)
  }

  it should "make the 2-D point source divergence-free away from the axis" in
  {
    // F = (1/r, 0, 0):  div = (1/r)·∂(r·1/r)/∂r = 0 — wrong scale factors would give -1/r²
    near(scalarAt("div([[1/r], [0], [0]], r, t, z, cylindrical)"), 0.0)
  }

  "the cylindrical laplacian" should "match the classic closed form" in
  {
    // ∇²(r²) = (1/r)·∂/∂r(r·2r) = 4, everywhere
    near(scalarAt("laplacian(r^2, r, t, z, cylindrical)"), 4.0)
    // ∇²(ln r) = 0 away from the axis
    near(scalarAt("laplacian(ln(r), r, t, z, cylindrical)"), 0.0)
  }

  // ── spherical (r, θ, φ) with θ polar, scale factors (1, r, r·sin θ) ─────────

  "the spherical laplacian" should "annihilate 1/r" in
  {
    // ∇²(1/r) = 0 away from the origin — the Newtonian potential is harmonic.
    // A single wrong scale factor breaks this, which is why it is the sharpest test here.
    near(scalarAt("laplacian(1/r, r, t, p, spherical)"), 0.0)
  }

  it should "give the known value for r^2" in
  {
    // ∇²(r²) = (1/r²)·∂/∂r(r²·2r) = 6
    near(scalarAt("laplacian(r^2, r, t, p, spherical)"), 6.0)
  }

  "spherical div" should "make the inverse-square field source-free" in
  {
    // F = (1/r², 0, 0):  div = (1/(r² sinθ))·∂/∂r(r² sinθ · 1/r²) = 0
    near(scalarAt("div([[1/r^2], [0], [0]], r, t, p, spherical)"), 0.0)
  }

  it should "give 3 for the radial field F = (r, 0, 0)" in
  {
    // div = (1/r²)·∂(r²·r)/∂r = 3
    near(scalarAt("div([[r], [0], [0]], r, t, p, spherical)"), 3.0)
  }

  "spherical grad" should "scale the angular components by 1/r and 1/(r sinθ)" in
  {
    val g = cellsAt("grad(r*t*p, r, t, p, spherical)")
    near(g(0), 0.7 * 0.4)                          // ∂f/∂r = θφ
    near(g(1), (2.0 * 0.4) / 2.0)                  // (1/r)·rφ = φ
    near(g(2), (2.0 * 0.7) / (2.0 * math.sin(0.7)))// (1/(r sinθ))·rθ
  }

  // ── the coordinate-free identities, which hold in every orthogonal system ───

  "curl(grad f)" should "vanish in cylindrical and spherical coordinates" in
  {
    for system <- List("cylindrical", "spherical") do
      val third = if system == "cylindrical" then "z" else "p"
      val vals  = cellsAt(s"curl(grad(r^2*t*$third, r, t, $third, $system), r, t, $third, $system)")
      assert(vals.forall(v => math.abs(v) < 1e-8), s"curl(grad f) in $system: $vals")
  }

  "div(curl F)" should "vanish in cylindrical and spherical coordinates" in
  {
    for system <- List("cylindrical", "spherical") do
      val third = if system == "cylindrical" then "z" else "p"
      val f     = s"[[r*$third], [r^2], [t*r]]"
      val v     = scalarAt(s"div(curl($f, r, t, $third, $system), r, t, $third, $system)")
      assert(math.abs(v) < 1e-8, s"div(curl F) in $system: $v")
  }

  "laplacian" should "still equal div(grad f) in a curvilinear system" in
  {
    val direct = scalarAt("laplacian(r^2*t, r, t, z, cylindrical)")
    val viaDiv = scalarAt("div(grad(r^2*t, r, t, z, cylindrical), r, t, z, cylindrical)")
    near(direct, viaDiv)
  }

  // ── refusals ────────────────────────────────────────────────────────────────

  "a curvilinear system with the wrong arity" should "stay symbolic" in
  {
    // cylindrical and spherical are inherently three-dimensional
    staysSymbolic("grad(r*t, r, t, cylindrical)")
    staysSymbolic("div([[r], [0]], r, t, spherical)")
    staysSymbolic("laplacian(r, r, t, z, w, cylindrical)")
  }

  // ── Cartesian is the (1,1,1) special case, and is unchanged ─────────────────

  "the Cartesian formulas" should "be reproduced by the general evaluator" in
  {
    // the generalisation must not perturb the h = 1 case: same results as before 6.26
    val xyz = Seq("x" -> 3.0, "y" -> 5.0)
    near(scalarAt("div([[x^2], [y^3]], x, y)", xyz), 2 * 3.0 + 3 * 25.0)
    assert(cellsAt("grad(x^2*y, x, y)", xyz) == Vector(30.0, 9.0))
    // and the explicit cartesian keyword is the same thing as omitting it
    assert(cellsAt("grad(x^2*y, x, y, cartesian)", xyz) == cellsAt("grad(x^2*y, x, y)", xyz))
  }

  // ── round-trip ──────────────────────────────────────────────────────────────

  "the system suffix" should "round-trip through toString" in
  {
    for src <- List("grad(f, r, t, z, cylindrical)", "div(F, r, t, p, spherical)",
                    "curl(F, r, t, z, cylindrical)", "laplacian(f, r, t, p, spherical)") do
      assert(parse(src).toString == src, s"round-trip failed for $src")
    // Cartesian stays implicit, so 6.24's round-trip form is unchanged
    assert(parse("grad(f, x, y)").toString == "grad(f, x, y)")
    assert(parse("grad(f, x, y, cartesian)").toString == "grad(f, x, y)")
  }

  "the coordinate-system names" should "be reserved words" in
  {
    for name <- List("cartesian", "cylindrical", "spherical") do
      assert(Parser.parse(name).isInstanceOf[Parser.NoSuccess], s"$name should be reserved")
    assert(parse("sphericalHarmonic") == _Variable("sphericalHarmonic"))
  }
