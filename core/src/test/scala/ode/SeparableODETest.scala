package it.grypho.scala.leonardo
package ode

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.6 — separation of variables for `y' = f(t)·g(y)`.
 *
 *  Checked against the **analytic** solution rather than against RK4: RK4 is the fallback
 *  this tier is meant to replace, so agreeing with it would only prove the two are
 *  consistent, not that either is right.
 */
class SeparableODETest extends AnyFlatSpec:

  private val env = new Environment()

  private def solve(src: String): Option[Double] =
    Parser.parse(src) match
      case Parser.Success(e, _) =>
        e.eval(env) match
          case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
          case _                                              => None
      case other => fail(s"parse failed for '$src': $other")

  private def close(got: Option[Double], expected: Double, tol: Double = 1e-4): Unit =
    assert(got.exists(d => math.abs(d - expected) < tol),
           s"expected ~$expected, got $got")

  // ── the classic separable cases ────────────────────────────────────────────

  "y' = y^2" should "match the analytic -1/(t + C)" in
  {
    // y(0) = 1  ->  y = 1/(1 - t);  at t = 0.5 that is 2.
    close(solve("ode(y^2, y, t, 0, 1, 0.5)"), 2.0)
  }

  it should "agree at a second point" in
  {
    // y = 1/(1 - t) at t = 0.25 is 4/3.
    close(solve("ode(y^2, y, t, 0, 1, 0.25)"), 4.0 / 3.0)
  }

  "y' = t/y" should "match the analytic sqrt(t^2 + C)" in
  {
    // y(0) = 1  ->  y^2 = t^2 + 1  ->  y = sqrt(t^2 + 1);  at t = 1 that is sqrt(2).
    close(solve("ode(t / y, y, t, 0, 1, 1)"), math.sqrt(2.0))
  }

  "y' = t*y^2" should "handle a genuine product of both variables" in
  {
    // y(0) = 1  ->  -1/y = t^2/2 - 1  ->  y = 1/(1 - t^2/2);  at t = 1 that is 2.
    close(solve("ode(t * y^2, y, t, 0, 1, 1)"), 2.0)
  }

  // ── the equilibrium trap ───────────────────────────────────────────────────

  "an equilibrium initial condition" should "give the constant solution, not a separated one" in
  {
    // y' = y^2 with y(0) = 0: g(y0) = 0, so y stays 0 forever. Separation would divide by
    // zero here and the integrals would still produce *something*.
    close(solve("ode(y^2, y, t, 0, 0, 5)"), 0.0)
  }

  it should "hold for a shifted equilibrium" in
  {
    // y' = y*(y-2), y(0) = 2 is an equilibrium.
    close(solve("ode(y * (y - 2), y, t, 0, 2, 3)"), 2.0)
  }

  // ── the tier must not damage what already worked ───────────────────────────

  "the linear tier" should "still take precedence" in
  {
    // y' = 2y is linear; the constant-coefficient closed form must still handle it.
    close(solve("ode(2*y, y, t, 0, 1, 1)"), math.exp(2.0))
  }

  "a pure function of t" should "still integrate directly" in
  {
    // y' = 2t, y(0) = 0  ->  y = t^2;  at t = 3 that is 9.
    close(solve("ode(2*t, y, t, 0, 0, 3)"), 9.0)
  }

  "a non-separable equation" should "still fall through to RK4 rather than break" in
  {
    // y' = t + y is linear, so it is handled above the separable tier; y' = sin(t*y) is
    // neither, and must still produce the numeric fallback.
    assert(solve("ode(sin(t*y), y, t, 0, 1, 0.5)").isDefined,
           "a non-separable equation should still be solved numerically")
  }

  // ── branch selection ───────────────────────────────────────────────────────

  "a negative initial condition" should "stay on its own branch" in
  {
    // y' = t/y with y(0) = -1  ->  y = -sqrt(t^2 + 1), NOT +sqrt.
    close(solve("ode(t / y, y, t, 0, -1, 1)"), -math.sqrt(2.0))
  }
