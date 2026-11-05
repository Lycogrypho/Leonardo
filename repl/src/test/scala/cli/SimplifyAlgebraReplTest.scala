package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** F_0292 (LT L_0053): the `simplify` command collects like terms, reads `sin^2 + cos^2` and cancels a
 *  common factor. */
class SimplifyAlgebraReplTest extends AnyFlatSpec:

  "simplify" should "apply the Pythagorean identity (F_0292)" in {
    val s = new Session()
    assert(s.execute("simplify cos(t)^2 + sin(t)^2") == "1.0")
    assert(s.execute("simplify det(jacobian([[r*cos(t)], [r*sin(t)]], r, t))") == "r")
  }

  it should "cancel a common factor" in {
    val s = new Session()
    val q = s.execute("simplify ((x + h)^2 - x^2)/h")
    assert(!q.contains("/"), q)
    assert(s.execute("simplify div([[r], [0], [0]], r, t, p, spherical)") == "3.0")
  }

  "derive" should "reach 0 on an identity" in {
    val s = new Session()
    assert(s.execute("derive(sin(x)^2 + cos(x)^2, x)") == "0.0")
  }
