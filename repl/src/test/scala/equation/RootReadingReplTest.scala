package it.grypho.scala.leonardo
package equation

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0370: a polynomial whose real roots the root finder cannot read is declined, not answered as having none. */
class RootReadingReplTest extends AnyFlatSpec:

  "solve" should "not answer an inequality on a cubic as true when it lost the cubic's real root" in {
    val s = Session()
    // negative for x < about -1e-308 (x = -1 gives -1e308), so `true` is wrong
    assert(s.execute("solve(x^3 + 1e308*x + 1 > 0, x)") != "true")
    assert(!s.execute("solve(1e-320*x^3 + 1 > 0, x)").contains("NaN"))
    assert(!s.execute("solve(1e-320*x^3 + 1 = 0, x)").contains("NaN"))
  }

  "domain and singularities" should "not report a cubic's pole away" in {
    val s = Session()
    assert(s.execute("domain(1/(x^3 + 1e308*x + 1), x)") != "true")
    assert(s.execute("singularities(1/(x^3 + 1e308*x + 1), x)") != "false")
  }
