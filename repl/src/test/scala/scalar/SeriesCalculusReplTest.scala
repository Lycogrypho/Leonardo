package it.grypho.scala.leonardo
package scalar

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0320 at the prompt: a stored series differentiates without `simplify` (LT L_0074's case). */
class SeriesCalculusReplTest extends AnyFlatSpec:

  "the REPL" should "differentiate a stored maclaurin series" in {
    val s = Session()
    s.execute("P := maclaurin(sin(x), x, 5)")
    val out = s.execute("derive(P, x)")
    assert(!out.startsWith("derive(") && out.contains("x"), out)
    assert(s.execute("integral(maclaurin(1/(1 + x), x, 3), x)").startsWith("("), s.execute("integral(maclaurin(1/(1 + x), x, 3), x)"))
  }
