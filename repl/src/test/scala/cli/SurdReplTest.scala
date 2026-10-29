package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0231 (LT L_0010): exact mode answers a square root as the surd it is, displays it as a reader
 *  writes it, reads that display back, and saves it in ASCII. */
class SurdReplTest extends AnyFlatSpec:

  private def exact: Session =
    val s = Session()
    s.execute("exact on")
    s

  "the course's examples" should "answer surds, not decimals" in {
    val s = exact
    assert(s.execute("solve(1 - 2*k^2 = 0, k)") == "k_1 := -√2/2\nk_2 := √2/2")
    assert(s.execute("norm([[2], [3]])") == "√13")
  }

  "surd arithmetic" should "stay exact over one radical" in {
    val s = exact
    assert(s.execute("√13 * √13") == "13")
    assert(s.execute("2√13 + √13") == "3√13")
    assert(s.execute("(1 + √2)^2") == "(3 + 2√2)")
    assert(s.execute("1/(1 + √2)") == "(-1 + √2)")
    assert(s.execute("abs(1 - √2)") == "(-1 + √2)")
    assert(s.execute("2^(3/2)") == "2√2")
    assert(s.execute("stddev([[1, 2, 3, 4]])") == "√15/3")
    assert(s.execute("solve(x^2 - 2*x - 1 = 0, x)") == "x_1 := (1 - √2)\nx_2 := (1 + √2)")
  }

  it should "leave the field the way every irrational did: a working-precision decimal" in {
    val s = exact
    assert(s.execute("√2 + √3") == "3.14626")
    assert(s.execute("sin(√2)") == "0.98777")
  }

  "the display" should "read back as the same value" in {
    val s = exact
    s.execute("x := √2/2")
    assert(s.execute("x * 2") == "√2")
    assert(s.execute("(1 - √2) * (1 + √2)") == "-1")
    assert(s.execute("√2 < 3/2") == "true")
  }

  ":save" should "write ASCII that reloads to the same surds" in {
    val s = exact
    s.execute("solve(x^2 - 2*x - 1 = 0, x)")
    val script = s.script
    assert(script.contains("x_1 := (1 + -1 * 2^(1/2))") && !script.contains("√"), script)
    val t = Session()
    t.load(script)
    assert(t.execute("x_1 + x_2") == "2")
    assert(t.execute("x_2") == "(1 + √2)")
  }

  "latex" should "typeset the radical" in {
    val s = exact
    s.execute("latex on")
    s.execute("√13/2")
    assert(s.lastLatex.contains("\\frac{\\sqrt{13}}{2}"), s.lastLatex)
  }

  "inexact mode" should "read √ as an ordinary square root" in {
    val s = Session()
    assert(s.execute("√2") == "1.41421")
    assert(s.execute("2√13") == "7.2111")
  }
