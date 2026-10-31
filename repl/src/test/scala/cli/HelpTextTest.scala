package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** F_0201 / D_0152 item 7 -- the REPL's `help` text is the documentation a REPL user reads first,
 *  and it had gone stale: eight topics stated behaviour the library no longer has, and none of the
 *  functions added since 3.8 appeared in the listing. */
class HelpTextTest extends AnyFlatSpec:

  private def topic(t: String): String = Session.helpTopics.getOrElse(t, fail(s"no help topic $t"))

  "the help topics" should "no longer state behaviour the library has outgrown (F_0201)" in {
    assert(topic("latex").contains("beside") && !topic("latex").contains("instead"))
    assert(!topic("exact").contains("Double-accurate") && topic("exact").contains("surd"))
    assert(!topic("fact").contains("not\n         |implemented") && !topic("fact").contains("stays symbolic.") )
    assert(topic("fact").contains("digamma") && topic("fact").contains("complex"))
    assert(!topic("taylor").contains("needs the\n") && !topic("taylor").contains("maclaurin(Gamma(x), x, 3)"))
    assert(!topic("invlaplace").contains("deg D >= 3"))
    assert(topic("eigen").contains("Francis") && !topic("eigen").contains("Wilkinson"))
    assert(!topic("samples").contains("silently") && topic("samples").contains("note"))
    assert(topic("simplify").toLowerCase.contains("every node"))
    assert(topic("solveSystem").contains("more equations than unknowns"))
    assert(topic("defuzz").contains("bisector") && topic("defuzz").contains("mom"))
  }

  it should "show examples that answer as they say" in {
    val s = Session()
    assert(s.execute("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, mom)") == "3.0")
    s.execute("g := invlaplace(1/s^3, s, t)")
    assert(s.execute("t := 2") == "t := 2.0" && s.execute("g") == "2.0")
    val digamma = Session().execute("taylor(digamma(x), x, 1, 2)")
    assert(digamma.startsWith("taylor(") && digamma.contains("note"), digamma)
    assert(Session().execute("derive(Gamma(x), x)").contains("digamma"))
  }

  "the help listing" should "name the functions added since 3.8, and point at the cheat sheet (D_0152)" in {
    for word <- Seq("dot(", "cross(", "trace(", "norm(", "size(", "svd(", "chol(", "median(", "anova(",
                    "ttest(", "tfpoles(", "routhTable(", "acker(", "dlsim(", "minimize(", "qp(",
                    "lsqfit(", "nlp(", "lp(", "rationalize(", "dft(", "cnf(") do
      assert(Session.help.contains(word), word)
    assert(Session.help.contains("cheat sheet"))
  }
