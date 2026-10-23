package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 at the prompt: each case failed before its fix. */
class ReplEdgeCaseTest extends AnyFlatSpec:

  "a derivative with respect to a definition" should "survive being stored, simplified and sampled (F_0185)" in {
    val s = new Session()
    List("f := sin(x)", "g := f^2", "q := derive(g, f)").foreach(s.execute)
    s.execute("x := 0.5")
    assert(s.execute("q") == "0.95885")
    assert(s.execute("derive(g, f)") == "0.95885")
    val t = new Session()
    List("f := sin(x)", "g := f^2").foreach(t.execute)
    assert(t.execute("samples derive(g, f) x 0.5 0.6 2").startsWith("0.5\t0.95885"))
  }

  ":save" should "write infinities, distributions and negative based values so they reload (F_0186)" in {
    val s = new Session()
    List("lb := [[-inf], [0]]", "d := normal(0, 1e-20)", "n := tobase(-255, 16)").foreach(s.execute)
    val script = s.script
    assert(script.contains("[[-inf], [0.0]]"), script)
    assert(!script.contains("Infinity"), script)
    assert(script.contains("tobase(-255, 16)"), script)
    val r = new Session()
    r.load(script)
    assert(r.execute("at(lb, 1, 1) < 0") == "true")
    assert(r.execute("n + 0") == "-255.0")
    assert(r.execute("variance(d)") != "variance(d)", r.execute("variance(d)"))
  }

  "a name with an underscore after a constant's letters" should "read back once assigned (F_0203)" in {
    val s = new Session()
    s.execute("e_x := 3")
    assert(s.execute("e_x + 1") == "4.0")
    s.execute("pi_2 := 5")
    assert(s.execute("pi_2 * 2") == "10.0")
  }

  "a sweep command" should "read a long run of spaces in linear time (F_0182)" in {
    val s     = new Session()
    val start = System.nanoTime()
    val out   = s.execute("samples x" + " " * 40000 + "!")
    val secs  = (System.nanoTime() - start) / 1e9
    assert(out.startsWith("usage:"), out)
    assert(secs < 1.0, s"took $secs s")
    assert(s.execute("samples x^2 x 0 1 3").linesIterator.size == 3)
    assert(s.execute("samples x + 1 x 0 1").linesIterator.nonEmpty)
  }

  "the math editor's AsciiMath" should "refuse ± and read the connectives (F_0208)" in {
    // The sources are what MathLive 0.111.1 emits, recorded with its SSR build.
    def norm(s: String): Either[String, String] = AsciiMath.toGrammar(s).map(_.replaceAll("\\s+", " ").trim)
    assert(AsciiMath.toGrammar("1+-2").isLeft)
    assert(AsciiMath.toGrammar("1∓2").isLeft)
    assert(norm("6 -: 3") == Right("6/3"))
    assert(norm("a and b") == Right("a and b"))
    assert(norm("a or b") == Right("a or b"))
    assert(norm(" not a") == Right("not a"))
    assert(norm("a=>b") == Right("a implies b"))
    assert(norm("a⇒b") == Right("a implies b"))
    assert(norm("x y") == Right("x*y"), "juxtaposed names are still a product")
    val s = new Session()
    assert(s.execute("true and false") == "false")
    assert(s.execute(norm("a=>b").getOrElse("")).contains("implies"))
  }

  "minimize" should "say why a start at a maximum is refused (F_0199)" in {
    val s = new Session()
    assert(s.execute("minimize(x^4 - 2*x^2, [[x]], 0, bfgs)").contains("maximum or a saddle"))
  }
