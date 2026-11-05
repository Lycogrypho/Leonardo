package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0294 (LT L_0051, the owner's decision): `f(x) := …` declares a parameter and `f(e)` applies it. */
class FunctionDefinitionTest extends AnyFlatSpec:

  private def session(lines: String*): Session =
    val s = new Session()
    lines.foreach(s.execute)
    s

  "a definition with a parameter list" should "apply to an argument" in {
    val s = session("f(x) := x^3 - 2*x")
    assert(s.execute("f(2)") == "4.0")
    assert(s.execute("f(3) - f(1)") == "22.0")
    assert(s.execute("(f(2 + 0.5) - f(2))/0.5") == "13.25")          // (10.625 - 4)/0.5
    assert(s.execute("f(f(2))") == "56.0")                         // nested, innermost first
    s.execute("x := 10")
    assert(s.execute("f(2)") == "4.0")                             // the parameter, not the binding
  }

  it should "substitute several parameters at once" in {
    val s = session("g(x, y) := x - y", "a := 1", "b := 5")
    assert(s.execute("g(b, a)") == "4.0")
    assert(s.execute("g(2, 3)") == "-1.0")
    s.execute("x := 1"); s.execute("y := 5")
    assert(s.execute("g(y, x)") == "4.0")                          // a swap, not x -> y -> x
  }

  it should "refuse a wrong argument count and a reserved parameter" in {
    val s = session("f(x) := x^2")
    assert(s.execute("f(1, 2)").contains("takes 1 argument"))
    assert(s.execute("k(sin) := 1").contains("reserved"))
    assert(s.execute("k(x, x) := 1").contains("twice"))
  }

  "its derivative" should "be the ordinary one for one parameter, the partial for several" in {
    val s = session("f(x) := x^3 - 2*x", "g(x, y) := x^2*y")
    s.execute("x := 2"); s.execute("y := 3")
    assert(s.execute("derive(f, x)") == "10.0")                    // 3x² - 2
    assert(s.execute("derive(g, x)") == "12.0")                    // ∂/∂x = 2xy
    val t = session("f(x) := x^3 - 2*x")
    assert(t.execute("derive(f(u^2), u)").contains("u"))           // u is free: symbolic
    t.execute("u := 1")
    assert(t.execute("derive(f(u^2), u)") == "2.0")               // 6u⁵ - 4u, the chain rule
  }

  "a parameterised definition" should "survive :save and unset" in {
    val s = session("f(x) := x^3 - 2*x")
    assert(s.script.contains("f(x) := "), s.script)
    val r = new Session()
    r.load(s.script)
    assert(r.execute("f(2)") == "4.0")
    s.execute("unset f")
    assert(s.execute("f(2)").contains("not a function"))
  }

  "a definition without a parameter list" should "keep its old meaning" in {
    val s = session("h := x^2")
    assert(s.execute("h(2)").contains("not a function"))
  }
