package it.grypho.scala.leonardo

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


class ReplSessionTest extends AnyFlatSpec:

  def session: Session = new Session()

  "a constant assignment" should "bind a numeric value" in
  {
    val s = session
    assert(s.execute("x = 3.001") == "x = 3.001")
    assert(s.execute("x") == "3.001")
  }

  "a constant expression assignment" should "fold and bind the result" in
  {
    val s = session
    s.execute("y = 2 * 3")
    assert(s.execute("y") == "6.0")
  }

  "an assignment with free variables" should "become a definition" in
  {
    val s = session
    assert(s.execute("f = sin(x) + x") == "f = (sin(x) + x)")
    // stays symbolic while x is unbound
    assert(s.execute("f") == "(sin(x) + x)")
  }

  "a definition" should "evaluate numerically once its variable is bound" in
  {
    val s = session
    s.execute("f = sin(x) + x")
    s.execute("x = 0")
    assert(s.execute("f") == "0.0")
  }

  "definitions" should "be late-bound through other definitions" in
  {
    val s = session
    s.execute("f = x + 1")
    s.execute("g = f * 2")
    s.execute("x = 4")
    assert(s.execute("g") == "10.0")
    // redefining f changes g too
    s.execute("f = x + 2")
    assert(s.execute("g") == "12.0")
  }

  "a self-referential definition" should "not loop forever" in
  {
    val s = session
    s.execute("f = f + 1")
    // inner f stays a free variable; result is symbolic, not a hang
    assert(s.execute("f") == "(f + 1.0)")
  }

  "reassigning a definition with a constant" should "turn it into a binding" in
  {
    val s = session
    s.execute("f = x + 1")
    s.execute("f = 5")
    assert(s.execute("f") == "5.0")
  }

  "derive through the expression path" should "differentiate a definition" in
  {
    val s = session
    s.execute("f = x * x")
    s.execute("x = 3")
    assert(s.execute("derive(f, x)") == "6.0")
  }

  "simplify" should "simplify without numeric evaluation" in
  {
    val s = session
    s.execute("x = 3")          // binding must NOT leak into simplify
    assert(s.execute("simplify x + 0") == "x")
  }

  "expand" should "distribute products over sums" in
  {
    val s = session
    assert(s.execute("expand x * (y + z)") == "((x * y) + (x * z))")
  }

  "precision" should "affect evaluation" in
  {
    val s = session
    s.execute("precision 2")
    s.execute("x = 3.14159")
    assert(s.execute("x") == "3.14")
  }

  "precision with a bad argument" should "report an error" in
  {
    val s = session
    assert(s.execute("precision abc").startsWith("precision expects"))
  }

  "unset" should "remove a binding" in
  {
    val s = session
    s.execute("x = 3")
    assert(s.execute("unset x") == "x unset")
    assert(s.execute("x") == "x")
    assert(s.execute("unset x") == "x is not set")
  }

  "env" should "list precision, bindings, and definitions" in
  {
    val s = session
    s.execute("x = 3")
    s.execute("f = x + 1")
    val state = s.execute("env")
    assert(state.contains("precision = 5"))
    assert(state.contains("x = 3.0"))
    assert(state.contains("f = (x + 1.0)"))
  }

  "an unparsable line" should "report a parse error" in
  {
    val s = session
    assert(s.execute("sin(").startsWith("parse error"))
  }

  "a blank line" should "produce no output" in
  {
    assert(session.execute("   ") == "")
  }

  "help" should "list the commands" in
  {
    assert(session.execute("help").contains("simplify"))
  }
