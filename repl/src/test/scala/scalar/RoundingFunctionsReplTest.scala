package it.grypho.scala.leonardo
package scalar

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0324 at the prompt (LT L_0080): the integer parts and the remainder, exact under `exact on`. */
class RoundingFunctionsReplTest extends AnyFlatSpec:

  "the REPL" should "round, and take the remainder with the divisor's sign" in {
    val s = Session()
    assert(s.execute("floor(2.7)") == "2.0" && s.execute("ceil(2.1)") == "3.0")
    assert(s.execute("round(2.5)") == "3.0" && s.execute("round(-2.5)") == "-3.0")
    assert(s.execute("mod(-7, 3)") == "2.0" && s.execute("mod(7, -3)") == "-2.0")
  }

  it should "answer an exact integer under exact on, with no note" in {
    val s = Session()
    s.execute("exact on")
    assert(s.execute("floor(7/2)") == "3" && s.execute("ceil(-7/2)") == "-3")
    assert(s.execute("round(5/2)") == "3" && s.execute("mod(1/2, 1/3)") == "1/6")
    assert(s.execute("floor((10^80 - 1)^(1/2))") == (BigInt(10).pow(40) - 1).toString)
  }

  it should "bind and reuse them like any function, and differentiate between the jumps" in {
    val s = Session()
    s.execute("f(x) := mod(x, 3) + floor(x)")
    assert(s.execute("f(7.5)") == "8.5")   // 1.5 + 7
    assert(s.execute("derive(mod(x, 3), x)") == "1.0")
  }

  it should "refuse the four words as names" in {
    val s = Session()
    for w <- Seq("floor", "ceil", "round", "mod") do
      assert(s.execute(s"$w := 3") == s"cannot assign to '$w': it is a reserved word", w)
  }

  it should "leave a zero divisor as written" in {
    assert(Session().execute("mod(7, 0)") == "mod(7.0, 0.0)")
  }
