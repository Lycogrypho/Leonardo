package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Pins the spelling every function node prints (issue F_0016 step 0).
 *
 *  `_Function` now carries an abstract `name` and one inherited `toString`, replacing 41
 *  hand-written overrides.  **The risk such a refactor carries is silent**: a wrong name still
 *  compiles, still round-trips through `toString` → parse → `toString`, and only shows up as a
 *  session that saves under one spelling and reloads under another.
 *
 *  So this suite asserts the spellings that a mechanical refactor would most plausibly get
 *  wrong — every case where the printed name is **not** simply the class name in lower case.
 *  Deriving `name` from the class name was rejected for exactly this reason: `Exp` prints
 *  `exp` but `Gamma` prints `Gamma`, so neither `productPrefix` nor its lower-cased form is
 *  right for both, and a default that is right for most nodes and wrong for a few is the
 *  confidently-wrong failure this codebase declines everywhere.
 */
class FunctionNameTest extends AnyFlatSpec:

  private val x = _Variable("x")

  /** Parses, and fails loudly rather than throwing out of a `.get`. */
  private def parse(s: String): _Expression =
    val r = Parser.parse(s)
    assert(r.successful, s"parse failed for \"$s\": $r")
    r.get

  "a function whose printed name differs from its class name" should "keep its own spelling" in
  {
    // Each of these five is a class whose name is NOT the token the grammar uses, so the
    // shared toString can only be right if `name` was transcribed by hand.
    assert(Tg(x).toString            == "tan(x)",         "Tg prints tan")
    assert(LogBase(x, _Number(10)).toString == "log(x, 10.0)", "LogBase prints log")
    assert(Factorial(x).toString     == "fact(x)",        "Factorial prints fact")
    assert(MultiFactorial(x, _Number(2)).toString == "mfact(x, 2.0)", "MultiFactorial prints mfact")
    assert(LogGamma(x).toString      == "lgamma(x)",      "LogGamma prints lgamma")
  }

  "the deliberately capitalised functions" should "stay capitalised" in
  {
    // 4.K capitalised these so the lower-case names stay free as variables (`alpha + beta` is
    // two variables). A refactor that lower-cased every name would silently free `Gamma(2)`
    // to re-parse as a multiplication by a variable named gamma.
    assert(Gamma(x).toString == "Gamma(x)")
    assert(Beta(x, _Number(2)).toString == "Beta(x, 2.0)")
    assert(Si(x).toString == "Si(x)")
    assert(Ci(x).toString == "Ci(x)")
    assert(Ei(x).toString == "Ei(x)")
    // ...while the ones that were deliberately left lower case stay that way.
    assert(Li(x).toString == "li(x)")
    assert(FresnelS(x).toString == "fresnelS(x)")
    assert(FresnelC(x).toString == "fresnelC(x)")
  }

  "every function node's printed name" should "round-trip through the parser" in
  {
    // The invariant that makes a name *correct* rather than merely consistent: whatever a node
    // prints must parse back to the same node. A misspelt name would either fail to parse or,
    // worse, parse as something else.
    val nodes: List[_Expression] = List(
      Exp(x), Ln(x), LogBase(x, _Number(10)), Sin(x), Cos(x), Tg(x),
      Asin(x), Acos(x), Atan(x),
      Sinh(x), Cosh(x), Tanh(x), Asinh(x), Acosh(x), Atanh(x),
      Sec(x), Csc(x), Cot(x), Sech(x), Csch(x), Coth(x),
      Factorial(x), MultiFactorial(x, _Number(2)), Gamma(x), LogGamma(x),
      Beta(x, _Number(2)), Si(x), Ci(x), Ei(x), Li(x), FresnelS(x), FresnelC(x)
    )
    for n <- nodes do
      val printed = n.toString
      assert(parse(printed) == n, s"'$printed' did not round-trip back to $n")
  }

  "a binder-carrying functional" should "keep its own toString" in
  {
    // These are the nine the shared toString deliberately does NOT cover: they print a binder
    // that `children` excludes on purpose, so `name(children)` would drop it and produce
    // `derive(x)` for what is `derive(x, x)`. Pinned so a later tidy-up does not "finish the
    // job" and lose them.
    assert(_Derivative(x, x).toString == "derive(x, x)")
    assert(_Integral(x, x).toString   == "integral(x, x)")
  }
