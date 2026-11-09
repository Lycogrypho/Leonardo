package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0322 (LT L_0079) -- `equivalent(f, g)`, logical equivalence of two formulas.  `==` between formulas
 *  compares values and stays what it was; equivalence is a different relation. */
class EquivalentTest extends AnyFlatSpec:

  private val plain = new Environment()

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def run(s: String, env: Environment = plain) = parse(s).eval(env)
  private def node(s: String): _Equivalent = parse(s) match
    case n: _Equivalent => n
    case other          => fail(s"not an equivalent node: $other")

  "equivalent" should "be true for formulas that agree on every row, however they are written" in {
    for src <- Seq("equivalent(p implies q, not p or q)",
                   "equivalent(p xor q, not (p iff q))",
                   "equivalent(not (p and q), not p or not q)",
                   "equivalent(p, p and (q or not q))",
                   "equivalent(true, p or not p)",
                   "equivalent(1 > 0 and p, p)")
    do assert(run(src) == Right(_Bool(true)), src)
  }

  it should "be false when two rows differ, and name the first" in {
    assert(run("equivalent(p and q, p or q)") == Right(_Bool(false)))
    // rows run all-false to all-true, the first name most significant: p = false, q = true comes first
    assert(node("equivalent(p and q, p or q)").detailed(plain) ==
      Right(EquivalenceVerdict.Differ(Map("p" -> false, "q" -> true), left = false, right = true)))
  }

  it should "enumerate the names of both sides, so a name on one side only counts" in {
    assert(node("equivalent(p, p and q)").detailed(plain) ==
      Right(EquivalenceVerdict.Differ(Map("p" -> true, "q" -> false), left = true, right = false)))
  }

  it should "read a bound name as its value rather than enumerate it" in {
    val env = plain.withBinding("p", _Bool(true))
    assert(run("equivalent(p and q, q)", env) == Right(_Bool(true)))
    // one name left to enumerate fits a cap of one
    assert(run("equivalent(p and q, q)", env.withLimit(Limit.MaxTruthTableVars, 1)) == Right(_Bool(true)))
  }

  it should "stay unevaluated on a graded or non-boolean operand" in {
    for src <- Seq("equivalent(p, unknown)", "equivalent(x > 0, x > 0)", "equivalent(truth(0.5) and p, p)") do
      val r = run(src)
      assert(r.isLeft && r.left.exists(_.isInstanceOf[_Equivalent]), s"$src -> $r")
      assert(node(src).detailed(plain) == Left(EquivalenceFailure.NotBoolean), src)
  }

  it should "stay unevaluated past the table's cap, saying why" in {
    val capped = plain.withLimit(Limit.MaxTruthTableVars, 2)
    val n = node("equivalent(a and b and c, c and b and a)")
    assert(n.eval(capped).isLeft)
    assert(n.detailed(capped) == Left(EquivalenceFailure.TooManyNames(3, 2)))
  }

  it should "print as the grammar reads it back" in {
    val e = node("equivalent(a implies b, not a or b)")
    assert(e.toString == "equivalent((a implies b), ((not a) or b))")
    assert(parse(e.toString) == e)
  }

  "== between formulas" should "stay what it was: value equality, not equivalence" in {
    // the operands of == are arithmetic, so a connective there is not parsed at all (L_0066)
    assert(!Parser.parse("(p implies q) == (not p or q)").successful)
  }
