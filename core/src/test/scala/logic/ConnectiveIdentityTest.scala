package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0326 (LT L_0082) — a connective with one known operand folds its identity and annihilator on
 *  either side.  Before the fix `true and q` evaluated to `(true and q)`. */
class ConnectiveIdentityTest extends AnyFlatSpec:

  private val q = _Variable("q")

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def under(sem: LogicSemantics): Environment =
    new Environment(Environment.DefaultPrecision, Map(), false, sem).withBinding("p", _Bool(true)).withBinding("n", _Bool(false))

  private val semantics = Seq(LogicSemantics.MinMax, LogicSemantics.Product, LogicSemantics.Lukasiewicz)

  "a true operand" should "leave the other side of and, decide or, under every semantics" in {
    for sem <- semantics do
      val env = under(sem)
      assert(parse("p and q").eval(env) == Left(q), sem.toString)
      assert(parse("q and p").eval(env) == Left(q), sem.toString)
      assert(parse("q or p").eval(env) == Right(_Bool(true)), sem.toString)
      assert(parse("q or n").eval(env) == Left(q), sem.toString)
      assert(parse("q and n").eval(env) == Right(_Bool(false)), sem.toString)
  }

  "implies" should "fold a known antecedent or consequent" in {
    for sem <- semantics do
      val env = under(sem)
      assert(parse("p implies q").eval(env) == Left(q), sem.toString)
      assert(parse("q implies p").eval(env) == Right(_Bool(true)), sem.toString)
      assert(parse("q implies n").eval(env) == Left(Not(q)), sem.toString)
      assert(parse("n implies q").eval(env) == Right(_Bool(true)), sem.toString)
  }

  "xor" should "fold a crisp operand on either side" in {
    for sem <- semantics do
      val env = under(sem)
      assert(parse("p xor q").eval(env) == Left(Not(q)), sem.toString)
      assert(parse("q xor p").eval(env) == Left(Not(q)), sem.toString)
      assert(parse("n xor q").eval(env) == Left(q), sem.toString)
      assert(parse("q xor n").eval(env) == Left(q), sem.toString)
  }

  "a graded other side" should "come through the identity unchanged" in {
    for sem <- semantics do
      val env = under(sem)
      assert(parse("p and unknown").eval(env) == Right(_Truth.Unknown), sem.toString)
      assert(parse("unknown or n").eval(env) == Right(_Truth.Unknown), sem.toString)
      assert(parse("p xor unknown").eval(env) == Right(_Truth.Unknown), sem.toString)
      assert(parse("p implies truth(0.3)").eval(env) == Right(_Truth.of(0.3)), sem.toString)
  }

  "a number beside a truth value" should "stay as written" in {
    val env = under(LogicSemantics.MinMax)
    assert(parse("3 and p").eval(env) == Left(And(_Number(3.0), _Bool(true))))
    assert(parse("3 xor p").eval(env) == Left(Xor(_Number(3.0), _Bool(true))))
    assert(parse("3 implies p").eval(env) == Left(Implies(_Number(3.0), _Bool(true))))
  }
