package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `cnf(e)` and `dnf(e)` at the prompt (F_0133): the node answers what `toCNF`/`toDNF` answer,
 *  and stays unevaluated where they would hand back their input on a refusal. */
class NormalFormNodeTest extends AnyFlatSpec:

  private def parsed(s: String): _Expression = Parser.parse(s).get

  private def evalOf(s: String, env: Environment = new Environment()): Either[_Expression, _Value] =
    parsed(s).eval(env)

  "cnf(e)" should "parse to the node and print back the way it was written" in {
    val e = parsed("cnf(a xor b)")
    assert(e == _NormalForm(NormalKind.CNF, Xor(_Variable("a"), _Variable("b"))))
    assert(parsed(e.toString) == e)
    assert(parsed("dnf(a implies b)") == _NormalForm(NormalKind.DNF, Implies(_Variable("a"), _Variable("b"))))
  }

  it should "answer what toCNF answers" in {
    val inner = parsed("(a or b) and not (a and c)")
    assert(evalOf("cnf((a or b) and not (a and c))") == Left(toCNF(inner)))
  }

  it should "print the cheat sheet's example exactly" in {
    assert(evalOf("cnf(a xor b)").left.map(_.toString) == Left("((a or b) and ((not b) or (not a)))"))
  }

  "dnf(e)" should "answer what toDNF answers" in {
    val inner = parsed("(a or b) and not a")
    assert(evalOf("dnf((a or b) and not a)") == Left(toDNF(inner)))
    assert(evalOf("dnf((a or b) and not a)") == Left(And(_Variable("b"), Not(_Variable("a")))))
  }

  "a normal form" should "take a bound name's value first" in {
    val env = new Environment().withBinding("a", _Bool(true))
    assert(evalOf("cnf(a and b)", env) == Left(_Variable("b")))
    assert(evalOf("dnf(a and true)", env) == Right(_Bool(true)))
  }

  it should "stay unevaluated on a graded truth value, never answering its operand as its own normal form" in {
    val r = evalOf("cnf(a and truth(0.3))")
    assert(r.left.exists(_.isInstanceOf[_NormalForm]), r)
  }

  it should "stay unevaluated past the environment's clause budget, and answer within a raised one" in {
    val chain = "cnf(a xor b xor c xor d)"
    val tight = new Environment().withLimit(Limit.MaxNormalFormClauses, 4)
    assert(evalOf(chain, tight).left.exists(_.isInstanceOf[_NormalForm]))
    val answered = evalOf(chain)
    assert(answered.left.exists(e => !e.isInstanceOf[_NormalForm]), answered)
  }

  "the words cnf and dnf" should "be reserved, since a production needs them" in {
    assert(Parser.ReservedWords.contains("cnf") && Parser.ReservedWords.contains("dnf"))
  }
