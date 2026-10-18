package it.grypho.scala.leonardo
package control

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `controllable`, `observable`, `ctrb` and `obsv` at the prompt (F_0134): each answers what the
 *  library's controllability test answers, from two operands or from a model row. */
class ControllabilityPromptTest extends AnyFlatSpec:

  private def ev(s: String, env: Environment = new Environment()): Either[_Expression, _Value] =
    Parser.parse(s).get.eval(env)

  private val dblInt = "[[0, 1], [0, 0]]"

  "controllable(A, B)" should "answer true for the double integrator driven by force" in {
    assert(ev(s"controllable($dblInt, [[0], [1]])") == Right(_Bool(true)))
  }

  it should "answer false when the input reaches one decoupled mode only" in {
    assert(ev("controllable([[1, 0], [0, 2]], [[1], [0]])") == Right(_Bool(false)))
  }

  it should "agree with the library function" in {
    val (a, b) = (Parser.parse("[[1, 2], [3, 4]]").get, Parser.parse("[[1], [1]]").get)
    assert(ev("controllable([[1, 2], [3, 4]], [[1], [1]])") == Right(_Bool(controllable(a, b).contains(true))))
  }

  it should "not change its verdict when the input is in different units" in {
    assert(ev(s"controllable($dblInt, [[0], [0.001]])") == Right(_Bool(true)))
  }

  it should "read a scalar plant written with numbers" in {
    assert(ev("controllable(0.5, 1)") == Right(_Bool(true)))
    assert(ev("controllable(0.5, 0)") == Right(_Bool(false)))
  }

  it should "stay unevaluated on non-conforming operands or a free name" in {
    assert(ev(s"controllable($dblInt, [[1, 0, 0]])").isLeft)
    assert(ev(s"controllable($dblInt, B)").isLeft)
  }

  "observable(A, C)" should "answer true when position is measured and false when only velocity is" in {
    assert(ev(s"observable($dblInt, [[1, 0]])") == Right(_Bool(true)))
    assert(ev(s"observable($dblInt, [[0, 1]])") == Right(_Bool(false)))
  }

  "the model-row form" should "read A and B, or A and C, from stateSpace(A, B, C, D)" in {
    val sys = s"stateSpace($dblInt, [[0], [1]], [[0, 1]], 0)"
    assert(ev(s"controllable($sys)") == Right(_Bool(true)))
    assert(ev(s"observable($sys)") == Right(_Bool(false)))
  }

  "ctrb(A, B)" should "build [B, A*B]" in {
    assert(ev(s"ctrb($dblInt, [[0], [1]])") == ev("[[0, 1], [1, 0]]"))
  }

  "obsv(A, C)" should "build [C; C*A]" in {
    assert(ev(s"obsv($dblInt, [[1, 0]])") == ev("[[1, 0], [0, 1]]"))
  }

  "the four nodes" should "round-trip through toString" in {
    for s <- List(s"controllable($dblInt, B)", s"observable(A, C)", "ctrb(A, B)", "obsv(A, C)", "controllable(sys)") do
      val e = Parser.parse(s).get
      assert(Parser.parse(e.toString).get == e, s)
  }

  "the words" should "be reserved, since productions need them" in {
    List("controllable", "observable", "ctrb", "obsv").foreach(w => assert(Parser.ReservedWords.contains(w), w))
  }
