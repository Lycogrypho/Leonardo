package it.grypho.scala.leonardo
package core

import org.scalatest.flatspec.AnyFlatSpec

/** The heavy nodes' one-solve-per-line memo (F_0212). */
class LastAnswerTest extends AnyFlatSpec:

  "LastAnswer" should "compute once for the same environment and again for another" in {
    var runs   = 0
    val memo   = LastAnswer[Int]()
    val (a, b) = (new Environment(), new Environment())
    assert(memo(a) { runs += 1; 7 } == 7)
    assert(memo(a) { runs += 1; 8 } == 7, "the same environment reads the remembered answer")
    assert(runs == 1)
    assert(memo(b) { runs += 1; 9 } == 9, "an equal but distinct environment computes again")
    assert(runs == 2)
  }

  "a qp node" should "solve once when its value and its detailed answer are both read" in {
    val q   = parser.Parser.parse("qp(eye(2), [[-1], [-1]], 0, 0, -inf, inf)").get.asInstanceOf[optimize._QP]
    val env = new Environment()
    val v   = q.eval(env)
    // The same instance and environment: the note's `detailed` is the answer `eval` computed.
    assert(q.detailed(env) eq q.detailed(env))
    assert(v.isRight)
  }
