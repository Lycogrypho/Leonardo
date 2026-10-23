package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the normal forms: each case failed before its fix. */
class NormalFormEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()

  "cnf" should "decline a long xor chain promptly, not build 2^k nodes first (F_0183)" in {
    val chain = (1 to 30).map(i => s"a$i").mkString(" xor ")
    val start = System.nanoTime()
    val r     = Parser.parse(s"cnf($chain)").get.eval(env)
    val secs  = (System.nanoTime() - start) / 1e9
    r match
      case Left(_: _NormalForm) => succeed
      case other                => fail(s"got $other")
    assert(secs < 5.0, s"took $secs s")
  }

  it should "still give the normal form of a short xor (F_0183)" in {
    val r = Parser.parse("dnf(a xor b)").get.eval(env)
    assert(r.fold(_.toString, _.toString) == "((a and (not b)) or ((not a) and b))", r)
  }
