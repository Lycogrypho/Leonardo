package it.grypho.scala.leonardo
package control

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** `mpcMatrices` over a symbolic plant (F_0183): the cells double per step, so a long horizon
 *  declines instead of hanging, while a short one still answers. */
class MpcSymbolicTest extends AnyFlatSpec:

  private val env = new Environment()

  "mpcMatrices with a symbolic A" should "decline a long horizon promptly (F_0183)" in {
    val start = System.nanoTime()
    val r     = Parser.parse("mpcMatrices([[a, 1], [1, a]], [[0], [1]], [[1, 0]], 40, 1)").get.eval(env)
    val secs  = (System.nanoTime() - start) / 1e9
    assert(r.fold(_.isInstanceOf[_MpcMatrices], _ => false), r)
    assert(secs < 10.0, s"took $secs s")
  }

  it should "still answer a short horizon (F_0183)" in {
    val r = Parser.parse("mpcMatrices([[a, 1], [1, a]], [[0], [1]], [[1, 0]], 3, 1)").get.eval(env)
    assert(!r.fold(_.isInstanceOf[_MpcMatrices], _ => false), r)
  }
