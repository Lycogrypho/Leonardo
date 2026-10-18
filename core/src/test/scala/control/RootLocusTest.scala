package it.grypho.scala.leonardo
package control

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `rootLocus` (F_0140): the closed-loop poles of `G` under gain `K`, as continuous branches. */
class RootLocusTest extends AnyFlatSpec:

  private val s = _Variable("s")
  private def g(text: String): _Expression = Parser.parse(text).get

  // 1/(s(s+2)) closes to s^2 + 2s + K: roots -1 ± sqrt(1 - K), meeting at K = 1 and leaving
  // the real axis there.
  private val plant = g("1/(s*(s + 2))")
  private val gains = Vector(0.25, 0.5, 0.75, 0.9, 1.1, 1.5, 2.0, 4.0)

  "rootLocus" should "give one branch per closed-loop pole, at every gain" in {
    rootLocus(plant, s, gains) match
      case Some(branches) =>
        assert(branches.size == 2)
        assert(branches.forall(_.size == gains.size))
      case None => fail("no locus")
  }

  it should "place every point at the closed form -1 ± sqrt(1 - K)" in {
    rootLocus(plant, s, gains) match
      case Some(branches) =>
        for (k, i) <- gains.zipWithIndex do
          val expected =
            if k <= 1 then Set((-1 + math.sqrt(1 - k), 0.0), (-1 - math.sqrt(1 - k), 0.0))
            else Set((-1.0, math.sqrt(k - 1)), (-1.0, -math.sqrt(k - 1)))
          val got = branches.map(_(i))
          assert(got.forall(p => expected.exists(e => math.hypot(p._1 - e._1, p._2 - e._2) < 1e-6)), s"K = $k: $got")
      case None => fail("no locus")
  }

  it should "keep each branch continuous through the breakaway point" in {
    // Above K = 1 one branch must leave upwards and the other downwards, each staying on its
    // own side: the nearest-continuation matching, without which the two would swap halves.
    rootLocus(plant, s, gains) match
      case Some(branches) =>
        val above = gains.indices.filter(gains(_) > 1)
        val signs = branches.map(b => above.map(i => math.signum(b(i)._2)).distinct)
        assert(signs.forall(_.size == 1), signs)
        assert(signs.map(_.head).toSet == Set(1.0, -1.0))
      case None => fail("no locus")
  }

  it should "decline an improper plant and a non-rational one" in {
    assert(rootLocus(g("s^2/(s + 1)"), s, gains).isEmpty)
    assert(rootLocus(g("exp(-s)/(s + 1)"), s, gains).isEmpty)
  }
