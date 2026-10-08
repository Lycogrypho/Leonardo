package it.grypho.scala.leonardo
package core

import parser.Parser
import scalar.*
import org.scalatest.flatspec.AnyFlatSpec


/** F_0078 -- the expression depth limit, which bounds what the recursive walkers must descend on
 *  the shallowest platform. A flat chain of terms is a deep tree, which `Parser.MaxDepth` (a
 *  nesting count) never saw.
 */
class DepthTest extends AnyFlatSpec:

  private var fresh = 0
  /** `p1 op p2 op ... pn` over names no other test has used, so no memo can shorten a walk. */
  private def chain(op: String, n: Int): String =
    fresh += 1
    (1 to n).map(i => s"d${fresh}_$i").mkString(s" $op ")

  "depthUpTo" should "count levels and stop counting past its limit" in
  {
    assert(depthUpTo(_Variable("x"), 10) == 1)
    assert(depthUpTo(Sum(_Variable("x"), Product(_Number(2), _Variable("y"))), 10) == 3)
    val spine = (1 to 5000).foldLeft(_Variable("x"): _Expression)((acc, i) => Sum(acc, _Number(i)))
    assert(depthUpTo(spine, 100) == 101, "counting must stop at the limit, and not recurse")
  }

  "the parser" should "refuse a sum or product chain deeper than the limit, with a reason" in
  {
    for op <- Seq("+", "*") do
      val r = Parser.parse(chain(op, 300))
      assert(!r.successful, s"a 300-term '$op' chain was accepted")
      assert(r.toString.contains(s"deeper than $MaxExpressionDepth levels"), r.toString)
    assert(Parser.parse(chain("+", 150)).successful)
  }

  it should "see through parentheses, which a per-chain count would not" in
  {
    // Two 150-term chains: neither is long, but the first is the left operand of the second,
    // so the tree is 300 deep.
    val r = Parser.parse(s"(${chain("+", 150)}) + ${chain("+", 150)}")
    assert(!r.successful, "the combined spine is 300 deep")
  }

  "a symbolic sum" should "decline a result deeper than the limit, while a numeric one does not" in
  {
    def evalOf(s: String) = Parser.parse(s).get.eval(new Environment())
    // Numeric terms fold flat, so the range cap (1000) is the only bound.
    assert(evalOf("sum(k, k, 1, 1000)") == Right(_Number(500500)))
    val deep = Parser.parse("sum(k*w, k, 1, 300)").get
    assert(deep.eval(new Environment()) == Left(deep), "300 symbolic terms is a 300-deep spine")
    deep match
      case r: _Reduction =>
        assert(r.declineReason(new Environment()).exists(_.contains("symbolic sum of 300 terms would be deeper")))
      case other => fail(s"expected a reduction, got $other")
    assert(evalOf("sum(k*w, k, 1, 150)").isLeft, "150 symbolic terms still answer (symbolically)")
    assert(evalOf("sum(k*w, k, 1, 150)") != Left(Parser.parse("sum(k*w, k, 1, 150)").get))
  }

  "expand" should "refuse an expansion deeper than the limit" in
  {
    val (r, why) = expandReporting(Parser.parse(s"(${chain("+", 150)}) * (a + b)").get)
    assert(why.exists(_.contains(s"deeper than the limit of $MaxExpressionDepth levels")), s"reason: $why")
    assert(!tooDeep(r), "what is returned is the product as written, not the deep expansion")
  }
