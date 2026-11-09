package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0325 (LT L_0081) -- the combining laws and De Morgan where it removes a negation.
 *
 *  Before, `simplify not (not p and not q)` stayed (it is `p or q`), as did `(p and q) or (p and not q)`
 *  (it is `p`) and `(p implies q) and p` (it is `p and q`). */
class LogicCombiningTest extends AnyFlatSpec:

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def simplified(s: String): _Expression = simplifyLogicFully(parse(s))

  private def is(src: String, expected: String): Unit =
    assert(simplified(src) == parse(expected), s"$src -> ${simplified(src)}")

  "De Morgan" should "apply where it removes a negation" in {
    is("not (not p and not q)", "p or q")
    is("not (not p or not q)", "p and q")
    is("not (not p and q)", "p or not q")
    is("not (p or not q)", "not p and q")
  }

  it should "leave a negation that has nothing to remove" in {
    is("not (p and q)", "not (p and q)")
    is("not (p or q)", "not (p or q)")
  }

  it should "hold under every semantics, for a graded operand too" in {
    // the conorm is the dual of the norm under the strong negation: min/max, product, Lukasiewicz
    for sem <- Seq(LogicSemantics.MinMax, LogicSemantics.Product, LogicSemantics.Lukasiewicz) do
      val graded = Not(And(Not(_Variable("p")), _Truth.of(0.3)))
      assert(simplifyLogicFully(graded, identity, false, sem) == Or(_Variable("p"), _Truth.of(1.0 - 0.3)), sem.toString)
  }

  "the combining law" should "drop the variable the two terms differ in" in {
    is("(p and q) or (p and not q)", "p")
    is("(q and p) or (p and not q)", "p")
    is("(p and not q) or (q and p)", "p")
    is("(not q and p) or (p and q)", "p")
    is("(p or q) and (p or not q)", "p")
    is("(q or p) and (not q or p)", "p")
    // the common part may be a whole subformula
    is("((a or b) and q) or ((a or b) and not q)", "a or b")
  }

  it should "not fire when the two terms differ in more than a sign" in {
    is("(p and q) or (p and not r)", "(p and q) or (p and not r)")
    is("(p and q) or (r and not q)", "(p and q) or (r and not q)")
  }

  it should "not fire on a graded operand, where it is false" in {
    // max(min(a, b), min(a, 1 - b)) = min(a, max(b, 1 - b)), which is below a = 0.9 at b = 0.5
    val tree = Or(And(_Truth.of(0.9), _Variable("q")), And(_Truth.of(0.9), Not(_Variable("q"))))
    assert(simplifyLogicFully(tree) == tree)
    val dual = And(Or(_Truth.of(0.9), _Variable("q")), Or(_Truth.of(0.9), Not(_Variable("q"))))
    assert(simplifyLogicFully(dual) == dual)
  }

  "implication" should "be absorbed by its antecedent" in {
    is("(p implies q) and p", "p and q")
    is("p and (p implies q)", "p and q")
    is("(p implies q) and r", "(p implies q) and r")
    val tree = And(Implies(_Truth.of(0.5), _Variable("q")), _Truth.of(0.5))
    assert(simplifyLogicFully(tree) == tree, "false at a graded antecedent, so it must not fire")
  }

  "the laws" should "be sound and never grow a formula, on a generated corpus" in {
    val random = new scala.util.Random(325)
    val atoms  = List(_Variable("a"), _Variable("b"), _Variable("c"))
    def tree(depth: Int): _Expression =
      if depth == 0 || random.nextInt(6) == 0 then atoms(random.nextInt(3))
      else random.nextInt(5) match
        case 0 => And(tree(depth - 1), tree(depth - 1))
        case 1 => Or(tree(depth - 1), tree(depth - 1))
        case 2 => Not(tree(depth - 1))
        case 3 => Implies(tree(depth - 1), tree(depth - 1))
        case _ => Xor(tree(depth - 1), tree(depth - 1))
    def size(e: _Expression): Int = 1 + e.children.map(size).sum
    def table(e: _Expression): List[Option[Boolean]] =
      for i <- (0 until 8).toList yield
        val env = atoms.zipWithIndex.foldLeft(new Environment()) { case (acc, (v, j)) =>
          acc.withBinding(v.variable, _Bool(((i >> j) & 1) == 1)) }
        e.eval(env) match
          case Right(_Bool(b)) => Some(b)
          case _               => None
    var shrunk = 0
    for _ <- 1 to 1500 do
      val e = tree(5)
      val s = simplifyLogicFully(e)
      assert(table(s) == table(e), s"$e simplified to $s")
      assert(size(s) <= size(e), s"$e grew to $s")
      if size(s) < size(e) then shrunk += 1
    assert(shrunk > 300, shrunk)
  }
