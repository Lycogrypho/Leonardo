package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable


/** F_0325 step 2 (LT L_0081) -- `mindnf` / `mincnf`, a minimal two-level form.
 *
 *  Decision A: fewest terms, then fewest literals.  Decision B: one minimal form, the first in a fixed
 *  canonical order, and the number of minimal forms.  Decision C: a non-connective subexpression is an
 *  opaque atom, as `cnf` and `dnf` read it. */
class MinimiseTest extends AnyFlatSpec:

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def dnf(src: String): Either[MinimiseFailure, MinimalForm] = minimalForm(parse(src), conjunctive = false)
  private def cnf(src: String): Either[MinimiseFailure, MinimalForm] = minimalForm(parse(src), conjunctive = true)
  private def answer(r: Either[MinimiseFailure, MinimalForm]): MinimalForm = r.fold(w => fail(s"declined: $w"), identity)

  // ── the textbook cases ──────────────────────────────────────────────────────────────────────────

  "mindnf" should "drop the consensus term the normal form keeps" in {
    // dnf answers three terms, the last the consensus of the other two
    assert(toDNF(parse("(p or q) and (not p or r)")) != parse("(p and r) or (not p and q)"))
    val r = answer(dnf("(p or q) and (not p or r)"))
    assert(r.expr == parse("(p and r) or (not p and q)"), r.expr.toString)
    assert(r.count == 1)
  }

  "mincnf" should "be the dual: the minimal product of sums" in {
    val r = answer(cnf("(p or q) and (not p or r)"))
    assert(r.expr == parse("(p or q) and (not p or r)"), r.expr.toString)
    assert(answer(cnf("(p and r) or (not p and q) or (q and r)")).expr == parse("(p or q) and (not p or r)"))
  }

  "a cyclic cover" should "have two minimal forms: one is answered, in a fixed order, and both are counted" in {
    // the six prime implicants of this function form a cycle; {ab, a'c', b'c} and {a'b', bc', ac} are both minimal
    val f = "(not a and not b) or (not a and not c) or (not b and c) or (b and not c) or (a and c) or (a and b)"
    val r = answer(dnf(f))
    assert(r.count == 2)
    // the sorted terms [ab, a'c', b'c] come before [ac, a'b', bc'] literal by literal, so that cover is the answer
    assert(r.expr == parse("(a and b) or (not a and not c) or (not b and c)"), r.expr.toString)
  }

  "parity" should "have no simplification: 2^(n-1) terms of n literals" in {
    val r = answer(dnf("a xor b xor c"))
    def terms(e: _Expression): Int = e match { case Or(x, y) => terms(x) + terms(y); case _ => 1 }
    assert(terms(r.expr) == 4 && r.count == 1)
    def clauses(e: _Expression): Int = e match { case And(x, y) => clauses(x) + clauses(y); case _ => 1 }
    assert(clauses(answer(cnf("a xor b xor c")).expr) == 4)
  }

  "a constant function" should "answer true or false in either form" in {
    assert(answer(dnf("p or not p")).expr == _Bool(true) && answer(cnf("p or not p")).expr == _Bool(true))
    assert(answer(dnf("p and not p")).expr == _Bool(false) && answer(cnf("p and not p")).expr == _Bool(false))
    assert(answer(dnf("(p and q) or (p and not q) or not p")).expr == _Bool(true))
  }

  "an already minimal formula" should "come back as itself" in {
    assert(answer(dnf("p")).expr == parse("p"))
    assert(answer(dnf("not p")).expr == parse("not p"))
    assert(answer(dnf("p and q")).expr == parse("p and q"))
    assert(answer(cnf("p or q")).expr == parse("p or q"))
  }

  // ── atoms (Decision C) ──────────────────────────────────────────────────────────────────────────

  "a relation" should "be an opaque atom, as cnf and dnf read it" in {
    assert(answer(dnf("x > 0 and y")).expr == parse("x > 0 and y"))
    assert(answer(dnf("x > 0 or (x > 0 and y)")).expr == parse("x > 0"))
    assert(answer(dnf("(x > 0 and y) or (x > 0 and not y)")).expr == parse("x > 0"))
    assert(answer(cnf("(x > 0 or y) and (x > 0 or not y)")).expr == parse("x > 0"))
  }

  // ── declines ────────────────────────────────────────────────────────────────────────────────────

  "a graded operand" should "be declined: a two-level form is a two-valued notion" in {
    assert(dnf("truth(0.3) and p") == Left(MinimiseFailure.Graded))
    assert(minimalForm(And(_Truth.of(0.5), _Variable("p")), conjunctive = false) == Left(MinimiseFailure.Graded))
    assert(cnf("unknown or p") == Left(MinimiseFailure.Graded))
  }

  "too many atoms" should "be declined by name" in {
    val limits = Limits.Default.withLimit(Limit.MaxMinimiseVars, 2)
    assert(minimalForm(parse("a and b and c"), conjunctive = false, limits = limits) == Left(MinimiseFailure.TooManyAtoms(3, 2)))
    assert(minimalForm(parse("a and b"), conjunctive = false, limits = limits).isRight)
  }

  "the work budget" should "stop a search that outruns it" in {
    val f = "(not a and not b) or (not a and not c) or (not b and c) or (b and not c) or (a and c) or (a and b)"
    val limits = Limits.Default.withLimit(Limit.MaxMinimiseWork, 1)
    assert(minimalForm(parse(f), conjunctive = false, limits = limits) == Left(MinimiseFailure.OverBudget))
    assert(minimalForm(parse(f), conjunctive = false).isRight)
  }

  "reading the truth vector" should "be charged to the work budget, before it is spent" in {
    // five atoms, nine nodes: 32 rows of 9 nodes is 288 units of reading alone
    val f = parse("a and b and c and d and e")
    assert(minimalForm(f, conjunctive = false, limits = Limits.Default.withLimit(Limit.MaxMinimiseWork, 100)) ==
      Left(MinimiseFailure.OverBudget))
    assert(minimalForm(f, conjunctive = false, limits = Limits.Default.withLimit(Limit.MaxMinimiseWork, 100000)).isRight)
  }

  // ── the node ────────────────────────────────────────────────────────────────────────────────────

  "the mindnf node" should "answer a formula, a constant and stay unevaluated on a refusal" in {
    val env = new Environment()
    assert(parse("mindnf((p or q) and (not p or r))").eval(env) == Left(parse("(p and r) or (not p and q)")))
    assert(parse("mincnf((p and r) or (not p and q) or (q and r))").eval(env) == Left(parse("(p or q) and (not p or r)")))
    assert(parse("mindnf(p or not p)").eval(env) == Right(_Bool(true)))
    assert(parse("mindnf(true and false)").eval(env) == Right(_Bool(false)))
    // like cnf/dnf, the refused node carries its operand as evaluated
    assert(parse("mindnf(truth(0.3) and p)").eval(env).left.exists(_.isInstanceOf[_MinimalForm]))
    assert(parse("mincnf(unknown or p)").eval(env).left.exists(_.isInstanceOf[_MinimalForm]))
  }

  it should "read a bound name as its value" in {
    val env = new Environment().withBinding("p", _Bool(true))
    assert(parse("mindnf(p and q)").eval(env) == Left(_Variable("q")))
  }

  it should "print as the grammar reads it back, and be reserved" in {
    val e = parse("mindnf(a and (b or c))")
    assert(e.toString == "mindnf((a and (b or c)))" && parse(e.toString) == e)
    assert(parse("mincnf(a or b)").toString == "mincnf((a or b))")
    assert(Parser.ReservedWords.contains("mindnf") && Parser.ReservedWords.contains("mincnf"))
  }

  // ── every four-variable function, against an independent minimum ───────────────────────────────

  /** All 81 cubes of four variables as `(value, mask, minterms)`, a minterm bit `m` set when the cube holds row `m`. */
  private val cubes4: Vector[(Int, Int, Int)] =
    for mask <- (0 until 16).toVector; value <- 0 until 16 if (value & mask) == 0 yield
      val rows = (0 until 16).filter(m => (m & ~mask & 15) == value).foldLeft(0)((acc, m) => acc | (1 << m))
      (value, mask, rows)

  /** The cheapest set of cubes whose union is exactly each of the 65 536 sets of rows -- a shortest path over
   *  the sets, never touching primes or a cover table, so it shares nothing with what it checks.  A cube costs
   *  1000 plus its literals: fewest terms first, then fewest literals. */
  private lazy val cheapest: Array[Int] =
    val dist = Array.fill(1 << 16)(Int.MaxValue)
    dist(0) = 0
    val queue = mutable.PriorityQueue.empty[(Int, Int)](using Ordering.by[(Int, Int), Int](_._1).reverse)
    queue.enqueue((0, 0))
    while queue.nonEmpty do
      val (d, state) = queue.dequeue()
      if d == dist(state) then
        for (_, mask, rows) <- cubes4 do
          val next = state | rows
          val cost = d + 1000 + (4 - Integer.bitCount(mask))
          if cost < dist(next) then
            dist(next) = cost
            queue.enqueue((cost, next))
    dist

  /** The rows a term, given as literal codes `atom * 2 + (1 if negated)`, holds. */
  private def rowsOf(term: Vector[Int], n: Int): Int =
    (0 until (1 << n)).filter { m =>
      term.forall(code => (((m >> (code / 2)) & 1) == 1) == ((code & 1) == 0))
    }.foldLeft(0)((acc, m) => acc | (1 << m))

  "the minimum, over every four-variable function" should "be the cheapest cover found by an independent search" in {
    var checked = 0
    for f <- 1 until (1 << 16) - 1 do
      val ones = (0 until 16).filter(m => ((f >> m) & 1) == 1).toArray
      val cover = minimise(4, ones, Long.MaxValue / 4).getOrElse(fail(s"declined at $f"))
      val union = cover.terms.foldLeft(0)((acc, t) => acc | rowsOf(t, 4))
      assert(union == f, s"function $f: the terms cover ${union.toBinaryString}")
      assert(cover.terms.forall(t => (rowsOf(t, 4) & ~f) == 0), s"function $f: a term outside it")
      val cost = cover.terms.size * 1000 + cover.terms.map(_.size).sum
      assert(cost == cheapest(f), s"function $f: cost $cost, cheapest ${cheapest(f)}")
      assert(cover.count >= 1)
      checked += 1
    assert(checked == 65534)
  }

  // ── six variables: the properties, with no second minimum to compare against ────────────────────

  "a minimal cover of a random six-variable function" should "be equivalent, prime and irredundant" in {
    val random = new scala.util.Random(325)
    val n = 6
    for density <- Seq(0.2, 0.5, 0.8); _ <- 1 to 60 do
      val ones = (0 until (1 << n)).filter(_ => random.nextDouble() < density).toArray
      if ones.nonEmpty && ones.length < (1 << n) then
        val f = ones.toSet
        val cover = minimise(n, ones, Long.MaxValue / 4).getOrElse(fail("declined"))
        def rows(term: Vector[Int]): Set[Int] =
          (0 until (1 << n)).filter(m => term.forall(c => (((m >> (c / 2)) & 1) == 1) == ((c & 1) == 0))).toSet
        assert(cover.terms.flatMap(rows).toSet == f, "not equivalent")
        for t <- cover.terms do
          assert(rows(t).subsetOf(f), s"$t is not an implicant")
          // prime: dropping any one literal leaves the function
          for i <- t.indices do assert(!rows(t.patch(i, Nil, 1)).subsetOf(f), s"$t is not prime")
        // irredundant: every term is the only one holding some row
        for (t, i) <- cover.terms.zipWithIndex do
          val others = cover.terms.patch(i, Nil, 1).flatMap(rows).toSet
          assert(!rows(t).subsetOf(others), s"$t is redundant")
  }
