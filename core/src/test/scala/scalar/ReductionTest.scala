package it.grypho.scala.leonardo
package scalar

import org.scalatest.flatspec.AnyFlatSpec

import core.*
import parser.Parser

/** Issue F_0037 — finite `sum` / `product`, the reductions `tabulate` could not express.
 *
 *  `tabulate(e, k, lo, hi)` yields the *terms*; these fold them.  That is why the AsciiMath
 *  reader refused a summation by name until now — `tabulate` is not an answer to a `Σ`, and
 *  the grammar had nothing else one could become.
 *
 *  **One node, two kinds** (`_Reduction` / `ReduceKind`), the `_Sequence`/`SeqKind` pattern:
 *  the two differ only in which binary node combines the terms and what an empty range is
 *  worth, so the family has one definition rather than two that could drift.
 */
class ReductionTest extends AnyFlatSpec:

  private def evalOf(src: String, exact: Option[Int] = None): Either[_Expression, _Value] =
    val p = Parser.parse(src, exact)
    assert(p.successful, s"'$src' must parse: $p")
    p.get.eval(new Environment(5, Map.empty))

  "sum" should "add the terms over an inclusive integer range" in
  {
    assert(evalOf("sum(k, k, 1, 5)") == Right(_Number(15.0)))
    assert(evalOf("sum(k^2, k, 1, 4)") == Right(_Number(30.0)))
  }

  "product" should "multiply them" in
  {
    assert(evalOf("product(k, k, 1, 5)") == Right(_Number(120.0)))
  }

  "the bounds" should "be inclusive at both ends, and a single-term range work" in
  {
    assert(evalOf("sum(k, k, 3, 3)") == Right(_Number(3.0)))
    assert(evalOf("sum(k, k, -2, 2)") == Right(_Number(0.0)))
  }

  "an empty range" should "be the identity, which is what makes a base case work" in
  {
    // The universal convention, and not merely tidiness: `sum(f, k, 1, n)` at n = 0 is the
    // empty sum, so a recurrence's base case reads correctly instead of refusing.
    assert(evalOf("sum(k, k, 1, 0)") == Right(_Number(0.0)))
    assert(evalOf("product(k, k, 1, 0)") == Right(_Number(1.0)))
  }

  "an exact term" should "stay exact, since no inexact identity is ever injected" in
  {
    // The numeric tier rule: the fold uses `reduce`, never a `fold` seeded with a literal
    // `_Number(0)` -- which would demote the whole sum through float contagion.
    evalOf("sum(1/k, k, 1, 3)", exact = Some(30)) match
      case Right(r: _Rational) => assert(r.exact == "11/6", r.exact)
      case other               => fail(s"the exact tier was lost: $other")
  }

  "a symbolic term" should "expand into the combined expression, not stay folded" in
  {
    // Expanding is what makes the node useful in a CAS: the terms become an ordinary
    // expression that `normalize`/`simplify` can then collect -- and since `_Reduction` is a
    // `_Functional`, F_0034's pass reduces it under the `simplify` command automatically.
    val out = evalOf("sum(k*x, k, 1, 3)")
    assert(out.isLeft, out)
    val collected = normalize(out.toExpression, _Variable("x")).toString
    assert(collected.contains("6"), s"x + 2x + 3x should collect to 6x; got $collected")
  }

  "a free bound" should "stay symbolic rather than guess" in
  {
    val out = evalOf("sum(k, k, 1, n)")
    assert(out.isLeft, out)
    assert(out.toExpression.toString == "sum(k, k, 1.0, n)", out.toExpression.toString)
  }

  it should "be refused past the term cap, the MaxTabulateTerms rule" in
  {
    // A mistyped bound must decline rather than build an unbounded expression -- the same
    // reasoning `tabulate` and `MaxSampleCount` follow.
    assert(evalOf(s"sum(k, k, 1, ${MaxTabulateTerms + 1})").isLeft)
    assert(evalOf(s"sum(k, k, 1, $MaxTabulateTerms)").isRight)
  }

  "the index" should "be a binder: outside children, and carried through rebuild" in
  {
    // What "binder" means here, checked against what the project actually does rather than
    // what the word suggests: the binder POSITION is excluded from `children` and survives
    // `rebuild`, so a traversal cannot rewrite the variable the reduction is taken over.
    // It is NOT hidden from `freeVars` -- `derive`, `integral`, `tabulate` and `taylor` all
    // report their binder there too, since the body's occurrences are ordinary ones.
    val e = Parser.parse("sum(k*x, k, 1, 3)").get
    assert(e.children.sizeIs == 3, s"the binder must not be a child: ${e.children}")
    assert(!e.children.contains(_Variable("k")))
    val rebuilt = e.rebuild(List(_Variable("y"), _Number(2), _Number(4)))
    assert(rebuilt.toString == "sum(y, k, 2.0, 4.0)", rebuilt.toString)
  }

  "both spellings" should "round-trip through toString" in
  {
    for src <- List("sum(k, k, 1, n)", "product(k, k, 1, n)") do
      val e = Parser.parse(src)
      assert(e.successful, src)
      assert(Parser.parse(e.get.toString).successful, s"round-trip of $src")
  }

  it should "be reserved words, since a production needs them" in
  {
    assert(Parser.ReservedWords.contains("sum"))
    assert(Parser.ReservedWords.contains("product"))
  }
