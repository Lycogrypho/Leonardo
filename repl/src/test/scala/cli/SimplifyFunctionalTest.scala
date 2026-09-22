package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** Issue F_0034 — `simplify` must never return something less reduced than the input.
 *
 *  `simplifyFully` is *structural*: it folds constants and identities but never calls `eval`,
 *  which is where every `_Functional` computes itself.  So `derive(sin(x), x)` answered
 *  `cos(x)` while `simplify derive(sin(x), x)` answered itself — the same question, answered
 *  differently because a command word preceded it.
 *
 *  **The defence that `simplify` deliberately does not evaluate was already half-spent**: the
 *  pipeline runs `resolveMatrixOps`, which *does* evaluate matrix operations, precisely so
 *  `simplify C` can execute a product.  The question was never whether it may evaluate, but why
 *  it evaluated one kind of node and not another.
 *
 *  What it still must not do is fold a *binding* — that is the documented difference between
 *  `simplify` and plain evaluation, and the second half of this suite pins it.
 */
class SimplifyFunctionalTest extends AnyFlatSpec:

  private def session: Session = new Session()

  // --- a functional must reduce, wherever it sits ----------------------------------------

  "simplify" should "differentiate, as bare evaluation already did" in
  {
    assert(session.execute("simplify derive(sin(x), x)") == "cos(x)")
  }

  it should "integrate" in
  {
    val out = session.execute("simplify integral(2x, x)")
    assert(!out.contains("integral"), out)
  }

  it should "take a limit" in
  {
    val out = session.execute("simplify limit(sin(x)/x, x, 0)")
    assert(!out.contains("limit"), out)
  }

  it should "reduce a functional nested inside an ordinary expression" in
  {
    // The pass is bottom-up over `children`, so depth is not special.
    val out = session.execute("simplify 1 + derive(sin(x), x)")
    assert(out.contains("cos(x)"), out)
    assert(!out.contains("derive"), out)
  }

  it should "agree with bare evaluation on the same input" in
  {
    // The invariant the issue is really about: a command word must not change the answer.
    val s = session
    for line <- List("derive(sin(x), x)", "derive(x^3, x)", "integral(2x, x)") do
      assert(s.execute(s"simplify $line") == s.execute(line), line)
  }

  "expand" should "reduce a functional too, since it shares the preparation" in
  {
    assert(session.execute("expand derive(sin(x), x)") == "cos(x)")
  }

  "a functional with no closed form" should "stay symbolic rather than being mangled" in
  {
    // `exp(x^2)` has no elementary antiderivative; the node must come back untouched, which is
    // also what makes the pass idempotent.
    val out = session.execute("simplify integral(exp(x^2), x)")
    assert(out.contains("integral"), out)
  }

  it should "leave the pass idempotent" in
  {
    // Re-simplifying the ANSWER, not nesting the command: `simplify` is a reserved word, so
    // `simplify simplify f` is a parse error rather than a second pass.
    val s = session
    val once  = s.execute("simplify derive(sin(x), x)")
    val twice = s.execute(s"simplify $once")
    assert(once == twice, s"'$once' then '$twice'")
  }

  // --- what must NOT change: simplify still ignores bindings -------------------------------

  "a numeric binding" should "still be ignored by simplify" in
  {
    // The documented difference between `simplify` and evaluation. Functionals are evaluated
    // in an environment carrying the session's PRECISION but no variables, so this holds
    // inside a functional exactly as it does outside one.
    val s = session
    s.execute("x := 2")
    assert(s.execute("simplify x + 0") == "x")
  }

  it should "be ignored inside a functional as well" in
  {
    val s = session
    s.execute("x := 2")
    // The derivative is taken, but `x` is not then replaced by 2 -- otherwise `simplify` would
    // fold bindings in one place and not the other, which is worse than either rule alone.
    assert(s.execute("simplify derive(sin(x), x)") == "cos(x)")
  }

  "a definition" should "still be substituted first, as it always was" in
  {
    val s = session
    s.execute("g := sin(x)")
    assert(s.execute("simplify derive(g, x)") == "cos(x)")
  }
