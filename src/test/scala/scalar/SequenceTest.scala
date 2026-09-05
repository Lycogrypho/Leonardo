package it.grypho.scala.leonardo
package scalar

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.28 — numeric sequences and the generic tabulator.
 *
 *  "Numeric", not "integer": the recurrence runs on the ordinary `Sum`/`Product` eval, so it
 *  is closed over whatever the seeds are — `fib(5, 1.5, -pi)` is a legitimate call, and the
 *  exact tier carries rational seeds through unrounded.
 *
 *  Indexing is the standard one (`x(0) = 0`, `x(1) = 1`, so `fib(10) = 55`, OEIS A000045);
 *  the "classic rabbit" pair is `fib(n, 1, 1)`, which is the same sequence shifted by one.
 */
class SequenceTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def num(src: String): Double =
    parse(src).eval(env) match
      case Right(_Number(d)) => d
      case other             => fail(s"'$src' did not fold to a number: $other")

  /** Evaluates in exact mode, returning the value's `toString` (a `_Rational` prints exactly). */
  private def exact(src: String): String =
    Parser.parse(src, Some(30)) match
      case Parser.Success(e, _) => e.eval(new Environment(workingPrecision = 30)) match
        case Right(v) => v.toString
        case other    => fail(s"'$src' stayed symbolic: $other")
      case other => fail(s"parse failed for '$src': $other")

  private def row(src: String): Vector[String] =
    parse(src).eval(env).toExpression match
      case m: _Matrix => assert(m.rows == 1, s"expected a row, got ${m.rows}x${m.cols}"); m.elems.map(_.toString)
      case other      => fail(s"'$src' did not evaluate to a matrix: $other")

  private def staysSymbolic(src: String): Unit =
    val r = parse(src).eval(env).toExpression
    assert(r.isInstanceOf[_Sequence] || r.isInstanceOf[_Tabulate] || r.isInstanceOf[_Function],
           s"'$src' should stay symbolic, got $r")

  // ── Fibonacci: the standard indexing ────────────────────────────────────────

  "fib" should "use the standard indexing x(0)=0, x(1)=1" in
  {
    assert(num("fib(0)") == 0.0)
    assert(num("fib(1)") == 1.0)
    assert(num("fib(2)") == 1.0)
    assert(num("fib(10)") == 55.0)     // OEIS A000045
    assert(num("fib(20)") == 6765.0)
  }

  it should "accept alternate seeds, the classic pair being a shift" in
  {
    // fib(n, 1, 1) is the "rabbit" sequence = fib(n+1)
    for n <- 0 to 12 do
      assert(num(s"fib($n, 1, 1)") == num(s"fib(${n + 1})"), s"at n=$n")
  }

  // ── the named variants are the same node, and agree with the seeded form ────

  "the named variants" should "agree with fib's generalisation" in
  {
    // lucas(n) = fib(n, 2, 1): 2, 1, 3, 4, 7, 11, ...
    assert(num("lucas(0)") == 2.0 && num("lucas(1)") == 1.0 && num("lucas(6)") == 18.0)
    for n <- 0 to 10 do assert(num(s"lucas($n)") == num(s"fib($n, 2, 1)"), s"lucas at $n")
    // pell:       0, 1, 2, 5, 12, 29, ...   x(n) = 2x(n-1) + x(n-2)
    assert(num("pell(5)") == 29.0)
    // jacobsthal: 0, 1, 1, 3, 5, 11, ...    x(n) = x(n-1) + 2x(n-2)
    assert(num("jacobsthal(5)") == 11.0)
  }

  it should "each round-trip under its own name" in
  {
    // numeric literals normalise to their Double spelling, as everywhere else in the library
    for src <- List("fib(7.0)", "lucas(7.0)", "pell(7.0)", "jacobsthal(7.0)",
                    "fib(7.0, 2.0, 3.0)") do
      assert(parse(src).toString == src, s"round-trip failed for $src")
  }

  // ── "numeric", not "integer" ────────────────────────────────────────────────

  "non-integer seeds" should "work, which is why these are numeric sequences" in
  {
    // fib(5, a, b) = 3a + 5b for the standard recurrence
    val a = 1.5
    val b = -math.Pi
    assert(math.abs(num("fib(5, 1.5, -pi)") - (3 * a + 5 * b)) < 1e-12)
  }

  // ── exactness past the Double integer range ─────────────────────────────────

  "the exact tier" should "give exact Fibonacci past F(78), where a Double cannot" in
  {
    // F(79) = 14472334024676221 exceeds 2^53, so the Double path can only approximate it
    assert(exact("fib(79)") == "14472334024676221")
    assert(exact("fib(100)") == "354224848179261915075")
    // and the last exactly-representable one still agrees with the Double path
    assert(num("fib(78)") == 8944394323791464.0)
  }

  it should "keep rational seeds exact" in
  {
    // fib(4, 1/2, 1/3) = 2*(1/2) + 3*(1/3) = 2  -> exact
    assert(exact("fib(4, 1/2, 1/3)") == "2")
  }

  // ── caps and refusals ───────────────────────────────────────────────────────

  "an out-of-range or non-integer index" should "stay symbolic" in
  {
    staysSymbolic("fib(-1)")
    staysSymbolic("fib(2.5)")
    staysSymbolic("fib(x)")
  }

  // ── the combinatorial batch ─────────────────────────────────────────────────

  "binom" should "give the binomial coefficient" in
  {
    assert(num("binom(5, 2)") == 10.0)
    assert(num("binom(10, 5)") == 252.0)
    assert(num("binom(7, 0)") == 1.0)
    assert(num("binom(5, 7)") == 0.0)   // k > n >= 0
    assert(num("binom(5, -1)") == 0.0)
  }

  it should "be exact for integers and generalise to a real upper index" in
  {
    assert(exact("binom(30, 15)") == "155117520")
    // generalised: binom(-1, 3) = (-1)(-2)(-3)/3! = -1
    assert(num("binom(-1, 3)") == -1.0)
  }

  "catalan" should "give the Catalan numbers" in
  {
    assert(Vector(0, 1, 2, 3, 4, 5).map(n => num(s"catalan($n)")) ==
           Vector(1.0, 1.0, 2.0, 5.0, 14.0, 42.0))
    assert(exact("catalan(20)") == "6564120420")
  }

  "harmonic" should "sum the reciprocals, exactly in the exact tier" in
  {
    assert(num("harmonic(0)") == 0.0)
    assert(math.abs(num("harmonic(4)") - (1 + 0.5 + 1.0 / 3 + 0.25)) < 1e-12)
    assert(exact("harmonic(4)") == "25/12")
  }

  it should "agree with digamma, which already existed" in
  {
    // H(n) = digamma(n+1) + gamma  (Euler-Mascheroni)
    val g = 0.5772156649015329
    assert(math.abs(num("harmonic(6)") - (num("digamma(7)") + g)) < 1e-9)
  }

  // ── tabulate ────────────────────────────────────────────────────────────────

  "tabulate" should "turn any expression into a row of values" in
  {
    assert(row("tabulate(k^2, k, 1, 5)") == Vector("1.0", "4.0", "9.0", "16.0", "25.0"))
    assert(row("tabulate(fib(k), k, 0, 8)") ==
           Vector("0.0", "1.0", "1.0", "2.0", "3.0", "5.0", "8.0", "13.0", "21.0"))
  }

  it should "work for every function, not just the sequences" in
  {
    // a Pascal row, which needs no new machinery
    assert(row("tabulate(binom(4, k), k, 0, 4)") == Vector("1.0", "4.0", "6.0", "4.0", "1.0"))
  }

  it should "produce an indexable row matrix" in
  {
    assert(num("at(tabulate(fib(k), k, 0, 10), 1, 11)") == 55.0)   // 1-based -> fib(10)
  }

  it should "stay symbolic on non-integer or inverted bounds, and honour the cap" in
  {
    staysSymbolic("tabulate(k, k, 1, 0.5)")
    staysSymbolic("tabulate(k, k, 5, 1)")
    staysSymbolic(s"tabulate(k, k, 1, ${MaxTabulateTerms + 1})")
  }

  it should "round-trip through toString" in
  {
    assert(parse("tabulate((k ^ 2.0), k, 1.0, 5.0)").toString == "tabulate((k ^ 2.0), k, 1.0, 5.0)")
  }

  // ── the names are reserved ──────────────────────────────────────────────────

  "the sequence names" should "be reserved words" in
  {
    for name <- List("fib", "lucas", "pell", "jacobsthal", "binom", "catalan", "harmonic",
                     "tabulate") do
      assert(Parser.parse(name).isInstanceOf[Parser.NoSuccess], s"$name should be reserved")
    assert(parse("fibre") == _Variable("fibre"))
  }
