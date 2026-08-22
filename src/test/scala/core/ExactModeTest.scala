package it.grypho.scala.leonardo
package core

import parser.Parser
import scalar.{Sum, Product, Ratio}
import org.scalatest.flatspec.AnyFlatSpec


/** End-to-end tests for the exact-arithmetic tier (issue 4.L slice A).
 *
 *  Spans parser, `scalar._Operation` and `scalar._Function`, so it lives here beside the
 *  value type rather than in any one of them.  The suite has three jobs: prove the
 *  arithmetic really is exact, pin the promotion lattice, and — the one most likely to catch
 *  a future mistake — assert that turning exact mode **on takes nothing away**.
 */
class ExactModeTest extends AnyFlatSpec:

  private val Digits = 30
  private val env    = new Environment()

  /** Parses in exact mode and evaluates, failing the test on a parse error. */
  private def exact(input: String): Either[_Expression, _Value] = evalWith(input, Some(Digits))

  /** Parses on the default `Double` path and evaluates. */
  private def plain(input: String): Either[_Expression, _Value] = evalWith(input, None)

  private def evalWith(input: String, mode: Option[Int]): Either[_Expression, _Value] =
    Parser.parse(input, mode) match
      case Parser.Success(e, _) => e.eval(env)
      case other                => fail(s"parse failed for \"$input\": $other")

  /** The exact value of `input`, failing the test if it did not stay exact. */
  private def rational(input: String): _Rational = exact(input) match
    case Right(r: _Rational) => r
    case other               => fail(s"\"$input\" did not stay exact: $other")

  private def r(n: Int, d: Int): _Rational =
    _Rational.of(BigInt(n), BigInt(d)) match
      case Some(v) => v
      case None    => fail(s"$n/$d has a zero denominator")

  // --- the arithmetic really is exact ---

  "exact mode" should "add tenths without the Double representation error" in
  {
    assert(rational("0.1 + 0.2") == r(3, 10))
    // The plain path gives 0.30000000000000004, which is the whole reason for the tier.
    assert(plain("0.1 + 0.2") != Right(_Rational.Zero))
    assert(0.1 + 0.2 != 0.3)
  }

  it should "read a literal as written, not as the Double nearest to it" in
  {
    // The reason exact mode has to be a PARSE-time decision: by the time 0.1 is a Double,
    // one tenth is already gone and nothing downstream can recover it.
    assert(rational("0.1") == r(1, 10))
  }

  it should "recover exactly through a division and its inverse" in
  {
    assert(rational("1/3 * 3") == _Rational.One)
    assert(rational("1/3 + 1/6") == r(1, 2))
  }

  it should "keep subtraction and unary minus inside the exact tier" in
  {
    // Subtraction is built as a + (-1)*b, so the -1 must itself be exact or every
    // subtraction would silently leave the tier through float contagion.
    assert(rational("3 - 5") == _Rational(-5 + 3))
    assert(rational("-2") == _Rational(-2))
    assert(rational("1/3 - 1/3") == _Rational.Zero)
  }

  it should "keep integer powers exact, including negative ones" in
  {
    assert(rational("2^10") == _Rational(1024))
    assert(rational("2^-2") == r(1, 4))
    assert(rational("(1/2)^3") == r(1, 8))
  }

  it should "not underflow where a Double would" in
  {
    // 1e-320 / 1e10 is 0.0 in Double arithmetic -- the result is below the smallest
    // denormal.  Exactly, it is 1/10^330 and perfectly representable.
    val tiny = rational("1e-320 / 1e10")
    assert(!tiny.isZero, "the exact tier must not underflow to zero")
    assert(tiny == _Rational.of(BigInt(1), BigInt(10).pow(330)).getOrElse(fail("unreachable")))
    assert(plain("1e-320 / 1e10") == Right(_Number(0.0)), "the Double path really does underflow")
  }

  // --- operations that leave the rationals ---

  "a transcendental" should "re-enter the tier at the working precision" in
  {
    val s = rational("sin(1/3)")
    assert(math.abs(s.toDouble - math.sin(1.0 / 3.0)) < 1e-12)
  }

  it should "make sin(pi) exactly zero" in
  {
    // pi becomes a rational approximation, so sin of it is ~1.2e-16 rather than 0.  The
    // re-approximation to the working precision is what snaps that noise to an exact zero.
    assert(rational("sin(pi)") == _Rational.Zero)
    assert(rational("ln(1)") == _Rational.Zero)
    assert(rational("exp(0)") == _Rational.One)
  }

  it should "keep an exact integer result exact" in
  {
    assert(rational("log(100)") == _Rational(2))
    assert(rational("fact(5)") == _Rational(120))
    assert(rational("dfact(7)") == _Rational(105))
  }

  it should "claim no more digits than a Double actually carries" in
  {
    // The working precision bounds the ARITHMETIC, not the transcendental kernels, which
    // are still Double-accurate.  Asking for 60 digits of sin must not manufacture 45.
    val e = new Environment(workingPrecision = 60)
    Parser.parse("sin(1/3)", Some(60)) match
      case Parser.Success(x, _) => x.eval(e) match
        case Right(v: _Rational) =>
          assert(v.den <= BigInt(10).pow(_Rational.DoubleReliableDigits),
                 s"denominator claims more precision than a Double has: $v")
        case other => fail(s"expected an exact result, got $other")
      case other => fail(s"parse failed: $other")
  }

  // --- the promotion lattice ---

  "an exact value meeting an inexact one" should "produce an inexact result" in
  {
    // Float contagion, and deliberately the asymmetric choice: absorbing the Double would
    // be lossless, but it would dress representation error up as an exact answer.
    assert(Sum(_Rational(1), _Number(0.5)).eval(env) == Right(_Number(1.5)))
    assert(Product(_Rational(2), _Number(0.5)).eval(env) == Right(_Number(1.0)))
    assert(Ratio(_Rational(1), _Number(2.0)).eval(env) == Right(_Number(0.5)))
  }

  it should "still combine with a complex value" in
  {
    // _Complex.parts reads a rational, so the complex kernels need no rational cases.
    assert(Sum(_Rational(2), _Complex.of(0, 3)).eval(env) == Right(_Complex.of(2, 3)))
  }

  "an exact zero" should "short-circuit a product without being demoted" in
  {
    assert(exact("0 * 5") == Right(_Rational.Zero))
    assert(Product(_Rational(0), _Variable("x")).eval(env) == Right(_Rational.Zero))
  }

  "a domain error" should "stay symbolic in exact mode too" in
  {
    assert(exact("1/0").isLeft)
  }

  // --- display and serialization are separate paths ---

  "display" should "show a short value as a fraction" in
  {
    assert(r(1, 3).display(5) == "1/3")
    assert(r(2, 6).display(5) == "1/3", "reduces before deciding")
    assert(_Rational(7).display(5) == "7")
    assert(r(-1, 3).display(5) == "-1/3")
  }

  it should "fall back to a decimal once the fraction stops being readable" in
  {
    val pi = rational("pi")
    assert(pi.display(5) == "3.14159", s"got ${pi.display(5)}")
    assert(pi.display(2) == "3.14")
  }

  "serialization" should "always write the exact fraction, whatever display chose" in
  {
    // The split that makes the display rule safe: a value shown rounded still round-trips.
    val pi = rational("pi")
    assert(pi.exact.contains("/"), s"pi must serialize as a fraction, got ${pi.exact}")
    assert(pi.display(5) == "3.14159", "...while still displaying readably")
    assert(r(2, 6).exact == "1/3", "reduced, so the gcd policy never leaks into a script")
    assert(_Rational(7).exact == "7")
  }

  it should "round-trip an exact value through its serialized form" in
  {
    val pi = rational("pi")
    Parser.parse(pi.exact, Some(Digits)) match
      case Parser.Success(e, _) => assert(e.eval(env) == Right(pi))
      case other                => fail(s"serialized form did not re-parse: $other")
  }

  // --- turning exact mode on must take nothing away ---

  "exact mode" should "not make any expression less reducible than the Double path" in
  {
    // The property that matters most for future work: a new _Value type silently breaks
    // every node that pattern-matches _Number, and the fix (the widening core._Number
    // extractor) is easy to undo by accident.  This is the guard.
    val corpus = List(
      "[[1,2],[3,4]]", "det([[1,2],[3,4]])", "inv([[1,2],[3,4]])", "transpose([[1,2],[3,4]])",
      "eye(2)", "zeros(2)", "[[1,2],[3,4]] * [[1,0],[0,1]]", "[[1,2],[3,4]]^2",
      "integral(x, x, 0, 1)", "limit(sin(x)/x, x, 0)", "ode(y, y, t, 0, 1, 1)",
      "step(3)", "trimf(5, 0, 5, 10)", "defuzz(trimf(v, 0, 5, 10), v, 0, 10)",
      "truth(0.3)", "1 = 1", "true and false", "2 + 3i", "Gamma(5)", "Beta(2,3)",
      "at([[1,2],[3,4]], 1, 2)", "lu([[4,3],[6,3]])"
    )
    for input <- corpus do
      if plain(input).isRight then
        assert(exact(input).isRight,
               s"""exact mode lost "$input": plain=${plain(input)} exact=${exact(input)}""")
  }

  it should "not make any expression evaluate to a different number" in
  {
    val corpus = List(
      "2 + 3", "10 / 4", "2^8", "sin(0)", "exp(1)", "log(1000)", "fact(6)",
      "det([[1,2],[3,4]])", "integral(x^2, x, 0, 3)", "limit(sin(x)/x, x, 0)", "step(-1)"
    )
    for input <- corpus do
      (exact(input), plain(input)) match
        case (Right(a), Right(b)) =>
          val (x, y) = (asDouble(a), asDouble(b))
          assert(math.abs(x - y) < 1e-9, s"""exact and plain disagree on "$input": $x vs $y""")
        case (a, b) => assert(a.isLeft == b.isLeft, s"""reducibility differs on "$input"""")
  }

  private def asDouble(v: _Value): Double = v match
    case _Number(d) => d
    case other      => fail(s"expected a real result, got $other")
