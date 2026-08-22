package it.grypho.scala.leonardo
package core

import parser.Parser
import equation._Equation
import org.scalatest.flatspec.AnyFlatSpec


/** Arbitrary-precision transcendentals — issue 4.N, tier 2 of the exact-arithmetic plan.
 *
 *  Tier 1 made the *arithmetic* exact but still evaluated every transcendental in `Double`
 *  and re-approximated, capped at `_Rational.DoubleReliableDigits`.  So `exact precision 60`
 *  sharpened everything around a `sin()` and nothing about the `sin()` itself, and
 *  `exp(1000)` overflowed to an infinity.  These tests pin the two acceptance cases that
 *  were deferred out of tier 1 for exactly that reason.
 */
class IrrationalTest extends AnyFlatSpec:

  /** Parses in exact mode at `digits` and evaluates at the same working precision. */
  private def at(input: String, digits: Int): Either[_Expression, _Value] =
    Parser.parse(input, Some(digits)) match
      case Parser.Success(e, _) => e.eval(new Environment(workingPrecision = digits))
      case other                => fail(s"parse failed for \"$input\": $other")

  private def rational(input: String, digits: Int): _Rational = at(input, digits) match
    case Right(r: _Rational) => r
    case other               => fail(s"\"$input\" @$digits did not stay exact: $other")

  // --- the two deferred acceptance cases ---

  "exp(1000)" should "be an exact 435-digit value rather than an overflow" in
  {
    // The Double kernel returns Infinity here, so tier 1 had to stay symbolic. The true
    // value is finite and perfectly representable once the kernel is not a Double.
    assert(math.exp(1000.0).isInfinite, "the Double path really does overflow")
    val v = rational("exp(1000)", 30)
    val whole = v.toBigDecimal(30).toBigInt
    assert(whole.toString.length == 435, s"exp(1000) has 435 integer digits, got ${whole.toString.length}")
    assert(v.toBigDecimal(20).toString.startsWith("1.9700711140170469939E+434"),
           s"got ${v.toBigDecimal(20)}")
  }

  "the ill-conditioned quadratic" should "improve monotonically with the working precision" in
  {
    // The property tier 2 adds over issue 1.1's per-case fix: 1.1 made this one formula
    // correct, this makes the accuracy a knob the user controls.
    //
    // Measured by the RESIDUAL of the recovered root, not against a reference constant --
    // an early version of this compared against a 30-digit literal and "found" an error of
    // 2e-40, which was the error in the literal, not in the root.
    def residualAt(digits: Int): BigDecimal =
      at("solve(x^2 + 1e8*x + 1 = 0, x)", digits) match
        case Left(m) =>
          val small = m.children.collect { case eq: _Equation => eq.rhs }
                       .collect { case r: _Rational if r.abs < _Rational.One => r }
          small.headOption match
            case None    => fail(s"no small root at $digits digits")
            case Some(r) =>
              val p = _Rational.thresholdFor(digits)
              r.multiply(r, p)
               .add(_Rational(BigInt(100000000)).multiply(r, p), p)
               .add(_Rational.One, p)
               .toBigDecimal(10).abs
        case other => fail(s"solve did not produce a solution set: $other")

    val residuals = List(20, 30, 40, 60).map(residualAt)
    for (a, b) <- residuals.zip(residuals.tail) do
      assert(b < a, s"residual must shrink as precision rises, got $residuals")
    assert(residuals.last < BigDecimal("1e-70"), s"60 digits should be very accurate: ${residuals.last}")
    // ...and every one of them already beats what a Double can do.
    val doubleResidual = BigDecimal(1.1102230246251565E-16)
    assert(residuals.head < doubleResidual, s"even 20 digits must beat Double: ${residuals.head}")
  }

  // --- precision is genuinely delivered, not merely requested ---

  "a transcendental" should "carry as many correct digits as were asked for" in
  {
    // sin(1/3) to 40 places, independently computed.
    val reference = BigDecimal("0.3271946967961522441733440852676206060643")
    for digits <- List(20, 30, 40) do
      val v = rational("sin(1/3)", digits)
      val err = (v.toBigDecimal(45) - reference).abs
      assert(err < BigDecimal(s"1e-${digits - 2}"), s"sin(1/3) @$digits off by $err")
  }

  it should "cover the whole elementary set" in
  {
    val cases = List(
      ("exp(1)",   "2.718281828459045235360287471352662497757"),
      ("ln(2)",    "0.693147180559945309417232121458176568076"),
      ("sin(1)",   "0.841470984807896506652502321630298999622"),
      ("cos(1)",   "0.540302305868139717400936607442976603733"),
      ("tan(1)",   "1.557407724654902230506974807458360173087"),
      ("atan(1)",  "0.785398163397448309615660845819875721049"),
      ("asin(1/2)","0.523598775598298873077107230546583814032"),
      ("acos(1/2)","1.047197551196597746154214461093167628066")
    )
    for (input, expected) <- cases do
      val v = rational(input, 30)
      assert((v.toBigDecimal(38) - BigDecimal(expected)).abs < BigDecimal("1e-28"),
             s"$input gave ${v.toBigDecimal(38)}, expected $expected")
  }

  "pi and e" should "be computed rather than read off a Double" in
  {
    // Tier 1 approximated these from math.Pi / math.E, so they could never exceed 15 places.
    val pi = rational("pi", 40)
    assert((pi.toBigDecimal(45) - BigDecimal("3.14159265358979323846264338327950288419716939937510")).abs
             < BigDecimal("1e-38"), s"pi @40 = ${pi.toBigDecimal(45)}")
    val e = rational("e", 40)
    assert((e.toBigDecimal(45) - BigDecimal("2.71828182845904523536028747135266249775724709369995")).abs
             < BigDecimal("1e-38"), s"e @40 = ${e.toBigDecimal(45)}")
  }

  "a fractional power" should "reach the working precision too" in
  {
    val root2 = rational("2^0.5", 30)
    assert((root2.toBigDecimal(35) - BigDecimal("1.41421356237309504880168872420969807857")).abs
             < BigDecimal("1e-28"), s"got ${root2.toBigDecimal(35)}")
  }

  // --- domain handling is unchanged ---

  "an out-of-domain argument" should "still take the complex or symbolic route" in
  {
    // The exact kernels report their own domain rather than computing in Double first and
    // testing for a finite result -- that oracle gets exp(1000) exactly wrong.
    assert(at("ln(-1)", 30).exists(_.isInstanceOf[_Complex]), "ln of a negative is complex")
    assert(at("ln(0)", 30).isLeft, "ln(0) is undefined")
    assert(at("asin(2)", 30).isLeft || at("asin(2)", 30).exists(_.isInstanceOf[_Complex]),
           "asin beyond 1 is not a real value")
  }

  "sin(pi)" should "stay exactly zero at every precision" in
  {
    for digits <- List(15, 30, 60) do
      assert(rational("sin(pi)", digits) == _Rational.Zero, s"sin(pi) @$digits")
  }

  // --- display must not report a non-zero value as zero ---

  "a small exact value" should "never display as zero" in
  {
    // Rounding -1e-8 to five decimals gives 0.0, which for a Double is merely ambiguous but
    // for an exact value is false -- and it is precisely the value the tier exists to
    // recover.  Below the display precision the rendering switches to significant digits.
    // A short one is still readable as a fraction, which already says it is not zero.
    val short = _Rational.of(BigInt(-1), BigInt(10).pow(8)).getOrElse(fail("unreachable"))
    assert(short.display(5) == "-1/100000000", s"got ${short.display(5)}")

    // A long one has to fall back to the decimal form, and that is where rounding used to
    // turn it into "0.0".  This is the shape the quadratic's small root actually takes.
    val long = _Rational.of(BigInt(-3), BigInt(10).pow(20) + 7).getOrElse(fail("unreachable"))
    val shown = long.display(5)
    assert(BigDecimal(shown) != BigDecimal(0), s"a non-zero value displayed as zero: $shown")
    assert(BigDecimal(shown).abs < BigDecimal("1e-19"), s"magnitude must survive: $shown")

    // A genuine zero still prints as a plain zero.
    assert(_Rational.Zero.display(5) == "0")
  }

  "a value beyond Double's range" should "display as a decimal, not an 800-digit fraction" in
  {
    // exp(1000) is exact but its fraction is unreadable, and toDouble is an infinity, so
    // neither of the other two renderings says anything useful.  BigDecimal has no range
    // limit.  :save still writes the exact fraction, so nothing is lost by this.
    val big = rational("exp(1000)", 40)
    assert(big.display(5) == "1.9701E+434", s"got ${big.display(5)}")
    assert(big.exact.contains("/"), "serialization is still the exact fraction")
    // An exact INTEGER of any size still prints in full -- 171! must not become 1.2410E+308.
    val f171 = rational("fact(171)", 30)
    assert(f171.display(5).length == 310, s"171! should print all 310 digits, got ${f171.display(5).take(30)}")
  }

  // --- the Double fast path ---

  "a low working precision" should "keep using the Double kernel" in
  {
    // Nothing to gain below ~15 digits and roughly 110x to lose, so the Double kernel is
    // kept there.  The two agree to within the precision either claims, which is what makes
    // the choice unobservable apart from the speed.
    val fast = rational("sin(1/3)", 10)
    val slow = rational("sin(1/3)", 30)
    assert((fast.toBigDecimal(20) - slow.toBigDecimal(20)).abs < BigDecimal("1e-9"),
           s"the two paths must agree to the lower precision: $fast vs $slow")
  }

  "the default path" should "be completely untouched by any of this" in
  {
    // Nothing above is reachable without exact mode, which is off by default.
    Parser.parse("sin(1/3)", None) match
      case Parser.Success(e, _) =>
        assert(e.eval(new Environment()) == Right(_Number(math.sin(1.0 / 3.0))))
      case other => fail(s"parse failed: $other")
  }
