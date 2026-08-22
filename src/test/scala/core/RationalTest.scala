package it.grypho.scala.leonardo
package core

import org.scalatest.flatspec.AnyFlatSpec


/** Correctness suite for the exact-arithmetic kernel (issues 4.M / 4.L).
 *
 *  Two jobs.  The first is ordinary: exact arithmetic, the bounded-denominator
 *  approximation, conversions and ordering.  The second is the one that outlives the
 *  gcd-policy decision — asserting that all three [[GcdPolicy]] arms produce *equal values*
 *  on every workload, since the policy is allowed to change representation and nothing else.
 *  That doubles as a genuine correctness test of the [[GcdPolicy.Lazy]] path, which no other
 *  test would otherwise exercise.
 */
class RationalTest extends AnyFlatSpec:

  private val policies: List[GcdPolicy] =
    List(GcdPolicy.Eager, GcdPolicy.Lazy, GcdPolicy.Threshold(_Rational.DefaultThresholdBits))

  /** The kernel's default policy must stay the one the 4.M benchmark actually chose —
   *  otherwise the recorded decision and the shipped behaviour drift apart silently.
   */
  "the default policy" should "be the benchmarked Threshold, not Eager" in
  {
    assert(_Rational.DefaultPolicy == GcdPolicy.Threshold(256))
    assert(_Rational.thresholdFor(10) == GcdPolicy.Threshold(256),  "floored at the default")
    assert(_Rational.thresholdFor(64) == GcdPolicy.Threshold(512),  "scales with working precision")
  }

  /** Exact constructor for a denominator known non-zero, failing the test otherwise. */
  private def r(n: Int, d: Int, p: GcdPolicy = GcdPolicy.Eager): _Rational =
    _Rational.of(BigInt(n), BigInt(d), p) match
      case Some(v) => v
      case None    => fail(s"$n/$d has a zero denominator")

  /** pi to 50 decimal places, as an exact rational — the reference value for the
   *  continued-fraction tests, whose convergents 22/7, 333/106 and 355/113 are the classic
   *  worked examples.
   */
  private val piApprox: _Rational =
    _Rational.make(BigInt("314159265358979323846264338327950288419716939937510"),
                   BigInt(10).pow(50), GcdPolicy.Eager)

  // --- exact arithmetic: the point of the tier ---

  "_Rational" should "add tenths exactly, where Double cannot" in
  {
    // 0.1 + 0.2 == 0.3 exactly, not to within a tolerance.
    assert(r(1, 10).add(r(2, 10)) == r(3, 10))
    assert(0.1 + 0.2 != 0.3, "the Double comparison this replaces really does fail")
  }

  it should "recover exactly from a division and its inverse" in
  {
    assert(r(1, 3).multiply(_Rational(3)) == _Rational.One)
  }

  it should "compare equal across representations" in
  {
    assert(r(2, 6, GcdPolicy.Lazy) == r(1, 3))
    assert(r(2, 6, GcdPolicy.Lazy).hashCode == r(1, 3).hashCode)
  }

  it should "normalise the sign into the numerator" in
  {
    val neg = r(1, -3)
    assert(neg.den.signum > 0)
    assert(neg == r(-1, 3))
  }

  it should "reject a zero denominator instead of throwing" in
  {
    assert(_Rational.of(BigInt(1), BigInt(0)).isEmpty)
    assert(r(1, 3).divide(_Rational.Zero).isEmpty)
    assert(_Rational.Zero.reciprocal().isEmpty)
    assert(_Rational.Zero.pow(-1).isEmpty)
  }

  it should "raise to integer powers, negative included" in
  {
    assert(r(2, 3).pow(3).contains(r(8, 27)))
    assert(r(2, 3).pow(-2).contains(r(9, 4)))
    assert(r(2, 3).pow(0).contains(_Rational.One))
  }

  it should "order by value regardless of representation" in
  {
    assert(r(1, 3) < r(1, 2))
    assert(r(-1, 3) < r(1, 3))
    assert(r(2, 6, GcdPolicy.Lazy).compare(r(1, 3)) == 0)
  }

  // --- reduction policy: representation only, never value ---

  "GcdPolicy" should "leave Lazy results unreduced and Eager results reduced" in
  {
    val lazyProduct  = r(2, 3, GcdPolicy.Lazy).multiply(r(3, 2, GcdPolicy.Lazy), GcdPolicy.Lazy)
    val eagerProduct = r(2, 3).multiply(r(3, 2), GcdPolicy.Eager)
    assert(lazyProduct.num == BigInt(6) && lazyProduct.den == BigInt(6), s"got $lazyProduct")
    assert(eagerProduct.num == BigInt(1) && eagerProduct.den == BigInt(1), s"got $eagerProduct")
    // ...and yet the two are the same number.  This is the invariant the whole spike rests on.
    assert(lazyProduct == eagerProduct)
  }

  it should "reduce under Threshold only once an operand outgrows the bound" in
  {
    val tight = GcdPolicy.Threshold(4)
    val small = _Rational.make(BigInt(2), BigInt(6), tight)   // 3 bits -- left alone
    val big   = _Rational.make(BigInt(200), BigInt(600), tight) // 10 bits -- reduced
    assert(small.den == BigInt(6), s"got $small")
    assert(big.den == BigInt(3), s"got $big")
  }

  // --- bounded-denominator approximation ---

  "approximate" should "produce the classical convergents of pi" in
  {
    assert(piApprox.approximate(BigInt(7))   == r(22, 7))
    assert(piApprox.approximate(BigInt(113)) == r(355, 113))
    assert(piApprox.approximate(BigInt(110)) == r(333, 106))
  }

  it should "use a semiconvergent when it beats the last full convergent" in
  {
    // Denominator <= 100 rules out 333/106; the best that fits is the semiconvergent
    // 311/99 = 3.1414..., closer to pi than the convergent 22/7 = 3.1428... .
    assert(piApprox.approximate(BigInt(100)) == r(311, 99))
  }

  it should "pick the nearer neighbour on a tie-free small bound" in
  {
    assert(r(1, 3).approximate(BigInt(2)) == r(1, 2))
  }

  it should "leave a value alone when it already fits the bound" in
  {
    assert(r(3, 7).approximate(BigInt(10)) == r(3, 7))
    assert(r(6, 14, GcdPolicy.Lazy).approximate(BigInt(10)) == r(3, 7))
  }

  it should "depend on the value only, never on the representation" in
  {
    // The property every cross-policy equality assertion below rests on.
    val bound = BigInt(50)
    assert(piApprox.approximate(bound) ==
             _Rational.make(piApprox.num * 7, piApprox.den * 7, GcdPolicy.Lazy).approximate(bound))
  }

  it should "bound the error by roughly one over the squared denominator" in
  {
    val approx = piApprox.approximateToDigits(6)
    val err    = approx.subtract(piApprox).abs
    assert(err < r(1, 1000000), s"error $err too large for 6 digits")
  }

  // --- conversions ---

  "fromDouble" should "recover the exact dyadic a Double actually holds" in
  {
    _Rational.fromDouble(0.1) match
      case None    => fail("0.1 is finite and must convert")
      case Some(v) =>
        // 0.1 is stored as 3602879701896397 / 2^55.
        assert(v == _Rational.make(BigInt("3602879701896397"), BigInt(2).pow(55), GcdPolicy.Eager))
        // ...and it is NOT one tenth.  This is why 4.L needs an exact mode in the parser:
        // by the time a literal is a Double the exactness is already gone.
        assert(v != r(1, 10))
  }

  it should "return None for the non-finite Doubles" in
  {
    assert(_Rational.fromDouble(Double.NaN).isEmpty)
    assert(_Rational.fromDouble(Double.PositiveInfinity).isEmpty)
  }

  "toDouble" should "survive terms far outside Double's range" in
  {
    // Both terms overflow Double individually; the value is a perfectly ordinary 1/2.
    val huge = _Rational.make(BigInt(10).pow(400), BigInt(2) * BigInt(10).pow(400), GcdPolicy.Lazy)
    assert(math.abs(huge.toDouble - 0.5) < 1e-12, s"got ${huge.toDouble}")
  }

  // --- participation in the AST (4.L slice A; 4.M shipped the kernel alone) ---

  "the exact tier" should "be a concrete value that evaluates to itself" in
  {
    assert(_Rational.One.isInstanceOf[_Value])
    assert(_Rational.One.eval(new Environment()) == Right(_Rational.One))
    assert(_Rational.One.children.isEmpty)
    assert(_Rational.One.freeVars.isEmpty)
  }

  it should "be read as a plain real number by the widening _Number extractor" in
  {
    // The one decision that let the tier be added without editing the ~108 `case _Number(x)`
    // sites: the pattern means "reads as a real number", so every node that does not opt in
    // to exactness keeps working and merely degrades to the Double it already used.
    r(1, 2) match
      case _Number(d) => assert(d == 0.5)
      case other      => fail(s"a rational must read as a number, got $other")
  }

  it should "still be distinguishable from a _Number by type" in
  {
    // Degrading is opt-out, not irreversible: the exact cases in scalar._Operation select
    // on the type, which is why they must be matched BEFORE any _Number case.
    assert(!r(1, 2).isInstanceOf[_Number])
    assert(r(1, 2) != _Number(0.5), "exact and inexact halves are different values")
  }

  // --- cross-policy agreement on the benchmark workloads ---

  "all three policies" should "agree on the primitive chain" in
  {
    for digits <- List(Some(10), Some(30), None) do
      val results = policies.map(p => RationalWorkloads.primitiveChain(60, p, digits, new BitStats))
      assert(results.distinct.size == 1, s"policies disagreed at digits=$digits: $results")
  }

  it should "agree on the Hilbert solve" in
  {
    for digits <- List(Some(10), Some(30), None) do
      val results = policies.map(p => RationalWorkloads.hilbertSolve(4, p, digits, new BitStats))
      assert(results.distinct.size == 1, s"policies disagreed at digits=$digits: $results")
      assert(results.head.isDefined, "the 4x4 Hilbert system is non-singular and must solve")
  }

  it should "agree on the exponential series" in
  {
    for digits <- List(Some(10), Some(30), None) do
      val results = policies.map(p => RationalWorkloads.expSeries(15, r(1, 3), p, digits, new BitStats))
      assert(results.distinct.size == 1, s"policies disagreed at digits=$digits: $results")
  }

  "the exponential series" should "converge on exp(1/3)" in
  {
    val sum = RationalWorkloads.expSeries(20, r(1, 3), GcdPolicy.Eager, None, new BitStats)
    assert(math.abs(sum.toDouble - math.exp(1.0 / 3.0)) < 1e-12, s"got ${sum.toDouble}")
  }

  "the Hilbert solve" should "return the exact known solution for n = 3" in
  {
    // H3 x = 1 has the exact integer solution (3, -24, 30).
    RationalWorkloads.hilbertSolve(3, GcdPolicy.Eager, None, new BitStats) match
      case None    => fail("the 3x3 Hilbert system is non-singular and must solve")
      case Some(x) => assert(x == Vector(_Rational(3), _Rational(-24), _Rational(30)), s"got $x")
  }
