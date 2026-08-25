package it.grypho.scala.leonardo
package scalar

import core.*
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.3 slices A–C — domain of definition, differentiability, and the complex arm. */
class DomainTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")
  private val a   = _Variable("a")

  private def realDomain(e: _Expression): DomainSet = domainOf(e, x, DomainKind.Real, env)
  private def cplxDomain(e: _Expression): DomainSet = domainOf(e, x, DomainKind.Complex, env)

  private def reqs(d: DomainSet): Set[Requirement] = d.constraints.map(_.req).toSet

  // ── slice A: the requirement table ─────────────────────────────────────────

  "ln" should "require a positive argument over the reals" in
  {
    assert(reqs(realDomain(Ln(x))) == Set(Requirement.Positive))
  }

  it should "resolve to x > 0" in
  {
    realDomain(Ln(x)).intervals match
      case Some(Vector(i)) => assert(i.lo == 0.0 && !i.loIncl && i.hi.isPosInfinity)
      case other           => fail(s"expected one interval (0, inf), got $other")
  }

  "a denominator" should "require non-zero" in
  {
    assert(reqs(realDomain(Ratio(_Number(1), x))) == Set(Requirement.NonZero))
  }

  it should "split the line at the pole" in
  {
    realDomain(Ratio(_Number(1), x)).intervals match
      case Some(v) => assert(v.size == 2, s"1/x should give two intervals, got $v")
      case None    => fail("1/x should resolve")
  }

  "asin" should "require the closed unit interval" in
  {
    val d = realDomain(Asin(x))
    assert(reqs(d) == Set(Requirement.InClosedUnit))
    d.intervals match
      case Some(Vector(i)) => assert(i.lo == -1.0 && i.hi == 1.0 && i.loIncl && i.hiIncl)
      case other           => fail(s"expected [-1, 1], got $other")
  }

  "tan" should "record its exclusions but refuse to enumerate them" in
  {
    val d = realDomain(Tg(x))
    assert(reqs(d) == Set(Requirement.NotOddMultipleOfHalfPi))
    // Infinitely many excluded points cannot be a finite interval list.
    assert(d.intervals.isEmpty, "tan must not pretend to resolve")
  }

  "Gamma" should "record its poles and refuse to enumerate them" in
  {
    val d = realDomain(Gamma(x))
    assert(reqs(d) == Set(Requirement.NotNonPositiveInteger))
    assert(d.intervals.isEmpty)
  }

  "a fractional power" should "require a non-negative base over the reals" in
  {
    assert(reqs(realDomain(Power(x, _Number(0.5)))) == Set(Requirement.NonNegative))
  }

  "a negative integer power" should "require a non-zero base" in
  {
    assert(reqs(realDomain(Power(x, _Number(-2)))) == Set(Requirement.NonZero))
  }

  "nesting" should "collect from both levels" in
  {
    // ln(ln(x)) needs ln(x) > 0 AND x > 0.
    val d = realDomain(Ln(Ln(x)))
    assert(d.constraints.size == 2, s"expected two constraints, got ${d.constraints}")
    assert(reqs(d) == Set(Requirement.Positive))
  }

  "an unconstrained expression" should "report no requirements" in
  {
    val d = realDomain(Sum(Product(_Number(2), x), _Number(1)))
    assert(d.isUnrestricted)
    assert(d.intervals.contains(Vector(Interval.All)))
  }

  "a symbolic coefficient" should "keep the constraint but decline intervals" in
  {
    // ln(x - a) with `a` free: the constraint survives, the interval cannot be placed.
    val d = realDomain(Ln(Sum(x, Product(_Number(-1), a))))
    assert(reqs(d) == Set(Requirement.Positive))
    assert(d.intervals.isEmpty, "a free coefficient must not resolve to an interval")
  }

  "1/(x^2 - 1)" should "resolve to three intervals" in
  {
    val den = Sum(Power(x, _Number(2)), _Number(-1))
    realDomain(Ratio(_Number(1), den)).intervals match
      case Some(v) => assert(v.size == 3, s"expected three intervals, got $v")
      case None    => fail("should resolve")
  }

  // ── slice B: differentiability ─────────────────────────────────────────────

  "a step" should "be defined everywhere but differentiable nowhere at the step" in
  {
    assert(realDomain(_Heaviside(x)).isUnrestricted)
    val d = differentiableDomainOf(_Heaviside(x), x, DomainKind.Real, env)
    assert(d.constraints.exists(_.req == Requirement.Never))
    assert(d.isEmpty)
  }

  "ln" should "be differentiable exactly where it is defined" in
  {
    // The constraint set is a conjunction and is NOT minimised: derive(ln x) = 1/x adds
    // `NonZero`, which is implied by `Positive` but still recorded. The resolved interval
    // is the meaningful claim, so that is what this pins.
    val d = differentiableDomainOf(Ln(x), x, DomainKind.Real, env)
    assert(reqs(d).contains(Requirement.Positive))
    d.intervals match
      case Some(Vector(i)) => assert(i.lo == 0.0 && !i.loIncl && i.hi.isPosInfinity)
      case other           => fail(s"expected (0, inf), got $other")
  }

  "Gamma" should "be differentiable now that digamma exists" in
  {
    // 4.O made this differentiable; the only requirement is still its own poles.
    val d = differentiableDomainOf(Gamma(x), x, DomainKind.Real, env)
    assert(!d.constraints.exists(_.req == Requirement.Never),
           s"Gamma should be differentiable, got ${d.constraints}")
  }

  // ── slice C: the complex arm ───────────────────────────────────────────────

  "ln over the complex plane" should "need only a non-zero argument" in
  {
    assert(reqs(cplxDomain(Ln(x))) == Set(Requirement.NonZero))
  }

  "a fractional power over the complex plane" should "need nothing" in
  {
    assert(cplxDomain(Power(x, _Number(0.5))).isUnrestricted)
  }

  "asin over the complex plane" should "report an EMPTY domain, not the analytic one" in
  {
    // The Asin convention: the library leaves these symbolic on complex input, so the
    // honest report is that it computes nothing -- not the mathematical extension.
    val d = cplxDomain(Asin(x))
    assert(reqs(d) == Set(Requirement.Never))
    assert(d.isEmpty)
  }

  "atan over the complex plane" should "also be empty" in
  {
    assert(cplxDomain(Atan(x)).isEmpty)
  }

  "atan over the reals" should "be unrestricted" in
  {
    assert(realDomain(Atan(x)).isUnrestricted)
  }
