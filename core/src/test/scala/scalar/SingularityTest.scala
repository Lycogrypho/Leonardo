package it.grypho.scala.leonardo
package scalar

import core.*
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.3 slice D — locating and classifying singularities.
 *
 *  This is what 6.14 (Laurent) needs: a pole's *order* is the length of the principal part,
 *  and a removable singularity has none at all.
 */
class SingularityTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")
  private val a   = _Variable("a")

  /** `x - c` as a polynomial factor. */
  private def linear(c: Double): _Expression = Sum(x, _Number(-c))

  private def sing(e: _Expression): Option[Vector[Singularity]] = singularitiesOf(e, x, env)

  "a removable point beside a pole" should "be told apart from it at any scale (F_0125)" in
  {
    // (x − 1e-7)/((x − 1e-7)(x − 3e-7)): the numerator root matched BOTH denominator roots under
    // an absolute 1e-6, and the pole at 3e-7 was reported with order 0.
    sing(Ratio(linear(1e-7), Product(linear(1e-7), linear(3e-7)))) match
      case Some(Vector(Singularity(_, SingularityKind.Removable), Singularity(at, SingularityKind.Pole(1)))) =>
        assert(math.abs(at - 3e-7) <= 1e-15)
      case other => fail(s"$other")
  }

  "1/(x-1)" should "be a simple pole at 1" in
  {
    sing(Ratio(_Number(1), linear(1))) match
      case Some(Vector(Singularity(at, SingularityKind.Pole(1)))) =>
        assert(math.abs(at - 1.0) < 1e-6, s"pole at $at")
      case other => fail(s"expected one simple pole at 1, got $other")
  }

  "1/((x - 1e-6)(x - 2e-6))" should "be two simple poles, not one double pole between them (F_0118)" in
  {
    sing(Ratio(_Number(1), Product(linear(1e-6), linear(2e-6)))) match
      case Some(Vector(Singularity(p, SingularityKind.Pole(1)), Singularity(q, SingularityKind.Pole(1)))) =>
        assert(math.abs(p - 1e-6) <= 1e-15 && math.abs(q - 2e-6) <= 1e-15, s"poles at $p, $q")
      case other => fail(s"expected simple poles at 1e-6 and 2e-6, got $other")
  }

  "1/(1e-12x² + x + 1)" should "keep its tiny leading coefficient: two simple poles, -1 and about -1e12 (F_0119)" in
  {
    // The x² coefficient is genuine; read as zero, the pole near -1e12 vanished.
    sing(Ratio(_Number(1), parser.Parser.parse("1e-12*x^2 + x + 1").get)) match
      case Some(Vector(Singularity(p, SingularityKind.Pole(1)), Singularity(q, SingularityKind.Pole(1)))) =>
        // Roots of 1e-12x² + x + 1: -(1e12 - 1) and 1/(1e-12·-(1e12 - 1)) ≈ -1 - 1e-12.
        assert(math.abs(q + 1) <= 1e-9, s"near pole $q")
        assert(math.abs(p / -999999999999.0 - 1) <= 1e-9, s"far pole $p")
      case other => fail(s"expected two simple poles, got $other")
  }

  it should "not invent a pole from a top coefficient that is rounding residue (F_0119)" in
  {
    // (0.1x + 0.2)·0.3x − 0.03x² is 0.06x exactly; in floating point the x² coefficient is
    // 0.1·0.3 − 0.03 ≈ 3.5e-18. Kept, it would put a pole near -1.7e16.
    sing(Ratio(_Number(1), parser.Parser.parse("(0.1*x + 0.2)*(0.3*x) - 0.03*x^2").get)) match
      case Some(Vector(Singularity(at, SingularityKind.Pole(1)))) => assert(math.abs(at) <= 1e-12, s"pole at $at")
      case other => fail(s"expected one simple pole at 0, got $other")
  }

  "1/(x-1)^2" should "be a pole of order 2" in
  {
    sing(Ratio(_Number(1), Power(linear(1), _Number(2)))) match
      case Some(Vector(Singularity(at, SingularityKind.Pole(2)))) =>
        assert(math.abs(at - 1.0) < 1e-6)
      case other => fail(s"expected a pole of order 2, got $other")
  }

  "(x-1)/(x-1)" should "be removable, not a pole" in
  {
    // Saying "pole of order 1" here would send a Laurent expansion looking for a principal
    // part that does not exist.
    sing(Ratio(linear(1), linear(1))) match
      case Some(Vector(Singularity(_, SingularityKind.Removable))) => succeed
      case other => fail(s"expected a removable singularity, got $other")
  }

  "(x-1)/(x-1)^3" should "drop the order by the numerator's multiplicity" in
  {
    sing(Ratio(linear(1), Power(linear(1), _Number(3)))) match
      case Some(Vector(Singularity(_, SingularityKind.Pole(2)))) => succeed
      case other => fail(s"expected a pole of order 2, got $other")
  }

  // ── multiplicity by square-free factorisation, not clustering (issue 2.5) ────
  // The QR iteration does not merely SCATTER a repeated root — it can fail to converge on
  // one outright, so a pure high-multiplicity denominator yielded no classification at all.
  // squareFreeFactors reads the multiplicity arithmetically, before any root-finding.

  "1/(x-1)^3" should "be a pole of order 3" in
  {
    sing(Ratio(_Number(1), Power(linear(1), _Number(3)))) match
      case Some(Vector(Singularity(at, SingularityKind.Pole(3)))) =>
        assert(math.abs(at - 1.0) < 1e-9, s"pole at $at")
      case other => fail(s"expected a pole of order 3, got $other")
  }

  "1/(x-2)^5" should "be a pole of order 5, located exactly" in
  {
    sing(Ratio(_Number(1), Power(linear(2), _Number(5)))) match
      case Some(Vector(Singularity(at, SingularityKind.Pole(5)))) =>
        assert(math.abs(at - 2.0) < 1e-9, s"pole at $at")
      case other => fail(s"expected a pole of order 5, got $other")
  }

  "1/((x-1)^3*(x+2))" should "give both poles with their own orders" in
  {
    sing(Ratio(_Number(1), Product(Power(linear(1), _Number(3)), linear(-2)))) match
      case Some(Vector(Singularity(a1, SingularityKind.Pole(o1)),
                       Singularity(a2, SingularityKind.Pole(o2)))) =>
        assert(math.abs(a1 + 2.0) < 1e-6 && o1 == 1, s"first pole $a1 order $o1")
        assert(math.abs(a2 - 1.0) < 1e-6 && o2 == 3, s"second pole $a2 order $o2")
      case other => fail(s"expected poles at -2 (order 1) and 1 (order 3), got $other")
  }

  "1/((x-1)(x+2))" should "find both poles in ascending order" in
  {
    val den = Product(linear(1), linear(-2))
    sing(Ratio(_Number(1), den)) match
      case Some(v) =>
        assert(v.size == 2, s"expected two poles, got $v")
        assert(math.abs(v(0).at + 2.0) < 1e-6, s"first should be -2, got ${v(0).at}")
        assert(math.abs(v(1).at - 1.0) < 1e-6, s"second should be 1, got ${v(1).at}")
      case None => fail("should resolve")
  }

  "a polynomial" should "have no singularities at all" in
  {
    assert(sing(Sum(Power(x, _Number(2)), _Number(1))).contains(Vector.empty))
  }

  "a symbolic coefficient" should "decline rather than guess" in
  {
    assert(sing(Ratio(_Number(1), Sum(x, a))).isEmpty,
           "a free coefficient cannot place a pole")
  }

  "a transcendental" should "decline rather than report a partial list" in
  {
    // tan has infinitely many poles; an enumerated prefix would read as exhaustive.
    assert(sing(Tg(x)).isEmpty)
    assert(sing(Ratio(_Number(1), Sin(x))).isEmpty)
  }
