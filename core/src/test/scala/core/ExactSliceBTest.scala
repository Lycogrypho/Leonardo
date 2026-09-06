package it.grypho.scala.leonardo
package core

import parser.Parser
import matrix.*
import scalar.{Sum, Product, simplifyFully, MaxExactFactorial}
import org.scalatest.flatspec.AnyFlatSpec


/** Exact matrices and the unbounded factorial family — issue 4.L slice B.
 *
 *  Slice A made scalar arithmetic exact; this covers the two places that still computed in
 *  `Double` afterwards.  The matrix half rests on one design choice worth restating: the
 *  **symbolic `_Matrix` is the exact carrier**.  `_MatrixValue` holds a private
 *  `Array[Double]` and must keep it, so an exactly-written matrix simply does not collapse
 *  into it — which means a finished exact matrix is a `Left(_Matrix)` of concrete cells, not
 *  a `Right`.  Several assertions below exist to pin exactly that.
 */
class ExactSliceBTest extends AnyFlatSpec:

  private val Digits = 30
  private val env    = new Environment()

  private def exact(input: String): Either[_Expression, _Value] =
    Parser.parse(input, Some(Digits)) match
      case Parser.Success(e, _) => e.eval(env)
      case other                => fail(s"parse failed for \"$input\": $other")

  private def plain(input: String): Either[_Expression, _Value] =
    Parser.parse(input, None) match
      case Parser.Success(e, _) => e.eval(env)
      case other                => fail(s"parse failed for \"$input\": $other")

  /** The exact scalar result of `input`, failing if it did not stay exact. */
  private def rational(input: String): _Rational = exact(input) match
    case Right(r: _Rational) => r
    case other               => fail(s"\"$input\" did not stay exact: $other")

  /** The exact cells of a matrix result, failing if it is not an exact matrix. */
  private def cells(input: String): Vector[_Rational] = exact(input) match
    case Left(m: _Matrix) => exactCells(m).getOrElse(fail(s"\"$input\" has inexact cells: $m"))
    case other            => fail(s"\"$input\" is not an exact matrix: $other")

  private def r(n: Int, d: Int): _Rational =
    _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail(s"$n/$d has a zero denominator"))

  private def ints(ns: Int*): Vector[_Rational] = ns.toVector.map(_Rational(_))

  // --- simplify no longer disagrees with eval about exactness ---

  "simplify" should "keep exact constants exact" in
  {
    // Before slice B this folded to the Double 0.66667, while `eval` gave 2/3 on the same
    // input -- two commands disagreeing about the same expression.
    val third = r(1, 3)
    assert(simplifyFully(Sum(third, third)) == r(2, 3))
    assert(simplifyFully(Product(r(2, 6), _Rational(3))) == _Rational.One)
  }

  it should "still fold inexact constants exactly as before" in
  {
    assert(simplifyFully(Sum(_Number(1.0), _Number(2.0))) == _Number(3.0))
  }

  // --- the factorial family loses its Double ceiling ---

  "the exact factorial" should "compute past 170!, where a Double overflows" in
  {
    // 170! is the largest a Double can hold at all; 171! is the first that could not exist
    // before slice B.  The exact value ends in a long run of zeros (trailing factors of 10).
    assert(plain("fact(171)").isLeft, "the Double path still gives up at 171!")
    val f171 = rational("fact(171)")
    assert(f171.toBigIntExact.isDefined, "must be an exact integer")
    assert(f171.toBigIntExact.exists(_.toString.length == 310), s"171! has 310 digits, got $f171")
  }

  it should "stay exact for the double factorial and the general step" in
  {
    assert(rational("fact(5)") == _Rational(120))
    assert(rational("dfact(7)") == _Rational(105))
    assert(rational("mfact(20, 3)") == _Rational(4188800))
    // dfact is parser sugar for mfact(n, 2); its synthesised 2 must be built in the exact
    // tier too, or the pair mixes tiers and silently drops back to Double.
    assert(rational("dfact(200)") == rational("mfact(200, 2)"))
    assert(rational("dfact(200)").toBigIntExact.isDefined, "must be exact, not 1.18e188")
  }

  it should "cover Gamma at a positive integer, where it is a factorial" in
  {
    assert(rational("Gamma(5)") == _Rational(24))
    assert(rational("Gamma(1)") == _Rational.One)
    // Everywhere else Gamma is genuinely transcendental and stays approximate.
    assert(math.abs(rational("Gamma(0.5)").toDouble - math.sqrt(math.Pi)) < 1e-9)
  }

  it should "give up past the compute cap rather than hang" in
  {
    // A different kind of limit from 170!: not representability but time.  10000! is a
    // 35660-digit integer computed in milliseconds; a few orders of magnitude more would
    // wedge the REPL, and an unbounded fact is exactly what a user types by accident.
    assert(rational(s"fact(${MaxExactFactorial})").toBigIntExact.isDefined, "the cap itself must work")
    assert(exact(s"fact(${MaxExactFactorial + 1})").isLeft, "past the cap it stays symbolic")
  }

  it should "reject a negative or non-integer argument to the exact path" in
  {
    // Those are the analytic cases; they must keep going through the Lanczos kernels.
    assert(math.abs(rational("Gamma(2.5)").toDouble - 1.3293403881791370) < 1e-9)
    assert(exact("fact(-1)").isLeft)
  }

  // --- exact matrices ---

  "an exact matrix" should "not collapse into the dense Double carrier" in
  {
    // The central design point of slice B.  _MatrixValue is Array[Double], so collapsing
    // would throw the exactness away at the literal, before any operation could use it.
    assert(cells("[[1/2, 1/3], [1/4, 1/5]]") == Vector(r(1, 2), r(1, 3), r(1, 4), r(1, 5)))
    // ...while an inexact matrix still collapses exactly as it always did.
    assert(plain("[[1/2, 1/3], [1/4, 1/5]]").isRight)
  }

  it should "add, scale and transpose element-wise without demoting" in
  {
    assert(cells("[[1/2, 1/3], [1/4, 1/5]] + [[1/2, 1/3], [1/4, 1/5]]") ==
             Vector(_Rational.One, r(2, 3), r(1, 2), r(2, 5)))
    assert(cells("2 * [[1/2, 1/3], [1/4, 1/5]]") ==
             Vector(_Rational.One, r(2, 3), r(1, 2), r(2, 5)))
    assert(cells("transpose([[1/2, 1/3], [1/4, 1/5]])") ==
             Vector(r(1, 2), r(1, 4), r(1, 3), r(1, 5)))
  }

  "the exact determinant" should "be right where the Double one rounds" in
  {
    assert(rational("det([[1/2, 1/3], [1/4, 1/5]])") == r(1, 60))
    // The 3x3 Hilbert determinant is exactly 1/2160; the Double path reports 4.6E-4.
    assert(rational("det([[1, 1/2, 1/3], [1/2, 1/3, 1/4], [1/3, 1/4, 1/5]])") == r(1, 2160))
    assert(rational("det([[1, 2], [3, 4]])") == _Rational(-2))
  }

  it should "return an exact zero for a singular matrix, not give up" in
  {
    assert(rational("det([[1, 1], [1, 1]])") == _Rational.Zero)
  }

  it should "carry no dimension cap, unlike the cofactor expansion" in
  {
    // symbolicDet is O(n!) and capped at MaxSymbolicDim = 6.  Gaussian elimination over an
    // exact field is O(n^3), so this path is uncapped -- a 7x7 would be refused by the old
    // route.  The determinant of a 7x7 Hilbert matrix is tiny but exact.
    val h7 = (0 until 7).map(i => (0 until 7).map(j => s"1/${i + j + 1}").mkString("[", ", ", "]"))
               .mkString("[", ", ", "]")
    val d = rational(s"det($h7)")
    assert(!d.isZero, "the Hilbert determinant is small but never zero")
    assert(d.den > BigInt(10).pow(20), s"expected an exactly tiny value, got $d")
  }

  "the exact inverse" should "satisfy A * inv(A) = I exactly" in
  {
    // The acceptance criterion for the whole tier, and the case where Double arithmetic is
    // least trustworthy: the Hilbert matrix is the standard ill-conditioned example.
    val h3 = "[[1, 1/2, 1/3], [1/2, 1/3, 1/4], [1/3, 1/4, 1/5]]"
    assert(cells(s"inv($h3)") == ints(9, -36, 30, -36, 192, -180, 30, -180, 180))
    assert(cells(s"$h3 * inv($h3)") == ints(1, 0, 0, 0, 1, 0, 0, 0, 1))
  }

  it should "stay symbolic for a singular matrix, like the dense kernel" in
  {
    assert(exact("inv([[1, 1], [1, 1]])").isLeft)
    exact("inv([[1, 1], [1, 1]])") match
      case Left(_: Inverse) => ()   // unreduced, not a wrong answer
      case other            => fail(s"expected the Inverse node back, got $other")
  }

  // --- what deliberately still computes in Double ---

  "the iterative decompositions" should "keep working by demoting an exact operand" in
  {
    // lu / qr / eigen / eig / jordan are Double algorithms and cannot be exact whatever
    // their input.  Since an exact matrix no longer collapses, they must demote explicitly
    // or they would silently stop working in exact mode.
    // Asserting equality with the Double path is the sharper statement: demoting means
    // these must produce the *identical* result in both modes, not merely some result.
    for input <- List("lu([[4, 3], [6, 3]])", "qr([[1, 2], [3, 4]])",
                      "eigen([[4, 1], [1, 3]])", "eig([[3, 1], [1, 3]])",
                      "jordan([[3, 1], [1, 3]])") do
      assert(exact(input) == plain(input), s"$input differs between modes")
      assert(exact(input).isLeft, s"$input returns a matrix-of-matrices, which is a Left")
  }

  "matrix power" should "demote rather than be lost, with A * A as the exact form" in
  {
    // `Power` lives in `scalar`, which cannot build a matrix product, so it cannot do exact
    // repeated multiplication.  Demoting keeps A^n working; A * A stays exact.
    assert(exact("[[1, 2], [3, 4]]^2").isRight, "A^n must still evaluate")
    assert(cells("[[1, 2], [3, 4]] * [[1, 2], [3, 4]]") == ints(7, 10, 15, 22))
  }

  // --- the parity guard, restated for the matrix tier ---

  "exact mode" should "still lose nothing across the matrix surface" in
  {
    val corpus = List(
      "[[1, 2], [3, 4]]", "det([[1, 2], [3, 4]])", "inv([[1, 2], [3, 4]])",
      "transpose([[1, 2], [3, 4]])", "eye(3)", "zeros(2, 3)",
      "[[1, 2], [3, 4]] * [[1, 0], [0, 1]]", "[[1, 2], [3, 4]] + [[1, 2], [3, 4]]",
      "[[1, 2], [3, 4]]^2", "at([[1, 2], [3, 4]], 1, 2)", "2 * [[1, 2], [3, 4]]",
      "[[1, 2], [3, 4]] / 2", "det(eye(4))"
    )
    for input <- corpus do
      if plain(input).isRight then
        val e = exact(input)
        val ok = e.isRight || (e match
          case Left(m: _Matrix) => m.elems.forall(_.isInstanceOf[_Value])
          case _                => false)
        assert(ok, s"""exact mode lost "$input": plain=${plain(input)} exact=$e""")
  }
