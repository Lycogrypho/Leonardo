package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0039 — `norm(x)` and `norm(x, kind)`.
 *
 *  **The shape decides the reading** (Decision A): a row or a column is a vector, anything else a
 *  matrix, and the default is the 2-norm either way — the vector 2-norm and the spectral norm
 *  agree on a column.  The kinds are `1`, `2`, `inf`, `fro`, and for a vector any `p ≥ 1`
 *  (Decision B).  The matrix 2-norm is the largest singular value through the SVD, never
 *  `eigen(AᵀA)`, and a vector 2-norm in `Double` is scaled by its largest modulus, so neither
 *  overflows on input whose norm is representable.
 */
class NormTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def number(s: String, e: Environment = env): Double = parse(s).eval(e) match
    case Right(_Number(d)) => d
    case other             => fail(s"\"$s\" gave no number: $other")

  private def exactValue(s: String): _Rational = parse(s, Some(30)).eval(env) match
    case Right(r: _Rational) => r
    case other               => fail(s"\"$s\" gave no exact value: $other")

  private def close(a: Double, b: Double): Boolean = math.abs(a - b) <= 1e-12 * math.max(1.0, math.abs(b))

  /** Stays unevaluated, printed as the call it was written as. */
  private def declines(s: String, e: Environment = env): Unit =
    parse(s).eval(e) match
      case Left(r) if r.toString.startsWith("norm(") => ()
      case other => fail(s"\"$s\" should stay unevaluated, got $other")

  private def rat(n: Int, d: Int): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail(s"$n/$d"))

  // --- vectors ---

  "norm" should "take a vector's 2-norm by default, for a row or a column" in {
    assert(number("norm([[3], [4]])") == 5.0)
    assert(number("norm([[3, 4]])") == 5.0)
    assert(number("norm([[3], [4]], 2)") == 5.0)
  }

  it should "take a vector's 1-, inf- and p-norms" in {
    assert(number("norm([[1], [-2], [3]], 1)") == 6.0)
    assert(number("norm([[1], [-2], [3]], inf)") == 3.0)
    assert(close(number("norm([[3], [4]], 3)"), math.cbrt(91.0)))
    assert(number("norm([[3], [4]], fro)") == 5.0)
  }

  it should "decline a p below 1, which is not a norm, and read p from a binding" in {
    declines("norm([[1], [2]], 0.5)")
    assert(number("norm([[1], [-2]], k)", env.withBinding("k", _Number(1))) == 3.0)
  }

  it should "read a complex entry by its modulus" in {
    assert(number("norm([[3i], [4]])") == 5.0)
    assert(number("norm([[3i], [-4]], 1)") == 7.0)
  }

  it should "not overflow or underflow where the norm itself is representable" in {
    assert(close(number("norm([[1e200], [1e200]])"), math.sqrt(2) * 1e200))
    assert(close(number("norm([[3e-200], [4e-200]])"), 5e-200))
  }

  it should "stay exact on exact cells" in {
    assert(exactValue("norm([[1/2], [-1/3]], 1)") == rat(5, 6))
    assert(exactValue("norm([[1/2], [-1/3]], inf)") == rat(1, 2))
    assert(exactValue("norm([[3/5], [4/5]])") == _Rational(1))
    assert(math.abs(exactValue("norm([[1], [1]])").toDouble - math.sqrt(2)) < 1e-15)
  }

  it should "build the norm of symbolic cells, except the max-norm" in {
    val two = parse("norm([[a], [b]])").eval(env).fold(identity, identity)
    assert(!two.toString.startsWith("norm("), two)
    assert(two.eval(env.withBinding("a", _Number(3)).withBinding("b", _Number(-4))) == Right(_Number(5)))
    val one = parse("norm([[a], [b]], 1)").eval(env).fold(identity, identity)
    assert(one.eval(env.withBinding("a", _Number(3)).withBinding("b", _Number(-4))) == Right(_Number(7)))
    declines("norm([[a], [b]], inf)")
  }

  it should "read a number as a 1x1 vector, and compose as a scalar" in {
    assert(number("norm(-5)") == 5.0)
    assert(number("1 + norm([[3], [4]])") == 6.0)
  }

  // --- matrices ---

  it should "take a matrix's 1-, inf- and Frobenius norms" in {
    val a = "[[1, -2], [-3, 4]]"
    assert(number(s"norm($a, 1)") == 6.0)
    assert(number(s"norm($a, inf)") == 7.0)
    assert(close(number(s"norm($a, fro)"), math.sqrt(30)))
  }

  it should "take the spectral norm by default, the largest singular value" in {
    val a = "[[1, -2], [-3, 4]]"
    val sigma = math.sqrt((30 + math.sqrt(884)) / 2)    // the larger eigenvalue of AᵀA, rooted
    assert(close(number(s"norm($a)"), sigma))
    assert(number(s"norm($a, 2)") == number(s"norm($a)"))
  }

  it should "scale with the matrix (the units rule)" in {
    val a = "[[1, -2], [-3, 4]]"
    for k <- Seq(1e-150, 1e150) do
      assert(close(number(s"norm($k * $a)"), k * number(s"norm($a)")), k)
  }

  it should "stay exact for the 1- and inf-norms of an exact matrix, and demote the 2-norm" in {
    assert(exactValue("norm([[1/2, 1], [1/3, 1]], 1)") == _Rational(2))
    assert(exactValue("norm([[1/2, 1], [1/3, 1]], inf)") == rat(3, 2))
    parse("norm([[1/2, 1], [1/3, 1]])", Some(30)).eval(env) match
      case Right(_: _Rational) => fail("the spectral norm is not exact")
      case Right(_Number(_))   => succeed
      case other               => fail(s"$other")
  }

  it should "decline a matrix p-norm other than 1, 2 or inf, and a symbolic matrix's operator norms" in {
    declines("norm([[1, 2], [3, 4]], 3)")
    declines("norm([[a, 2], [3, 4]], 1)")
    declines("norm([[a, 2], [3, 4]])")
  }

  it should "read a bound matrix, and stay symbolic on a free name" in {
    val bound = env.withBinding("A", _MatrixValue(2, 2, Array(3.0, 0.0, 0.0, 4.0)))
    assert(number("norm(A, 1)", bound) == 4.0)
    parse("norm(A, fro)").eval(env) match
      case Left(e) => assert(e.toString == "norm(A, fro)")
      case other   => fail(s"$other")
  }

  it should "print as written, so a saved session re-parses it" in {
    for s <- Seq("norm(x)", "norm(x, fro)", "norm(x, 1.0)") do
      assert(parse(s).toString == s)
      assert(parse(parse(s).toString) == parse(s))
  }
