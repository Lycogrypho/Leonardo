package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0160, F_0161, F_0162 — `dot`, `cross` and `trace`, the course LA prerequisites.
 *
 *  Each is checked on the three carriers a matrix literal arrives as — dense, exact and symbolic —
 *  and on its refusals: a shape it does not define stays unevaluated as the call written.
 *  `dot` takes no conjugate (F_0160 Decision 1), so a complex vector is refused rather than
 *  guessed; `cross` is the right-handed product and keeps its first argument's shape.
 */
class VectorProductsTest extends AnyFlatSpec:

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

  private def dense(s: String, e: Environment = env): (Int, Int, Vector[Double]) = parse(s).eval(e) match
    case Right(m: _MatrixValue) => (m.rows, m.cols, m.toVector)
    case other                  => fail(s"\"$s\" gave no dense matrix: $other")

  /** Stays unevaluated, printed as the call it was written as. */
  private def declines(s: String, e: Environment = env): Unit =
    val name = s.takeWhile(_ != '(')
    parse(s).eval(e) match
      case Left(r) if r.toString.startsWith(s"$name(") => ()
      case other => fail(s"\"$s\" should stay unevaluated, got $other")

  private def bound(pairs: (String, Double)*): Environment =
    pairs.foldLeft(env)((en, p) => en.withBinding(p._1, _Number(p._2)))

  // --- trace (F_0162) ---

  "trace" should "sum a square matrix's diagonal" in {
    assert(number("trace([[1, 2], [3, 4]])") == 5.0)
    assert(number("trace(eye(4))") == 4.0)
  }

  it should "stay exact on exact cells" in {
    assert(exactValue("trace([[1/2, 7], [9, 1/3]])") == _Rational.of(BigInt(5), BigInt(6)).getOrElse(fail("5/6")))
  }

  it should "sum symbolic cells symbolically" in {
    val e = parse("trace([[a, 1], [2, b]])").eval(env).fold(identity, identity)
    assert(!e.toString.startsWith("trace("), e)
    assert(e.eval(bound("a" -> 1, "b" -> 2)) == Right(_Number(3)))
  }

  it should "compose as a scalar, not a 1x1 matrix" in {
    assert(number("2 + trace([[1, 0], [0, 1]])") == 4.0)
  }

  it should "read a bound matrix and decline a non-square one" in {
    val a = env.withBinding("A", _MatrixValue(2, 2, Array(1.0, 2.0, 3.0, 5.0)))
    assert(number("trace(A)", a) == 6.0)
    declines("trace([[1, 2, 3], [4, 5, 6]])")
    declines("trace(5)")
  }

  it should "equal the sum of the eigenvalues" in {
    val m = "[[2, 1, 0], [1, 3, 1], [0, 1, 4]]"
    val lambdas = parse(s"eigen($m)").eval(env) match
      case Left(r: _Matrix) => r.elems.collect { case _Number(d) => d }
      case other            => fail(s"$other")
    assert(math.abs(lambdas.sum - number(s"trace($m)")) < 1e-9)
  }

  // --- dot (F_0160) ---

  "dot" should "answer the scalar sum of products of two columns, rows or one of each" in {
    assert(number("dot([[1], [2], [3]], [[4], [5], [6]])") == 32.0)
    assert(number("dot([[1, 2, 3]], [[4, 5, 6]])") == 32.0)
    assert(number("dot([[1, 2, 3]], [[4], [5], [6]])") == 32.0)
  }

  it should "compose as a scalar" in {
    assert(number("2 + dot([[1], [2]], [[3], [4]])") == 13.0)
    assert(number("dot([[3], [4]], [[3], [4]])^0.5") == 5.0)
  }

  it should "stay exact on exact cells" in {
    assert(exactValue("dot([[1/2], [1/3]], [[2], [3]])") == _Rational(2))
  }

  it should "build a sum of products on symbolic cells" in {
    val e = parse("dot([[x], [y]], [[1], [2]])").eval(env).fold(identity, identity)
    assert(!e.toString.startsWith("dot("), e)
    assert(e.eval(bound("x" -> 1, "y" -> 1)) == Right(_Number(3)))
  }

  it should "decline a length mismatch, a non-vector and a complex vector" in {
    declines("dot([[1], [2]], [[1], [2], [3]])")
    declines("dot([[1, 2], [3, 4]], [[1, 2], [3, 4]])")
    declines("dot([[1 + 2i], [1]], [[1], [1]])")
  }

  it should "refuse a symbolic sum deeper than the expression-depth limit" in {
    val n = MaxExpressionDepth + 5
    val u = (1 to n).map(k => s"[x$k]").mkString("[", ", ", "]")
    val v = (1 to n).map(_ => "[1]").mkString("[", ", ", "]")
    declines(s"dot($u, $v)")
  }

  // --- cross (F_0161) ---

  "cross" should "be the right-handed product" in {
    assert(dense("cross([[1], [0], [0]], [[0], [1], [0]])") == (3, 1, Vector(0.0, 0.0, 1.0)))
    assert(dense("cross([[0], [1], [0]], [[0], [0], [1]])") == (3, 1, Vector(1.0, 0.0, 0.0)))
    assert(dense("cross([[0], [0], [1]], [[1], [0], [0]])") == (3, 1, Vector(0.0, 1.0, 0.0)))
    assert(dense("cross([[0], [1], [0]], [[1], [0], [0]])") == (3, 1, Vector(0.0, 0.0, -1.0)))
  }

  it should "keep its first argument's shape" in {
    assert(dense("cross([[1, 0, 0]], [[0], [1], [0]])") == (1, 3, Vector(0.0, 0.0, 1.0)))
  }

  it should "be orthogonal to both arguments" in {
    val (u, v) = ("[[1], [-2], [5]]", "[[3], [7], [-1]]")
    assert(number(s"dot($u, cross($u, $v))") == 0.0)
    assert(number(s"dot($v, cross($u, $v))") == 0.0)
  }

  it should "stay exact on exact cells and work on symbolic ones" in {
    parse("cross([[1/2], [0], [0]], [[0], [1/3], [0]])", Some(30)).eval(env) match
      case Left(m: _Matrix) => assert(m.elems == Vector(_Rational(0), _Rational(0), _Rational.of(BigInt(1), BigInt(6)).getOrElse(fail("1/6"))))
      case other            => fail(s"$other")
    val c = parse("cross([[a], [b], [c]], [[1], [2], [3]])").eval(env).fold(identity, identity)
    assert(c.eval(bound("a" -> 1, "b" -> 0, "c" -> 0)) == Right(_MatrixValue(3, 1, Array(0.0, -3.0, 2.0))))
  }

  it should "decline anything but two 3-vectors" in {
    declines("cross([[1], [2]], [[3], [4]])")
    declines("cross([[1, 2, 3], [4, 5, 6]], [[1], [2], [3]])")
  }

  it should "scale as a matrix" in {
    assert(dense("2 * cross([[1], [0], [0]], [[0], [1], [0]])") == (3, 1, Vector(0.0, 0.0, 2.0)))
  }
