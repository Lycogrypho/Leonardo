package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec
import spire.math.Rational as Q


/** F_0176 — `jordan` of an exact matrix whose characteristic polynomial splits over the rationals.
 *
 *  Every answer is checked as the kernel certifies it: `A P = P J` exactly, `P` invertible, and
 *  `J` in Jordan form — eigenvalues on the diagonal, ascending, blocks by decreasing size, `1`
 *  above the diagonal inside a block and `0` everywhere else.
 */
class JordanTest extends AnyFlatSpec:

  private val env = new Environment()

  private type M = Vector[Vector[Q]]

  private def parse(s: String, exact: Boolean = true): _Expression =
    Parser.parse(s, if exact then Some(30) else None) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$s': $other")

  private def exactMatrix(e: _Expression): M = e match
    case m: _Matrix =>
      Vector.tabulate(m.rows, m.cols)((i, j) => m(i, j) match
        case r: _Rational => Q(r.num, r.den)
        case other        => fail(s"cell ($i, $j) is not exact: $other"))
    case other => fail(s"not an exact matrix: $other")

  private def mul(a: M, b: M): M =
    Vector.tabulate(a.size, b.head.size)((i, j) => a(i).indices.foldLeft(Q.zero)((s, k) => s + a(i)(k) * b(k)(j)))

  private def det(a: M): Q =
    if a.size == 1 then a(0)(0)
    else a.head.indices.foldLeft(Q.zero) { (s, j) =>
      val minor = a.tail.map(r => r.patch(j, Nil, 1))
      s + (if j % 2 == 0 then Q.one else -Q.one) * a(0)(j) * det(minor)
    }

  /** `jordan(a)` in exact mode, checked; returns `J`. */
  private def jordanOf(a: String): M =
    val am = exactMatrix(parse(a).eval(env).fold(identity, identity))
    parse(s"jordan($a)").eval(env) match
      case Left(row: _Matrix) if row.rows == 1 && row.cols == 2 =>
        val (p, j) = (exactMatrix(row(0, 0)), exactMatrix(row(0, 1)))
        assert(mul(am, p) == mul(p, j), s"A P != P J for $a")
        assert(!det(p).isZero, s"P is singular for $a")
        val n = j.size
        for r <- 0 until n; c <- 0 until n if c != r && c != r + 1 do
          assert(j(r)(c).isZero, s"J($r, $c) is not zero for $a")
        for r <- 0 until n - 1 do
          val sup = j(r)(r + 1)
          assert(sup.isZero || (sup == Q.one && j(r)(r) == j(r + 1)(r + 1)), s"J($r, ${r + 1}) = $sup for $a")
          assert(j(r)(r) <= j(r + 1)(r + 1), s"eigenvalues not ascending for $a")
        j
      case other => fail(s"jordan($a) should answer exactly, got $other")

  private def m(rows: Vector[Int]*): M = rows.toVector.map(_.map(Q(_)))

  "jordan of an exact defective matrix" should "return its Jordan block" in {
    assert(jordanOf("[[0, 4], [-1, 4]]") == m(Vector(2, 1), Vector(0, 2)))
    assert(jordanOf("[[4, 2], [0, 4]]") == m(Vector(4, 1), Vector(0, 4)))
  }

  it should "return a Jordan block as itself" in {
    assert(jordanOf("[[5, 1, 0], [0, 5, 1], [0, 0, 5]]") == m(Vector(5, 1, 0), Vector(0, 5, 1), Vector(0, 0, 5)))
  }

  it should "find blocks of sizes 2 and 1 for one eigenvalue" in {
    // P J P^-1 with J = [[3, 1, 0], [0, 3, 0], [0, 0, 3]] and P = [[1, 1, 0], [0, 1, 1], [1, 0, 1]].
    val j = jordanOf("[[7/2, 1/2, -1/2], [0, 3, 0], [1/2, 1/2, 5/2]]")
    assert(j == m(Vector(3, 1, 0), Vector(0, 3, 0), Vector(0, 0, 3)))
  }

  it should "find two blocks of size 2 for one eigenvalue" in {
    val j = jordanOf("[[2, 1, 0, 0], [0, 2, 0, 0], [0, 0, 2, 1], [0, 0, 0, 2]]")
    assert(j == m(Vector(2, 1, 0, 0), Vector(0, 2, 0, 0), Vector(0, 0, 2, 1), Vector(0, 0, 0, 2)))
  }

  it should "order mixed eigenvalues ascending, blocks by decreasing size" in {
    val j = jordanOf("[[5, 0, 0], [0, 2, 1], [0, 0, 2]]")
    assert(j == m(Vector(2, 1, 0), Vector(0, 2, 0), Vector(0, 0, 5)))
  }

  it should "handle a nilpotent matrix" in {
    assert(jordanOf("[[0, 1], [0, 0]]") == m(Vector(0, 1), Vector(0, 0)))
    assert(jordanOf("[[0, 0], [0, 0]]") == m(Vector(0, 0), Vector(0, 0)))
  }

  "jordan of an exact diagonalisable matrix" should "be exact and diagonal" in {
    assert(jordanOf("[[1, 2], [2, 1]]") == m(Vector(-1, 0), Vector(0, 3)))
    assert(jordanOf("[[1/2, 0], [0, 1/3]]") == Vector(Vector(Q(1, 3), Q.zero), Vector(Q.zero, Q(1, 2))))
  }

  "jordan outside the exact rational case" should "keep today's behaviour" in {
    // irrational eigenvalues: the numeric diagonal case still answers
    parse("jordan([[1, 1], [1, 0]])").eval(env) match
      case Left(row: _Matrix) => assert(row.rows == 1 && row.cols == 2)
      case other              => fail(s"$other")
    // a numeric defective matrix stays unevaluated (F_0174)
    val numeric = parse("jordan([[0, 4], [-1, 4]])", exact = false)
    assert(numeric.eval(env) == Left(numeric))
  }
