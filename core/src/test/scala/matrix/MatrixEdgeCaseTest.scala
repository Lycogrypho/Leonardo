package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the matrix package: each case failed before its fix. */
class MatrixEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "a matrix with complex cells" should "fold its arithmetic (F_0200)" in {
    ev("i * [[1, 2]]") match
      case Left(m: _Matrix) => assert(m.elems == Vector(_Complex.of(0, 1), _Complex.of(0, 2)), m)
      case other            => fail(s"got $other")
    ev("[[1, i]] + [[1, 1]]") match
      case Left(m: _Matrix) => assert(m.elems == Vector(_Number(2), _Complex.of(1, 1)), m)
      case other            => fail(s"got $other")
  }

  "at" should "refuse a non-whole index rather than round it (F_0205)" in {
    assert(ev("at([[1, 2], [3, 4]], 1.6, 1)").isLeft)
    assert(ev("at([[1, 2], [3, 4]], 2, 1)") == Right(_Number(3)))
  }

  "eig" should "answer two distinct eigenvalues closer than its repeat tolerance (F_0205)" in {
    ev("eig([[1, 0], [0, 1.0000000001]])") match
      case Left(m: _Matrix) => assert(m.cols == 2)
      case other            => fail(s"got $other")
    ev("eig([[1, 1], [0, 1]])") match
      case Left(_: _EigDecomposition) => succeed
      case other                      => fail(s"a defective matrix answered $other")
  }

  "dot" should "refuse a cell that contains a complex value (F_0205)" in {
    ev("dot([[x + i], [1]], [[1], [1]])") match
      case Left(_: _Dot) => succeed
      case other         => fail(s"got $other")
  }
