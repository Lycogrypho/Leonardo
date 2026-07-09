package it.grypho.scala.leonardo

import core.*
import scalar.*
import matrix.*
import org.scalatest.flatspec.AnyFlatSpec


class MatrixTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  def evalMatrix(e: _Expression, env: Environment = new Environment()): _MatrixValue =
    e.eval(env) match
      case Right(m: _MatrixValue) => m
      case other                  => fail(s"expected a dense matrix value but got: $other")

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def literal(rows: Int, cols: Int, elems: _Expression*): _Matrix =
    _Matrix(rows, cols, elems.toVector)

  // --- construction ---

  "a matrix literal with the wrong element count" should "be rejected" in
  {
    assertThrows[IllegalArgumentException](literal(2, 2, _Number(1), _Number(2), _Number(3)))
  }

  "ofRows with ragged rows" should "be rejected" in
  {
    assertThrows[IllegalArgumentException](
      _Matrix.ofRows(Vector(_Number(1), _Number(2)), Vector(_Number(3)))
    )
  }

  "ofRows" should "build a row-major matrix" in
  {
    val m = _Matrix.ofRows(Vector(_Number(1), _Number(2)), Vector(_Number(3), _Number(4)))
    assert(m == literal(2, 2, _Number(1), _Number(2), _Number(3), _Number(4)))
  }

  // --- dual evaluation: collapse to dense value vs stay symbolic ---

  "a numeric matrix literal" should "collapse to a dense _MatrixValue" in
  {
    val m = literal(2, 2, _Number(1), _Number(2), _Number(3), _Number(4))
    assert(evalMatrix(m) == dense(2, 2, 1, 2, 3, 4))
  }

  "a matrix with a free variable" should "stay symbolic with elements reduced" in
  {
    val m = literal(1, 2, Sum(_Number(1), _Number(2)), x)
    m.eval(new Environment()) match
      case Left(reduced: _Matrix) =>
        assert(reduced(0, 0) == _Number(3))   // constant element folded
        assert(reduced(0, 1) == x)            // free variable kept symbolic
      case other => fail(s"expected a symbolic matrix but got: $other")
  }

  // Elements are arbitrary expressions: functions and functionals inside a matrix
  // evaluate element-wise once their variables are bound.
  "a matrix containing functions and functionals" should "evaluate element-wise" in
  {
    val m = literal(1, 3,
      Sin(x),
      _Derivative(Power(x, _Number(2)), x),
      _DefIntegral(x, x, _Number(0), _Number(2))
    )
    val v = evalMatrix(m, envWith("x" -> 3.0))
    assert(math.abs(v(0, 0) - math.sin(3.0)) < 1e-5)
    assert(math.abs(v(0, 1) - 6.0) < 1e-4)
    assert(math.abs(v(0, 2) - 2.0) < 1e-4)
  }

  "a matrix value bound in the environment" should "be retrievable through a variable" in
  {
    val mv  = dense(2, 2, 1, 2, 3, 4)
    val env = new Environment().withBinding("M", mv)
    assert(_Variable("M").eval(env) == Right(mv))
  }

  // --- _MatrixValue semantics ---

  "_MatrixValue equality" should "compare dimensions and contents" in
  {
    assert(dense(2, 2, 1, 2, 3, 4) == dense(2, 2, 1, 2, 3, 4))
    assert(dense(2, 2, 1, 2, 3, 4) != dense(2, 2, 1, 2, 3, 5))
    assert(dense(2, 2, 1, 2, 3, 4) != dense(4, 1, 1, 2, 3, 4))
    assert(dense(2, 2, 1, 2, 3, 4).hashCode == dense(2, 2, 1, 2, 3, 4).hashCode)
  }

  // issue 2.1: the factory takes a defensive copy — mutating the source array
  // after construction must not change the value (equality, hashing, elements).
  "_MatrixValue" should "be immune to mutation of the array it was built from" in
  {
    val arr = Array(1.0, 2.0, 3.0, 4.0)
    val m   = _MatrixValue(2, 2, arr)
    val h   = m.hashCode
    arr(0) = 99.0
    assert(m(0, 0) == 1.0)
    assert(m == dense(2, 2, 1, 2, 3, 4))
    assert(m.hashCode == h)
  }

  // issue 2.2: the symbolic path folds concrete element pairs directly, without a
  // second evaluation pass — constant pairs are already _Numbers in the result.
  "MatSum's symbolic path" should "fold concrete element pairs eagerly" in
  {
    MatSum(literal(1, 2, x, _Number(1)), dense(1, 2, 10, 20)).eval(new Environment()) match
      case Left(m: _Matrix) =>
        assert(m(0, 0) == Sum(x, _Number(10)))   // symbolic pair stays a Sum node
        assert(m(0, 1) == _Number(21))           // concrete pair folded, no re-eval needed
      case other => fail(s"expected a symbolic matrix but got: $other")
  }

  "_MatrixValue toString" should "render rows with display rounding" in
  {
    assert(dense(2, 2, 1, 2, 3, 4.123456789).toString == "[[1.0, 2.0], [3.0, 4.12346]]")
  }

  // --- MatSum ---

  "MatSum of two dense matrices" should "add element-wise" in
  {
    val s = MatSum(dense(2, 2, 1, 2, 3, 4), dense(2, 2, 10, 20, 30, 40))
    assert(evalMatrix(s) == dense(2, 2, 11, 22, 33, 44))
  }

  "MatSum with a symbolic operand" should "stay symbolic until the variable is bound" in
  {
    val s = MatSum(literal(1, 2, x, _Number(1)), dense(1, 2, 10, 20))
    assert(s.eval(new Environment()).isLeft)
    assert(evalMatrix(s, envWith("x" -> 5.0)) == dense(1, 2, 15, 21))
  }

  "MatSum with mismatched dimensions" should "stay symbolic" in
  {
    val s = MatSum(dense(2, 2, 1, 2, 3, 4), dense(1, 2, 1, 2))
    assert(s.eval(new Environment()) == Left(s))
  }

  "MatSum of two non-matrix operands" should "stay symbolic" in
  {
    assert(MatSum(_Variable("M"), _Variable("N")).eval(new Environment()).isLeft)
  }

  // --- MatProduct ---

  "MatProduct of two dense 2x2 matrices" should "compute the matrix product" in
  {
    val p = MatProduct(dense(2, 2, 1, 2, 3, 4), dense(2, 2, 5, 6, 7, 8))
    assert(evalMatrix(p) == dense(2, 2, 19, 22, 43, 50))
  }

  "MatProduct of rectangular matrices" should "produce the right dimensions and values" in
  {
    // (2x3) * (3x2) = 2x2
    val p = MatProduct(dense(2, 3, 1, 2, 3, 4, 5, 6), dense(3, 2, 7, 8, 9, 10, 11, 12))
    assert(evalMatrix(p) == dense(2, 2, 58, 64, 139, 154))
  }

  "MatProduct with incompatible dimensions" should "stay symbolic" in
  {
    val p = MatProduct(dense(2, 2, 1, 2, 3, 4), dense(3, 2, 1, 2, 3, 4, 5, 6))
    assert(p.eval(new Environment()) == Left(p))
  }

  "MatProduct with a symbolic operand" should "combine element-wise and fold when bound" in
  {
    // [[x, 0], [0, x]] * [[1, 2], [3, 4]] = x * [[1, 2], [3, 4]]
    val diag = literal(2, 2, x, _Number(0), _Number(0), x)
    val p    = MatProduct(diag, dense(2, 2, 1, 2, 3, 4))
    assert(p.eval(new Environment()).isLeft)
    assert(evalMatrix(p, envWith("x" -> 2.0)) == dense(2, 2, 2, 4, 6, 8))
  }

  "a large MatProduct" should "compute correctly through the parallel kernel" in
  {
    // 48³ exceeds the sequential/parallel threshold (2^16): identity * A == A.
    val n  = 48
    val id = _MatrixValue(n, n, Array.tabulate(n * n)(i => if i / n == i % n then 1.0 else 0.0))
    val a  = _MatrixValue(n, n, Array.tabulate(n * n)(_.toDouble))
    assert(evalMatrix(MatProduct(id, a)) == a)
  }

  // --- MatScale ---

  "MatScale by a number" should "scale every element" in
  {
    val s = MatScale(_Number(3), dense(2, 2, 1, 2, 3, 4))
    assert(evalMatrix(s) == dense(2, 2, 3, 6, 9, 12))
  }

  "MatScale by a symbolic scalar" should "stay symbolic until the scalar is bound" in
  {
    val s = MatScale(x, dense(1, 2, 1, 2))
    assert(s.eval(new Environment()).isLeft)
    assert(evalMatrix(s, envWith("x" -> 2.0)) == dense(1, 2, 2, 4))
  }

  // --- Transpose ---

  "Transpose of a dense matrix" should "swap rows and columns" in
  {
    val t = Transpose(dense(2, 3, 1, 2, 3, 4, 5, 6))
    assert(evalMatrix(t) == dense(3, 2, 1, 4, 2, 5, 3, 6))
  }

  "Transpose of a symbolic matrix" should "rearrange the elements and stay symbolic" in
  {
    Transpose(literal(1, 2, x, y)).eval(new Environment()) match
      case Left(t: _Matrix) =>
        assert(t.rows == 2 && t.cols == 1)
        assert(t(0, 0) == x && t(1, 0) == y)
      case other => fail(s"expected a symbolic matrix but got: $other")
  }

  "Transpose of a transpose" should "give back the original values" in
  {
    val a = dense(2, 3, 1, 2, 3, 4, 5, 6)
    assert(evalMatrix(Transpose(Transpose(a))) == a)
  }

  // --- generic traversals work through matrix elements ---

  "substitute" should "replace definitions inside matrix elements" in
  {
    val m = literal(1, 2, _Variable("f"), _Number(1))
    val substituted = substitute(m, Map("f" -> Sum(x, _Number(1))))
    assert(substituted == literal(1, 2, Sum(x, _Number(1)), _Number(1)))
  }

  "dependsOn" should "see variables inside matrix elements" in
  {
    val m = literal(2, 1, Sin(x), _Number(2))
    assert(dependsOn(m, x))
    assert(!dependsOn(m, y))
  }
