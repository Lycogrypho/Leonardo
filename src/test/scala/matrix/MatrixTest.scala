package it.grypho.scala.leonardo
package matrix

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

  // issue 2.1: the factory takes a defensive copy â€” mutating the source array
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
  // second evaluation pass â€” constant pairs are already _Numbers in the result.
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

  // --- issue 1.6: display(precision) must honour the requested precision ---

  "_MatrixValue display(2)" should "round elements to 2 decimal places" in
  {
    assert(dense(1, 2, 1.23456789, 2.0).display(2) == "[[1.23, 2.0]]")
  }

  "_MatrixValue display(0)" should "round elements to whole numbers" in
  {
    assert(dense(1, 2, 1.7, 2.3).display(0) == "[[2.0, 2.0]]")
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

  "a large MatProduct" should "compute correctly through the parallel blocked kernel" in
  {
    // 80Â³ exceeds the work-volume threshold (2^16) AND spans two 64-row blocks,
    // exercising the parallel multi-block path: identity * A == A.
    val n  = 80
    val id = _MatrixValue(n, n, Array.tabulate(n * n)(i => if i / n == i % n then 1.0 else 0.0))
    val a  = _MatrixValue(n, n, Array.tabulate(n * n)(_.toDouble))
    assert(evalMatrix(MatProduct(id, a)) == a)
  }

  // Straightforward triple-loop reference with the same ascending-k accumulation
  // order as the blocked kernel, so results must be bit-identical.
  def refMultiply(a: _MatrixValue, b: _MatrixValue): _MatrixValue =
    val out = new Array[Double](a.rows * b.cols)
    for i <- 0 until a.rows; j <- 0 until b.cols do
      var s = 0.0
      for k <- 0 until a.cols do s += a(i, k) * b(k, j)
      out(i * b.cols + j) = s
    _MatrixValue(a.rows, b.cols, out)

  "the blocked multiply kernel" should "match a naive reference on tile-crossing shapes" in
  {
    // Dimensions deliberately not multiples of the 64-wide tile, rectangular both ways.
    for (ar, ac, bc) <- List((70, 65, 130), (129, 64, 63), (1, 100, 1), (64, 64, 64)) do
      val a = _MatrixValue(ar, ac, Array.tabulate(ar * ac)(i => (i % 7) - 3.0))
      val b = _MatrixValue(ac, bc, Array.tabulate(ac * bc)(i => (i % 5) - 2.0))
      assert(a.multiply(b) == refMultiply(a, b), s"mismatch for ${ar}x$ac * ${ac}x$bc")
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

  // --- issue 2.1: element-wise algorithms distribute over matrices ---

  "derive of a matrix" should "differentiate element-wise" in
  {
    val m = literal(1, 2, Power(x, _Number(2)), Sin(x))
    // d/dx [xÂ², sin(x)] = [2x, cos(x)] â†’ at x=2: [4, cos(2)]
    assert(evalMatrix(_Derivative(m, x), envWith("x" -> 2.0)) == dense(1, 2, 4.0, math.cos(2.0)))
  }

  "integrate of a matrix" should "integrate element-wise, constants included" in
  {
    val m = literal(1, 2, x, _Number(1))
    // âˆ« [x, 1] dx = [xÂ²/2, x] â†’ at x=2: [2, 2]
    assert(evalMatrix(_Integral(m, x), envWith("x" -> 2.0)) == dense(1, 2, 2.0, 2.0))
  }

  "integrate of a v-independent matrix" should "integrate per element, not wrap in a scalar Product" in
  {
    assert(integrate(literal(1, 2, y, _Number(2)), x)
      == literal(1, 2, Product(y, x), Product(_Number(2), x)))
  }

  "simplify" should "simplify inside matrix elements" in
  {
    val m = literal(1, 2, Sum(x, _Number(0)), Product(_Number(1), y))
    assert(simplify(m) == literal(1, 2, x, y))
  }

  "expand" should "distribute inside matrix elements" in
  {
    val m = literal(1, 1, Product(x, Sum(y, _Number(1))))
    assert(expand(m) == literal(1, 1, Sum(Product(x, y), Product(x, _Number(1)))))
  }

  "derive of a MatSum" should "distribute over both operands (linearity)" in
  {
    val d = _Derivative(MatSum(literal(1, 1, Power(x, _Number(2))), literal(1, 1, Product(_Number(3), x))), x)
    // d/dx (xÂ² + 3x) = 2x + 3 â†’ at x=2: 7
    assert(evalMatrix(d, envWith("x" -> 2.0)) == dense(1, 1, 7.0))
  }

  "derive of a Transpose" should "commute with transposition" in
  {
    val d = _Derivative(Transpose(literal(1, 2, Power(x, _Number(2)), x)), x)
    // (d/dx [xÂ², x])áµ€ = [2x, 1]áµ€ â†’ at x=3: column (6, 1)
    assert(evalMatrix(d, envWith("x" -> 3.0)) == dense(2, 1, 6.0, 1.0))
  }

  "derive of a MatProduct" should "stay symbolic (needs the product rule, not marked element-wise)" in
  {
    val p = MatProduct(literal(1, 1, x), literal(1, 1, Sin(x)))
    _Derivative(p, x).eval(envWith("x" -> 2.0)) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  // --- issue 4.1: scalar operations evaluate concrete matrix operands ---

  "scalar Sum of two bound matrix variables" should "add the dense values" in
  {
    val env = new Environment()
      .withBinding("M", dense(2, 2, 1, 2, 3, 4))
      .withBinding("N", dense(2, 2, 10, 20, 30, 40))
    assert(evalMatrix(Sum(_Variable("M"), _Variable("N")), env) == dense(2, 2, 11, 22, 33, 44))
  }

  "scalar Sum of matrices with mismatched dimensions" should "stay symbolic" in
  {
    val s = Sum(_Variable("M"), _Variable("N"))
    val env = new Environment()
      .withBinding("M", dense(2, 2, 1, 2, 3, 4))
      .withBinding("N", dense(1, 2, 1, 2))
    assert(s.eval(env) == Left(s))
  }

  "scalar Product" should "dispatch scale and matrix multiply on concrete values" in
  {
    val env = new Environment()
      .withBinding("M", dense(2, 2, 1, 2, 3, 4))
      .withBinding("N", dense(2, 2, 5, 6, 7, 8))
    assert(evalMatrix(Product(_Number(2), _Variable("M")), env) == dense(2, 2, 2, 4, 6, 8))
    assert(evalMatrix(Product(_Variable("M"), _Number(2)), env) == dense(2, 2, 2, 4, 6, 8))
    assert(evalMatrix(Product(_Variable("M"), _Variable("N")), env) == dense(2, 2, 19, 22, 43, 50))
  }

  "scalar Product of 0 and a concrete matrix" should "be the zero matrix, not the scalar 0" in
  {
    val env = new Environment().withBinding("M", dense(1, 2, 1, 2))
    assert(evalMatrix(Product(_Number(0), _Variable("M")), env) == dense(1, 2, 0, 0))
  }

  "scalar Ratio of a matrix by a number" should "scale by the reciprocal" in
  {
    val env = new Environment().withBinding("M", dense(1, 2, 2, 4))
    assert(evalMatrix(Ratio(_Variable("M"), _Number(2)), env) == dense(1, 2, 1, 2))
    // division by zero stays symbolic (non-finite elements)
    assert(Ratio(_Variable("M"), _Number(0)).eval(env).isLeft)
  }

  "parse + eval of a full matrix expression" should "compute end to end" in
  {
    val result = parser.Parser.parse("[[1, 2], [3, 4]] * [[5, 6], [7, 8]] + [[1, 0], [0, 1]]")
    assert(result.successful, s"parse failed: $result")
    assert(evalMatrix(result.get) == dense(2, 2, 20, 22, 43, 51))
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

  // --- issue 4.1: at(A, i, j) matrix element extraction (1-based) ---

  "at([[1, 2], [3, 4]], 2, 1)" should "return 3 (1-based row 2 col 1)" in
  {
    val expr = parser.Parser.parse("at([[1, 2], [3, 4]], 2, 1)")
    assert(expr.successful, s"parse failed: $expr")
    assert(expr.get.eval(new Environment()) == Right(_Number(3.0)))
  }

  "at([[1, 2], [3, 4]], 1, 2)" should "return 2 (1-based row 1 col 2)" in
  {
    assert(parser.Parser.parse("at([[1, 2], [3, 4]], 1, 2)").get.eval(new Environment()) == Right(_Number(2.0)))
  }

  "at(A, i, j) with A bound in the environment" should "resolve through the binding" in
  {
    val env = new Environment().withBinding("A", dense(2, 2, 1, 2, 3, 4))
    assert(parser.Parser.parse("at(A, 2, 1)").get.eval(env) == Right(_Number(3.0)))
  }

  "at([[x, 2]], 1, 2)" should "return 2.0 even though the matrix is partially symbolic" in
  {
    // The matrix is symbolic (x is free) but the requested element is concrete.
    assert(parser.Parser.parse("at([[x, 2]], 1, 2)").get.eval(new Environment()) == Right(_Number(2.0)))
  }

  "at([[x, 2]], 1, 1)" should "stay symbolic when the element is a free variable" in
  {
    val result = parser.Parser.parse("at([[x, 2]], 1, 1)").get.eval(new Environment())
    assert(result.isLeft, s"expected symbolic result but got: $result")
  }

  "at(A, 3, 1) on a 2Ã—2 matrix" should "stay symbolic (out of bounds)" in
  {
    val result = _MatrixIndex(dense(2, 2, 1, 2, 3, 4), _Number(3), _Number(1)).eval(new Environment())
    assert(result.isLeft, s"expected symbolic (out-of-bounds) but got: $result")
  }

  "at(...) toString" should "round-trip through the parser" in
  {
    val e = _MatrixIndex(literal(1, 2, x, y), _Number(1), _Number(1))
    val s = e.toString
    val reparsed = parser.Parser.parse(s)
    assert(reparsed.successful, s"round-trip parse failed for: $s")
  }

  "bare 'at'" should "be a reserved word (parse error)" in
  {
    assert(!parser.Parser.parse("at").successful)
  }

  "at(A, i, j) in the REPL" should "extract elements from a bound matrix" in
  {
    val s = cli.Session()
    s.execute("A := [[1, 2], [3, 4]]")
    assert(s.execute("at(A, 2, 1)") == "3.0")
    assert(s.execute("at(A, 1, 2)") == "2.0")
  }

  // --- Kronecker / vec / unvec / identity kernels (issue 4.5 prerequisites) ---

  "kronecker" should "produce the block matrix of scaled copies" in
  {
    // [[1,2],[3,4]] kron [[0,1],[1,0]]
    val k = dense(2, 2, 1, 2, 3, 4).kronecker(dense(2, 2, 0, 1, 1, 0))
    val expected = dense(4, 4,
      0, 1, 0, 2,
      1, 0, 2, 0,
      0, 3, 0, 4,
      3, 0, 4, 0)
    assert(k == expected)
  }

  "kronecker" should "satisfy the vec identity vec(A*X*B) = (Bt kron A)*vec(X)" in
  {
    val a  = dense(2, 2, 1, 2, 0, 3)
    val b  = dense(2, 2, 4, 1, 0, 5)
    val x0 = dense(2, 2, 1, 0, 2, 1)
    val lhs = a.multiply(x0).multiply(b).vec
    val rhs = b.transpose.kronecker(a).multiply(x0.vec)
    for i <- 0 until 4 do assert(math.abs(lhs(i, 0) - rhs(i, 0)) < 1e-12)
  }

  "vec" should "stack columns into a column vector" in
  {
    // [[1,2],[3,4]] -> columns (1,3) then (2,4)
    val v = dense(2, 2, 1, 2, 3, 4).vec
    assert(v.rows == 4 && v.cols == 1)
    assert(v.toVector == Vector(1.0, 3.0, 2.0, 4.0))
  }

  "unvec" should "invert vec" in
  {
    val m = dense(2, 3, 1, 2, 3, 4, 5, 6)
    assert(_MatrixValue.unvec(m.vec, 2, 3).contains(m))
  }

  "unvec with non-conforming dimensions" should "be None" in
  {
    assert(_MatrixValue.unvec(dense(3, 1, 1, 2, 3), 2, 2).isEmpty)
  }

  "_MatrixValue.identity" should "be the multiplicative identity" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(_MatrixValue.identity(2).multiply(a) == a)
    assert(a.multiply(_MatrixValue.identity(2)) == a)
  }
