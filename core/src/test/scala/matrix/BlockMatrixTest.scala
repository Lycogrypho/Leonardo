package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0055 — block matrices: `hcat`, `vcat`, `blkdiag`, `ones`, `repmat`, `kron`, `submatrix`.
 *
 *  All of them rearrange (or, for `kron`, multiply) cells, so the suite checks each on the three
 *  carriers the shared assembly is meant to serve unchanged — dense, exact and symbolic — and
 *  the declines: non-conforming shapes, out-of-range or non-integer indices, and an operand whose
 *  shape is not yet known (a free name, Decision C).
 */
class BlockMatrixTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def rows(s: String, e: Environment = env): Vector[Vector[Double]] = parse(s).eval(e) match
    case Right(m: _MatrixValue) => m.toVector.grouped(m.cols).toVector
    case other                  => fail(s"\"$s\" gave no dense matrix: $other")

  private def exactCellsOf(s: String): Vector[_Expression] = parse(s, Some(30)).eval(env) match
    case Left(m: _Matrix) => m.elems
    case other            => fail(s"\"$s\" gave no exact matrix: $other")

  /** Stays unevaluated as the node it was written as. */
  private def declines(s: String, e: Environment = env): Unit =
    val node = parse(s)
    node.eval(e) match
      case Left(r) if r.getClass == node.getClass => succeed
      case other                                  => fail(s"\"$s\" should decline, got $other")

  // --- hcat / vcat ---

  "hcat" should "join blocks side by side, any number of them" in
  {
    assert(rows("hcat([[1], [2]], [[3, 4], [5, 6]])") == Vector(Vector(1.0, 3, 4), Vector(2.0, 5, 6)))
    assert(rows("hcat([[1]], [[2]], [[3]])") == Vector(Vector(1.0, 2, 3)))
  }

  it should "read a number as a 1x1 block, and never guess the shape of a free name" in
  {
    assert(rows("hcat(1, [[2]])") == Vector(Vector(1.0, 2)))
    declines("hcat(x, [[2]])")
    // Bound to a matrix, x is a 1x2 block: guessing "scalar" would have given the wrong width.
    val bound = env.withBinding("x", _MatrixValue(1, 2, Array(7.0, 8.0)))
    assert(rows("hcat(x, [[2]])", bound) == Vector(Vector(7.0, 8, 2)))
  }

  it should "decline blocks of different heights" in
  {
    declines("hcat([[1], [2]], [[3]])")
  }

  it should "keep symbolic cells symbolic, and exact cells exact" in
  {
    parse("hcat([[x]], [[1]])").eval(env) match
      case Left(m: _Matrix) => assert(m.rows == 1 && m.cols == 2 && m(0, 0) == _Variable("x"))
      case other            => fail(s"expected a symbolic matrix, got $other")
    assert(exactCellsOf("hcat([[1/2]], [[1]])").forall(_.isInstanceOf[_Rational]))
  }

  "vcat" should "stack blocks, and decline blocks of different widths" in
  {
    assert(rows("vcat([[1, 2]], [[3, 4]], 5 * [[1, 1]])") == Vector(Vector(1.0, 2), Vector(3.0, 4), Vector(5.0, 5)))
    declines("vcat([[1, 2]], [[3]])")
  }

  // --- blkdiag / ones / repmat ---

  "blkdiag" should "place blocks on the diagonal with zeros elsewhere" in
  {
    assert(rows("blkdiag([[1]], [[2, 3], [4, 5]])") ==
      Vector(Vector(1.0, 0, 0), Vector(0.0, 2, 3), Vector(0.0, 4, 5)))
    assert(rows("blkdiag(2, 3)") == Vector(Vector(2.0, 0), Vector(0.0, 3)))
  }

  it should "build its zero blocks exact when the blocks are exact" in
  {
    val cells = exactCellsOf("blkdiag([[1/2]], [[1/3]])")
    assert(cells.forall(_.isInstanceOf[_Rational]), s"a zero block demoted the matrix: $cells")
  }

  "ones" should "build an all-ones matrix in both arities, and decline a bad dimension" in
  {
    assert(rows("ones(2, 3)") == Vector.fill(2)(Vector.fill(3)(1.0)))
    assert(rows("ones(2)") == Vector.fill(2)(Vector.fill(2)(1.0)))
    declines("ones(0)")
    declines("ones(1.5, 2)")
  }

  "repmat" should "tile a block, and decline a non-positive or non-integer count" in
  {
    assert(rows("repmat([[1, 2]], 2, 2)") == Vector(Vector(1.0, 2, 1, 2), Vector(1.0, 2, 1, 2)))
    declines("repmat([[1, 2]], 0, 1)")
    declines("repmat([[1, 2]], 2, 1.5)")
  }

  it should "refuse a result past MaxMatrixCells before building it" in
  {
    val side = math.sqrt(MaxMatrixCells.toDouble).toInt + 1
    declines(s"repmat([[1]], $side, $side)")
  }

  // --- kron ---

  "kron" should "give the acceptance product" in
  {
    assert(rows("kron(eye(2), [[1, 2]])") == Vector(Vector(1.0, 2, 0, 0), Vector(0.0, 0, 1, 2)))
  }

  it should "multiply symbolic cells, read a number as 1x1, and keep exact cells exact" in
  {
    assert(rows("kron([[a]], [[1, 2]])", env.withBinding("a", _Number(3))) == Vector(Vector(3.0, 6)))
    assert(parse("kron([[a]], [[1, 2]])").eval(env).isLeft)
    assert(rows("kron(2, [[1, 2]])") == Vector(Vector(2.0, 4)))
    assert(exactCellsOf("kron([[1/2, 0]], [[1, 1/3]])").forall(_.isInstanceOf[_Rational]))
  }

  // --- submatrix ---

  private val a3 = "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"

  "submatrix" should "slice 1-based and inclusive (Decision A)" in
  {
    assert(rows(s"submatrix($a3, 2, 3, 1, 2)") == Vector(Vector(4.0, 5), Vector(7.0, 8)))
    // The 1x1 slice holds exactly at(A, i, j): the two conventions agree.
    assert(rows(s"submatrix($a3, 2, 2, 3, 3)") == Vector(Vector(6.0)))
  }

  it should "decline an out-of-range, reversed or non-integer range" in
  {
    declines(s"submatrix($a3, 2, 4, 1, 1)")
    declines(s"submatrix($a3, 0, 1, 1, 1)")
    declines(s"submatrix($a3, 3, 2, 1, 1)")
    declines(s"submatrix($a3, 1.5, 2, 1, 1)")
  }

  it should "slice a symbolic matrix without evaluating it away" in
  {
    parse("submatrix([[x, 1], [2, y]], 2, 2, 1, 2)").eval(env) match
      case Left(m: _Matrix) => assert(m.elems == Vector(_Number(2), _Variable("y")))
      case other            => fail(s"expected a symbolic row, got $other")
  }

  // --- the language ---

  "the block-matrix words" should "round-trip, and be reserved — all but sub (Decision B)" in
  {
    for s <- List("hcat([[1]], x)", "vcat([[1]], [[2]], [[3]])", "blkdiag([[1]], y)", "ones(2, 3)",
                  "repmat([[1, 2]], 2, 3)", "kron(eye(2), A)", "submatrix(A, 1, 2, 1, 2)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"round-trip failed for $s: ${e.toString}")
    for w <- List("hcat", "vcat", "blkdiag", "ones", "repmat", "kron", "submatrix") do
      assert(Parser.ReservedWords.contains(w), w)
    assert(!Parser.ReservedWords.contains("sub"))
    assert(parse("sub + 1").freeVars == Set("sub"))
  }

  it should "be matrix-shaped, so a product with one dispatches as a matrix product" in
  {
    assert(rows("hcat([[1]], [[2]]) * [[3], [4]]") == Vector(Vector(11.0)))
  }
