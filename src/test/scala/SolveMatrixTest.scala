package it.grypho.scala.leonardo

import core.*
import scalar.*
import matrix.*
import equation.*
import parser.Parser
import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


// Issue 4.3a — solving a matrix equation for a SCALAR unknown by element-wise
// decomposition and intersection of the per-cell solution sets.
class SolveMatrixTest extends AnyFlatSpec:

  val x = _Variable("x")

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def parseEq(input: String): _Equation =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get match
      case eq: _Equation => eq
      case other         => fail(s"expected an equation but got: $other")

  def roots(s: List[_Equation]): List[Double] =
    s.map {
      case _Equation(_, _Number(d)) => d
      case other                    => fail(s"expected a numeric solution but got: $other")
    }

  // --- consistent systems: the shared root survives the intersection ---

  "solve([[x, x]] = [[2, 2]], x)" should "give the shared root x = 2" in
  {
    assert(roots(solve(parseEq("[[x, x]] = [[2, 2]]"), x)) == List(2.0))
  }

  "solve([[x, 2*x]] = [[3, 6]], x)" should "agree across cells (x = 3)" in
  {
    assert(roots(solve(parseEq("[[x, 2*x]] = [[3, 6]]"), x)) == List(3.0))
  }

  "constant cells that are satisfied" should "not constrain the unknown" in
  {
    // [[x, 0], [0, x]] = [[3, 0], [0, 3]] : the two 0 = 0 cells are identities
    assert(roots(solve(parseEq("[[x, 0], [0, x]] = [[3, 0], [0, 3]]"), x)) == List(3.0))
  }

  "a quadratic cell intersected with a linear cell" should "keep only the common root" in
  {
    // [[x^2, x]] = [[4, 2]] : cell 1 gives ±2, cell 2 gives 2 → intersection {2}
    assert(roots(solve(parseEq("[[x^2, x]] = [[4, 2]]"), x)) == List(2.0))
  }

  // --- inconsistent systems: empty solution set ---

  "the reported inconsistent equation" should "have no solution" in
  {
    // [[1,2],[1,3]] + [[x, x],[2x, 3x]] = [[1,3],[3,6]] forces x = 0 in cell (1,1)
    // but x = 1 in the other three cells → no value satisfies every cell.
    val eq = parseEq("[[1, 2], [1, 3]] + [[x, x], [2*x, 3*x]] = [[1, 3], [3, 6]]")
    assert(solve(eq, x).isEmpty)
  }

  "cells forcing different roots" should "intersect to nothing" in
  {
    // cell 1 → x = 1, cell 2 → x = 2
    assert(solve(parseEq("[[x, x]] = [[1, 2]]"), x).isEmpty)
  }

  "an inconsistent constant cell" should "make the whole system unsolvable" in
  {
    // cell 1 → x = 2, cell 2 is 5 = 6 (never true)
    assert(solve(parseEq("[[x, 5]] = [[2, 6]]"), x).isEmpty)
  }

  // --- dimension mismatch: no scalar value can satisfy it ---

  "a dimension mismatch" should "have no solution" in
  {
    assert(solve(parseEq("[[x, x]] = [[2, 2], [2, 2]]"), x).isEmpty)
  }

  // --- purely scalar equations still go through the scalar tiers ---

  "a scalar equation" should "be unaffected by the matrix path" in
  {
    assert(roots(solve(parseEq("10 * x = 2 * x + 1"), x)) == List(0.125))
  }

  // --- _Solve node / REPL end to end ---

  "the _Solve node on a consistent matrix equation" should "yield the single solution" in
  {
    assert(parser.Parser.parse("solve([[x, x]] = [[2, 2]], x)").get.eval(new Environment())
      == Left(_Equation(x, _Number(2))))
  }

  "the _Solve node on an inconsistent matrix equation" should "stay symbolic" in
  {
    val node = parser.Parser.parse("solve([[x, x]] = [[1, 2]], x)").get
    assert(node.eval(new Environment()) == Left(node))
  }

  "the reported REPL session" should "report no consistent solution" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [1, 3]]")
    s.execute("eq := A + [[x, x], [2*x, 3*x]] = [[1, 3], [3, 6]]")
    // no root verifies → the solve node stays symbolic (prints itself)
    assert(s.execute("solve(eq, x)").startsWith("solve("))
  }

  "a consistent matrix equation in the REPL" should "solve for the scalar" in
  {
    val s = Session()
    assert(s.execute("solve([[x, x]] = [[2, 2]], x)") == "x = 2.0")
  }

  // --- 4.3b: unknown MATRIX x (A·x = B) ---

  "solve(A*x = B, x)" should "give x = A⁻¹·B" in
  {
    val a = dense(2, 2, 2, 0, 0, 2)          // 2·I  → A⁻¹ = 0.5·I
    val b = dense(2, 2, 4, 6, 8, 10)
    assert(solve(_Equation(Product(a, x), b), x) == List(_Equation(x, dense(2, 2, 2, 3, 4, 5))))
  }

  "solve(x*A = B, x)" should "give x = B·A⁻¹" in
  {
    val a = dense(2, 2, 2, 0, 0, 2)
    val b = dense(2, 2, 2, 3, 4, 5)
    assert(solve(_Equation(Product(x, a), b), x) == List(_Equation(x, dense(2, 2, 1, 1.5, 2, 2.5))))
  }

  "solve(x = B, x) for a matrix B" should "give x = B" in
  {
    val b = dense(2, 2, 1, 2, 3, 4)
    assert(solve(_Equation(x, b), x) == List(_Equation(x, b)))
  }

  "the matrix-unknown solution" should "satisfy the original equation" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)          // invertible (det = -2)
    val b = dense(2, 2, 5, 6, 7, 8)
    solve(_Equation(Product(a, x), b), x) match
      case _Equation(_, sol: _MatrixValue) :: Nil =>
        val prod = a.multiply(sol)
        for i <- 0 until 2; j <- 0 until 2 do assert(math.abs(prod(i, j) - b(i, j)) < 1e-9)
      case other => fail(s"expected one matrix solution but got: $other")
  }

  "solve(A*x = B, x) with a singular A" should "have no solution" in
  {
    val a = dense(2, 2, 1, 2, 2, 4)          // singular
    val b = dense(2, 2, 1, 0, 0, 1)
    assert(solve(_Equation(Product(a, x), b), x).isEmpty)
  }

  "solve(A*x = B, x) with nonconforming shapes" should "have no solution" in
  {
    val a = dense(2, 2, 2, 0, 0, 2)
    val b = dense(3, 2, 1, 2, 3, 4, 5, 6)    // A⁻¹ is 2×2, cannot left-multiply a 3×2
    assert(solve(_Equation(Product(a, x), b), x).isEmpty)
  }

  "the REPL" should "solve a matrix unknown A*X = B" in
  {
    val s = Session()
    s.execute("A := [[2, 0], [0, 2]]")
    s.execute("B := [[4, 6], [8, 10]]")
    assert(s.execute("solve(A * X = B, X)") == "X = [[2.0, 3.0], [4.0, 5.0]]")
  }
