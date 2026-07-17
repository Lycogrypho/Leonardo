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
