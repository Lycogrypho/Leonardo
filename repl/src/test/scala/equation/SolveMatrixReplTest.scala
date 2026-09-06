package it.grypho.scala.leonardo
package equation

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL sessions for the matrix solver: a scalar unknown inside a matrix equation, a matrix
 *  unknown in each recognised shape (`A*X = B`, `X*A = B`, `A*X + C = B`, `A*X*D = B`), and
 *  the Sylvester / Lyapunov / scalar-coefficient tier.
 *
 *  Split out of `SolveMatrixTest` by issue 5.2 phase 1.1.
 */
class SolveMatrixReplTest extends AnyFlatSpec:

  // --- 4.3a: scalar unknown in a matrix equation ---

  "the reported REPL session" should "report no consistent solution" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [1, 3]]")
    s.execute("eq := A + [[x, x], [2*x, 3*x]] = [[1, 3], [3, 6]]")
    // no root verifies -> the solve node stays symbolic (prints itself)
    assert(s.execute("solve(eq, x)").startsWith("solve("))
  }

  "a consistent matrix equation in the REPL" should "solve for the scalar and auto-bind it" in
  {
    val s = Session()
    assert(s.execute("solve([[x, x]] = [[2, 2]], x)") == "x := 2.0")
    assert(s.execute("x") == "2.0")
  }

  // --- 4.3b / 4.4: matrix unknown, concrete and symbolic coefficients ---

  "the REPL" should "solve a matrix unknown A*X = B and auto-bind X" in
  {
    val s = Session()
    s.execute("A := [[2, 0], [0, 2]]")
    s.execute("B := [[4, 6], [8, 10]]")
    assert(s.execute("solve(A * X = B, X)") == "X := [[2.0, 3.0], [4.0, 5.0]]")
    assert(s.execute("X") == "[[2.0, 3.0], [4.0, 5.0]]")
  }

  "solve([[a, 0], [0, a]] * X = [[1], [2]], X)" should "give a symbolic solution and evaluate when a is bound" in
  {
    val s = Session()
    val result = s.execute("solve([[a, 0], [0, a]] * X = [[1], [2]], X)")
    assert(result.startsWith("X :="), s"expected 'X := ...' but got: $result")
    s.execute("a := 2")
    val numeric = s.execute("X")
    assert(numeric == "[[0.5], [1.0]]", s"with a=2 expected [[0.5], [1.0]] but got: $numeric")
  }

  "solve(X * A = B, X) with concrete A and symbolic B row" should "give X = B * A^-1 symbolically" in
  {
    // A = diag(2, 2), B = [[b, 2*b]] (row, free variable) -> X = [[b/2, b]]
    val s = Session()
    s.execute("A := [[2, 0], [0, 2]]")
    val result = s.execute("solve(X * A = [[b, 2*b]], X)")
    assert(result.startsWith("X :="), s"expected 'X := ...' but got: $result")
    s.execute("b := 3")
    val numeric = s.execute("X")
    assert(numeric == "[[1.5, 3.0]]", s"with b=3 expected [[1.5, 3.0]] but got: $numeric")
  }

  // --- 4.5: affine term and two-sided product ---

  "the REPL" should "solve an affine matrix equation A*X + C = B" in
  {
    val s = Session()
    s.execute("A := [[2, 0], [0, 2]]")
    s.execute("C := [[1, 1], [1, 1]]")
    s.execute("B := [[5, 7], [9, 11]]")
    assert(s.execute("solve(A * X + C = B, X)") == "X := [[2.0, 3.0], [4.0, 5.0]]")
  }

  "the REPL" should "solve a two-sided matrix equation A*X*D = B" in
  {
    val s = Session()
    s.execute("A := [[2, 0], [0, 2]]")
    s.execute("D := [[5, 0], [0, 5]]")
    s.execute("B := [[10, 20], [30, 40]]")
    assert(s.execute("solve(A * X * D = B, X)") == "X := [[1.0, 2.0], [3.0, 4.0]]")
  }

  // --- 4.5: Sylvester tier (Kronecker vectorization) ---

  "the REPL" should "solve a Sylvester equation A*X + X*B = C" in
  {
    // X0 = [[1,0],[2,1]] -> C = A*X0 + X0*B = [[9,3],[14,10]]
    val s = Session()
    s.execute("A := [[1, 2], [0, 3]]")
    s.execute("B := [[4, 1], [0, 5]]")
    s.execute("C := [[9, 3], [14, 10]]")
    assert(s.execute("solve(A * X + X * B = C, X)") == "X := [[1.0, 0.0], [2.0, 1.0]]")
  }

  "the REPL" should "solve a Lyapunov equation A*X + X*transpose(A) = C" in
  {
    // A eigs {2,3}: no eigenvalue shared with -transpose(A) -> unique; X = I satisfies
    // A + transpose(A) = C.
    val s = Session()
    s.execute("A := [[2, 1], [0, 3]]")
    s.execute("C := [[4, 1], [1, 6]]")
    assert(s.execute("solve(A * X + X * transpose(A) = C, X)") == "X := [[1.0, 0.0], [0.0, 1.0]]")
  }

  "the REPL" should "solve a scalar-coefficient matrix equation 2*X = B" in
  {
    val s = Session()
    s.execute("B := [[4, 6], [8, 10]]")
    assert(s.execute("solve(2 * X = B, X)") == "X := [[2.0, 3.0], [4.0, 5.0]]")
  }
