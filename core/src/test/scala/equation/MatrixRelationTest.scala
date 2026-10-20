package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0164 — `==` and `=` decide between matrices whose cells are all values, exact ones included;
 *  F_0165 — `solve` retries once on its sides as `eval` reduces them.
 *
 *  F_0164: an exact matrix is the symbolic `_Matrix` carrier, so the comparison read only dense
 *  matrices and `[[1/2]] == [[1/2]]` echoed itself.  Cells now compare exactly between rationals and
 *  by the `=` tolerance otherwise; a symbolic cell still leaves the relation symbolic.
 *
 *  F_0165: the scalar tiers read `collect`'s polynomial and compile the difference, and neither
 *  sees through a node such as `dot` or `det` that `eval` turns into a polynomial of the unknown.
 *  The retry runs only after every tier declined, so no existing answer changes.
 */
class MatrixRelationTest extends AnyFlatSpec:

  private val env = new Environment()
  private val k   = _Variable("k")

  private def eval(s: String, exact: Boolean = true): Either[_Expression, _Value] =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x, _) => x.eval(env)
      case other                => fail(s"parse failed for \"$s\": $other")

  private def parseEq(s: String): _Equation = Parser.parse(s) match
    case Parser.Success(e: _Equation, _) => e
    case other                           => fail(s"$other")

  // --- F_0164 ---

  "== between exact matrices" should "answer, exactly" in {
    assert(eval("[[1/2, 1], [2, 3]] == [[1/2, 1], [2, 3]]") == Right(_Bool(true)))
    assert(eval("[[1/3]] == [[1/2]]") == Right(_Bool(false)))
    assert(eval("transpose([[1/2, 1]]) == [[1/2], [1]]") == Right(_Bool(true)))
  }

  it should "compare shapes, and an exact cell with a Double one by tolerance" in {
    assert(eval("[[1, 2]] == [[1], [2]]") == Right(_Bool(false)))
    assert(eval("[[1/2, 1]] == [[0.5, 1.0]]", exact = false) == Right(_Bool(true)))
    assert(eval("[[1/2]] = [[1/2]]") == Right(_Bool(true)))
  }

  it should "stay symbolic while a cell is" in {
    assert(eval("[[x, 1]] == [[1/2, 1]]").isLeft)
  }

  // --- F_0165 ---

  "solve" should "see through a dot product of the unknown" in {
    assert(solve(parseEq("dot([[6], [k]], [[-1], [3]]) = 0"), k, env) == List(_Equation(k, _Number(2))))
  }

  it should "see through a determinant of the unknown" in {
    val roots = solve(parseEq("det([[k, 1], [1, k]]) = 0"), k, env).map {
      case _Equation(_, _Number(d)) => d
      case other                    => fail(s"$other")
    }
    assert(roots.sorted == List(-1.0, 1.0))
  }

  it should "leave an equation every tier already answers exactly as it was" in {
    assert(solve(parseEq("10 * k = 2 * k + 1"), k, env) == List(_Equation(k, _Number(0.125))))
    assert(solve(parseEq("sin(k) = sin(k)"), k, env).isEmpty)
  }
