package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0089 — matrices held under names in exact mode.
 *
 *  An exact matrix stays a symbolic `_Matrix` of rationals, so `A := [[1, 2], [3, 4]]` is a
 *  **definition**, and the prompt substitutes it before evaluating. The substituted tree held
 *  `Product(_Matrix, _Matrix)`, a shape the parser never builds for literals, so `A * B` did not
 *  evaluate and `A * B * transpose(B)` scaled B's cells by the unevaluated product. Every case
 *  here compares a named chain against the same chain typed as literals.
 */
class ExactMatrixReplTest extends AnyFlatSpec:

  private val A = "[[1, 2], [3, 4]]"
  private val B = "[[1/2, 0], [1, 1/3]]"

  private def exactSession(): Session =
    val s = Session()
    s.execute("exact on")
    s.execute(s"A := $A")
    s.execute(s"B := $B")
    s

  "named exact matrices" should "multiply, add and negate exactly as the literals do" in
  {
    val s = exactSession()
    for (named, literal) <- List(
        "A * B"                -> s"$A * $B",
        "A * B * B"            -> s"$A * $B * $B",
        "A * B * transpose(B)" -> s"$A * $B * transpose($B)",
        "A + B"                -> s"$A + $B",
        "A - B"                -> s"$A - $B",
        "-A"                   -> s"-$A",
        "2 * A"                -> s"2 * $A",
        "A * [[1], [1]]"       -> s"$A * [[1], [1]]"
      ) do
      val got = s.execute(named)
      assert(got == s.execute(literal), s"$named")
      assert(got.startsWith("[[") && !got.contains("*") && !got.contains("+"), s"$named did not reduce: $got")
  }

  it should "stay exact" in
  {
    val got = exactSession().execute("A * B")
    assert(got.contains("5/2") && got.contains("2/3") && got.contains("11/2") && got.contains("4/3"), got)
  }

  "a difference of exact matrix literals" should "stay exact (the parser's negation used an inexact -1)" in
  {
    val s = Session()
    s.execute("exact on")
    assert(s.execute(s"$A - $B") == "[[1/2, 2], [2, 11/3]]")
    assert(s.execute(s"-(-$B)") == s.execute(B))
  }

  it should "reduce through tuple assignment and an assignment of a product" in
  {
    val s = exactSession()
    s.execute("C := A * B")
    assert(s.execute("C") == s.execute("A * B"))
    s.execute("L, U, P := lu(A * B)")
    // The factors are exact since F_0226, so both sides are exact and agree exactly.
    assert(s.execute("L * U") == s.execute("P * A * B"))
    assert(!s.execute("L * U").contains("."))
  }

  "dense named matrices" should "print exactly as before, a free scalar factor included" in
  {
    val s = Session()
    s.execute(s"A := $A")
    assert(s.execute("A * x") == "([[1.0, 2.0], [3.0, 4.0]] * x)")
    assert(s.execute("A * A") == "[[7.0, 10.0], [15.0, 22.0]]")
  }
