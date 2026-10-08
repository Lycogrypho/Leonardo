package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0061 at the prompt: the factors bind by tuple assignment, the scalar answers print as
 *  numbers, and the four names are reserved while the slot word `full` is not.
 */
class SvdReplTest extends AnyFlatSpec:

  "the REPL" should "bind U, S, V := svd(A) and reconstruct A from them" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [3, 4], [5, 6]]")
    s.execute("U, S, V := svd(A)")
    assert(s.execute("U * S * transpose(V)") == s.execute("A"))
    s.execute("Uf, Sf, Vf := svd(A, full)")
    assert(s.execute("Uf * Sf * transpose(Vf)") == s.execute("A"))
    assert(s.execute("transpose(Uf) * Uf") == s.execute("eye(3)"))
  }

  it should "print rank, cond and pinv as values" in
  {
    val s = Session()
    assert(s.execute("rank([[1, 2], [2, 4]])") == s.execute("1"))
    assert(s.execute("cond([[1, 2], [2, 4]])") == "inf")
    assert(s.execute("pinv([[4, 7], [2, 6]])") == s.execute("inv([[4, 7], [2, 6]])"))
  }

  it should "reserve svd, pinv, rank and cond but leave full usable as a name" in
  {
    val s = Session()
    for name <- List("svd", "pinv", "rank", "cond") do
      assert(s.execute(s"$name := 3") == s"cannot assign to '$name': it is a reserved word")
    s.execute("full := 3")
    assert(s.execute("full + 1") == s.execute("4"))
  }

  it should "append the reason when a real matrix is refused, and nothing for a symbolic one" in
  {
    val s = Session()
    val big = s.execute("svd(ones(1001, 100))")
    assert(big.startsWith("svd(ones(1001.0, 100.0))") && big.contains("note: svd declined: the matrix is 1001x100"), big)
    assert(s.execute("rank([[1, 0], [0, 1]], 2)").contains("note: rank declined: the tolerance must be a number in [0, 1)"))
    assert(s.execute("cond(ones(1001, 100))").contains("note: cond declined"))
    assert(s.execute("pinv(ones(1001, 100))").contains("note: pinv declined"))
    assert(!s.execute("svd([[x, 1], [0, 1]])").contains("note"))
  }

  it should "round-trip a session holding the factors through script and load" in
  {
    // Only the displayed factors are compared: a dense matrix binding is saved at display
    // precision today, so a reconstruction from reloaded U and V is off in the fifth decimal
    // -- a defect of :save for every matrix value, filed as F_0088, not of svd.
    val s = Session()
    s.execute("U, S, V := svd([[3, 0], [4, 5]])")
    val t = Session()
    t.load(s.script)
    for name <- List("U", "S", "V") do assert(t.execute(name) == s.execute(name), name)
  }
