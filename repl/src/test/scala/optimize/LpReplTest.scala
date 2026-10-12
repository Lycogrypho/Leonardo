package it.grypho.scala.leonardo
package optimize

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0011 through the REPL: `lp` answers a column with a note giving the optimal value and the
 *  binding constraints, says why it declined, and stays exact in exact mode. */
class LpReplTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "lp" should "answer a column with its optimal value and active constraints" in
  {
    val out = session.execute("lp([[-3], [-5]], [[1, 0], [0, 2], [3, 2]], [[4], [12], [18]], 0, inf)")
    val lines = out.split("\n").toList
    assert(lines.head == "[[2.0], [6.0]]", out)
    assert(lines.exists(l => l.contains("note: lp: optimal value -36.0") && l.contains("rows 2 and 3 of A")), out)
  }

  it should "say an infeasible problem is proved infeasible" in
  {
    val out = session.execute("lp(1, [[1], [-1]], [[1], [-2]], -inf, inf)")
    assert(out.contains("note: lp declined: no point satisfies the constraints (proved"), out)
  }

  it should "stay exact in exact mode" in
  {
    val s = session
    s.execute("exact on")
    assert(s.execute("lp([[-1], [-1]], [[3, 1], [1, 3]], [[1], [1]], 0, inf)").startsWith("[[1/4], [1/4]]"))
  }
