package it.grypho.scala.leonardo
package optimize

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0066 through the REPL: `nlp` answers a column with a note naming what binds, explains a
 *  decline, and survives `:save` / `:load`. */
class NlpReplTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "nlp" should "answer T3-02's acceptance as a column, with the active inequality and its multiplier" in
  {
    val s   = session
    val out = s.execute("nlp(x + y, [[x], [y]], [[1], [0]], x^2 + y^2 <= 2, 0, sqp)")
    val lines = out.split("\n").toList
    assert(lines.head == "[[-1.0], [-1.0]]", out)
    assert(lines.exists(l => l.contains("note: nlp: a KKT point after") &&
                             l.contains("active: inequality 1 (multiplier 0.5)")), out)
    assert(s.lastLatex.isEmpty, "the note is prose the formula does not carry (F_0020)")
  }

  it should "name a variable a bound holds" in
  {
    val out = session.execute("nlp((1 - x)^2 + 100*(y - x^2)^2, [[x], [y]], [[-1.2], [1]], 0, 0, -inf, [[0.5], [inf]], sqp)")
    assert(out.contains("no inequality is active; x held at a bound"), out)
  }

  it should "explain a decline" in
  {
    val out = session.execute("nlp(x^2, [[x]], 0, [[x + 1], [1 - x]], 0, sqp)")
    assert(out.startsWith("nlp("), out)
    assert(out.contains("note: nlp declined: no feasible point was found"), out)
  }

  it should "round-trip through :save and :load" in
  {
    val s = session
    s.execute("p := nlp(x^2 + 2*y^2, [[x], [y]], [[0], [0]], 0, x + y = 3, auglag)")
    val t = session
    t.load(s.script)
    assert(t.execute("p").startsWith("[[2.0], [1.0]]"), t.execute("p"))
  }
