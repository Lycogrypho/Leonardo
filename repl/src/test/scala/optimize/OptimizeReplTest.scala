package it.grypho.scala.leonardo
package optimize

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0010 through the REPL: the classification note after `stationary`, the reason note after
 *  a declined `minimize` (F_0052 Decision A), and `:save` / `:load`.  Lives in the repl module
 *  because it constructs a `Session`. */
class OptimizeReplTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "stationary" should "list the points and classify them in a note" in
  {
    val s   = session
    val out = s.execute("stationary(x^3 - 3*x + y^2, x, y)")
    val lines = out.split("\n").toList
    assert(lines.head == "[[x = -1.0, y = 0.0], [x = 1.0, y = 0.0]]", out)
    assert(lines.exists(l => l.contains("note:") && l.contains("(x, y) = (-1.0, 0.0) is a saddle point") &&
                             l.contains("(x, y) = (1.0, 0.0) is a local minimum")), out)
    // the note is prose the formula does not carry, so no LaTeX is offered (F_0020)
    assert(s.lastLatex.isEmpty)
  }

  it should "call an extremum local, since the test is (F_0229)" in
  {
    // Unbounded below, so (1, 1) is no minimum of f; the second-order test reads only the point.
    val out = session.execute("stationary(2*x^3 - 6*x*y + 3*y^2, x, y)")
    assert(out.contains("(x, y) = (1.0, 1.0) is a local minimum"), out)
    assert(session.execute("stationary(-(x^2), x)").contains("is a local maximum"))
  }

  it should "say when the second-order test cannot decide" in
  {
    assert(session.execute("stationary(x^4, x)").contains("not decided by the second-order test"))
  }

  it should "not bind its answer, unlike solve" in
  {
    val s = session
    s.execute("stationary(x^2 - 4*x, x)")
    assert(s.execute("x") == "x")
  }

  "minimize" should "answer a column, and explain a decline" in
  {
    val s = session
    assert(s.execute("minimize((x - 1)^2 + (y + 2)^2, [[x], [y]], [[0], [0]], bfgs)") == "[[1.0], [-2.0]]")
    val declined = s.execute("minimize(x + y, [[x], [y]], [[0], [0]], bfgs)")
    assert(declined.contains("note: minimize declined: the objective appears unbounded below"), declined)
    val bounded = s.execute("minimize(x^2, [[x]], 1, 0, 2, newton)")
    assert(bounded.contains("newton takes no bounds; use pbfgs or gd"), bounded)
  }

  "qp" should "answer the minimiser and name the active constraints in a note (F_0053 Decision C)" in
  {
    val s   = session
    val out = s.execute("qp([[1, -1], [-1, 2]], [[-2], [-6]], [[1, 1], [-1, 2], [2, 1]], [[2], [2], [3]], 0, inf)")
    val lines = out.split("\n").toList
    assert(lines.head == "[[0.66667], [1.33333]]", out)
    assert(lines.exists(_.contains("note: active: rows 1 and 2 of A")), out)
    assert(s.lastLatex.isEmpty)
    assert(s.execute("qp(eye(2), [[0], [0]], 0, 0, [[1, 1]], [[1]], -inf, [[0.2], [inf]])")
             .contains("note: active: z1 at its upper bound"))
    assert(s.execute("qp(eye(2), [[-1], [-1]], 0, 0, -inf, inf)").contains("note: no constraint is active"))
  }

  it should "explain a decline" in
  {
    val s = session
    assert(s.execute("qp([[1, 0], [0, -1]], [[0], [0]], 0, 0, -inf, inf)")
             .contains("note: qp declined: H has a negative eigenvalue"))
    assert(s.execute("qp(eye(2), [[0], [0]], [[1, 0], [-1, 0]], [[-1], [-1]], -inf, inf)")
             .contains("note: qp declined: the constraints admit no point"))
    // F_0106: the semidefinite path's LP cap is named as such, in prose, not as an enum spelling.
    val lp = s.execute("qp(zeros(41), ones(41, 1), 0, 0, 0, inf)")
    assert(lp.contains("note: qp declined: a semidefinite H starts from an exact linear program"), lp)
    assert(lp.contains(s"at most $MaxLpVariables variables") && !lp.contains("InvalidInput"), lp)
  }

  "convex, lagrange and kkt" should "answer at the prompt" in
  {
    val s = session
    assert(s.execute("convex(x^2 + y^2, x, y)") == "true")
    assert(s.execute("lagrange(x + y, [[x^2 + y^2 - 2]], x, y)") == "[[x = -1.0, y = -1.0], [x = 1.0, y = 1.0]]")
    assert(s.execute("kkt(x^2 + y^2, 1 - x - y, 0, x, y)").contains("(mu0 >= 0"))
  }

  "an optimization definition" should "survive :save / :load" in
  {
    val s1 = session
    s1.execute("p := stationary(x^2 - a*x, x)")
    s1.execute("a := 6")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("p") == s1.execute("p"))
    assert(s2.execute("p").startsWith("[[x = 3.0]]"))
  }
