package it.grypho.scala.leonardo
package optimize

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0063 at the prompt: a fit answers with its iterations and residual in a note, `cov` binds the
 *  covariance by tuple assignment, and a refusal says why. */
class LsqFitReplTest extends AnyFlatSpec:

  private def session(): Session =
    val s = Session()
    s.execute("T := transpose(tabulate(0.5*k, k, 0, 20))")
    s.execute("Y := transpose(tabulate(2*exp(-0.25*k), k, 0, 20))")
    s

  "the REPL" should "fit a curve and say how" in
  {
    val s   = session()
    val out = s.execute("lsqfit(a*exp(b*t), t, T, Y, [[a], [b]], [[1], [0]])")
    assert(out.startsWith("[[2.0], [-0.5]]") && out.contains("note: lsqfit:") && out.contains("iterations"), out)
  }

  it should "bind the parameters and the covariance by tuple assignment" in
  {
    val s = session()
    s.execute("Z := transpose(tabulate(1 + 2*0.5*k + 0.1*sin(3.5*k), k, 0, 20))")
    s.execute("p, C := lsqfit(c0 + c1*t, t, T, Z, [[c0], [c1]], [[0], [0]], cov)")
    assert(s.execute("at(p, 2, 1)").startsWith("2.0"), s.execute("p"))
    assert(s.execute("at(C, 1, 2) == at(C, 2, 1)") == "true", s.execute("C"))
  }

  it should "say why a fit was declined" in
  {
    val out = session().execute("lsqfit((a + b)*t, t, T, Y, [[a], [b]], [[1], [1]])")
    assert(out.contains("note: lsqfit declined:") && out.contains("identif"), out)
  }
