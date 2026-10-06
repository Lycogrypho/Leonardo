package it.grypho.scala.leonardo
package probability

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0050 through the REPL: display, `:save`/`:load` of the new families and of a posterior
 *  definition, and the exact `bayes` row.  Lives in the repl module because it constructs a
 *  `Session`. */
class BayesReplTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "a conjugate family" should "bind, display and survive :save / :load" in
  {
    val s1 = session
    assert(s1.execute("P := betadist(2, 3)").startsWith("P :="))
    assert(s1.execute("P") == "betadist(2.0, 3.0)")
    s1.execute("G := gammadist(2, 3)")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("P") == "betadist(2.0, 3.0)")
    assert(s2.execute("G") == "gammadist(2.0, 3.0)")
  }

  "posterior" should "answer as an ordinary distribution" in
  {
    val s = session
    assert(s.execute("posterior(betadist(2, 2), binomial(10, p), 7)") == "betadist(9.0, 5.0)")
    assert(s.execute("posterior(gammadist(2, 1), poisson(l), [[3, 5, 4]])") == "gammadist(14.0, 4.0)")
  }

  it should "work as a saved definition, since its likelihood carries a free name" in
  {
    val s1 = session
    s1.execute("post := posterior(betadist(2, 2), binomial(10, p), 7)")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("expect(post)") == "0.64286")             // 9/14
    assert(s2.execute("expect(post)") == s1.execute("expect(post)"))
  }

  "bayes" should "print a row, and an exact row in exact mode" in
  {
    val s = session
    assert(s.execute("bayes([[0.5, 0.5]], [[0.8, 0.2]])") == "[[0.8, 0.2]]")
    s.execute("exact on")
    assert(s.execute("bayes([[1/2, 1/2]], [[4/5, 1/5]])") == "[[4/5, 1/5]]")
  }
