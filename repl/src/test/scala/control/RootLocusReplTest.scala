package it.grypho.scala.leonardo
package control

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** `Session.rootLocusPoints` (F_0140): the data behind the browser's `rlocus` command. */
class RootLocusReplTest extends AnyFlatSpec:

  "rootLocusPoints" should "sweep the gain band and carry the open-loop poles and zeros" in {
    Session().rootLocusPoints("(s + 3)/(s*(s + 2)) s 0.01 100 50") match
      case Right(l) =>
        assert(l.branches.size == 2 && l.branches.forall(_.size == 50))
        assert(l.poles.toSet == Set((0.0, 0.0), (-2.0, 0.0)))
        assert(l.zeros == Vector((-3.0, 0.0)))
      case Left(why) => fail(why)
  }

  it should "use the session's parameters" in {
    val session = Session()
    session.execute("a := 4")
    assert(session.rootLocusPoints("1/(s*(s + a)) s 0.1 10 20").exists(_.poles.toSet == Set((0.0, 0.0), (-4.0, 0.0))))
  }

  it should "explain an improper plant and a malformed command" in {
    assert(Session().rootLocusPoints("s^2/(s + 1) s 1 10").isLeft)
    assert(Session().rootLocusPoints("").left.exists(_.startsWith("usage:")))
  }
