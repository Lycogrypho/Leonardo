package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0170 at the prompt: `rationalize`, its decline note, and the note offering it on a result
 *  that looks rational.  In the repl module because it constructs a `Session`. */
class RationalizeReplTest extends AnyFlatSpec:

  private def exactSession: Session = { val s = new Session(); s.execute("exact on"); s }

  "rationalize" should "answer an exact value, and say why it declines" in {
    val s = new Session()
    assert(s.execute("rationalize(0.25)") == "1/4")
    val declined = s.execute("rationalize(1 + i)")
    assert(declined.contains("note: rationalize declined:") && declined.contains("complex"), declined)
  }

  "the note" should "offer rationalize on decimals that look exact, in exact mode" in {
    // qr demotes an exact matrix to Double: its 3/5 comes back as 0.6 (lu is exact since F_0226)
    val out = exactSession.execute("qr([[3, 0], [4, 5]])")
    assert(out.contains("note:") && out.contains("rationalize") && out.contains("3/5"), out)
  }

  it should "not offer it on an irrational, outside exact mode, or on an exact answer" in {
    assert(!exactSession.execute("norm([[2], [3]])").contains("rationalize"))
    assert(!new Session().execute("qr([[3, 0], [4, 5]])").contains("rationalize"))
    assert(!exactSession.execute("lu([[1, 2], [3, 4]])").contains("note:"))
    assert(!exactSession.execute("1/3 + 1/6").contains("note:"))
    assert(!exactSession.execute("rationalize(0.5)").contains("note:"))
    // a solve's answer binds, and the offer would wrap the solve and bind again
    val s = exactSession
    s.execute("L, U, P := lu([[1, 2], [3, 4]])")
    assert(!s.execute("solve(L * c = P * [[1], [2]], c)").contains("rationalize"))
  }

  it should "offer exactly what the call it names gives" in {
    // eig and pinv answer decimals a few ulps off small fractions: at fifteen digits
    // rationalize kept those ulps, so the bare call's default digits are the ones that give the
    // small fraction since F_0240, and the note names no digits.
    for input <- Seq("eigen([[4, 1], [2, 3]])", "pinv([[1, 1], [1, 1]])", "qr([[3, 0], [4, 5]])") do
      val s   = exactSession
      val out = s.execute(input)
      val Offer = """(?s).*note: the decimals look exact; if they are, (rationalize\(\.\.\.\)) gives (.*)""".r
      out match
        case Offer(call, shown) =>
          assert(s.execute(call.replace("...", input)) == shown, s"$input: $call")
          assert(!shown.contains('.'), s"$input offered a decimal: $shown")
        case _ => fail(s"$input offered nothing: $out")
  }

  it should "keep the LaTeX channel empty, as every note does (F_0020)" in {
    val s = exactSession
    s.execute("latex on")
    s.execute("qr([[3, 0], [4, 5]])")
    assert(s.lastLatex.isEmpty)
  }
