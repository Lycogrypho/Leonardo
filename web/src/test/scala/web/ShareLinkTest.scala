package it.grypho.scala.leonardo
package web

import org.scalatest.flatspec.AnyFlatSpec

import cli.Session

/** Tests for the shareable-link codec (F_0003 phase 3).
 *
 *  The round trip that matters is not string-to-string but **session-to-session**: a link is
 *  worth having only if opening it reproduces the sender's bindings, so the last case drives
 *  a real `Session` through `script` → `encode` → `decode` → `load` and compares state.
 */
class ShareLinkTest extends AnyFlatSpec:

  "a script" should "survive the round trip unchanged" in
  {
    val script = "precision 5\nx := 3.0\nf := (sin(x) + x)"
    ShareLink.encode(script) match
      case Left(why)       => fail(s"expected a fragment, got: $why")
      case Right(fragment) => assert(ShareLink.decode(fragment).contains(script))
  }

  it should "survive characters a URL would otherwise claim" in
  {
    // `#` ends a fragment, `&` and `=` separate parameters, `+` reads as a space in a query.
    // A session script contains `=` in every equation and `+` in most expressions, so this is
    // the ordinary case rather than an edge one.
    val script = "h := x^2 + 1 = 4\ng := a and b\nc := 100%"
    ShareLink.encode(script) match
      case Left(why)       => fail(s"expected a fragment, got: $why")
      case Right(fragment) => assert(ShareLink.decode(fragment).contains(script))
  }

  "decode" should "accept a fragment with or without its leading hash" in
  {
    ShareLink.encode("x := 1.0") match
      case Left(why)       => fail(why)
      case Right(fragment) =>
        assert(ShareLink.decode(fragment) == ShareLink.decode("#" + fragment))
  }

  it should "return None for a fragment that carries no session" in
  {
    assert(ShareLink.decode("").isEmpty)
    assert(ShareLink.decode("#").isEmpty)
    assert(ShareLink.decode("#section-3").isEmpty, "an ordinary anchor must not look like a session")
    assert(ShareLink.decode("#s=").isEmpty, "an empty session is not a session")
  }

  it should "survive a malformed fragment rather than throwing" in
  {
    // A truncated or hand-edited link makes decodeURIComponent raise a URIError. The page must
    // still start: a bad link is a missing session, never a blank screen.
    assert(ShareLink.decode("#s=%").isEmpty)
    assert(ShareLink.decode("#s=%E0%A4%A").isEmpty)
  }

  "an oversized session" should "be refused rather than truncated" in
  {
    // Half a script would `:load` as a VALID but different session -- silently the wrong
    // answer, which is worse than a link that says plainly that it is too big.
    val huge = "x := 1.0\n" * ShareLink.MaxFragment
    assert(ShareLink.encode(huge).isLeft)
  }

  "a shared link" should "reproduce the sender's session" in
  {
    val sender = new Session()
    sender.execute("precision 7")
    sender.execute("x := 3.5")
    sender.execute("f := sin(x) + x^2")

    val fragment = ShareLink.encode(sender.script).getOrElse(fail("expected a fragment"))
    val script   = ShareLink.decode(fragment).getOrElse(fail("expected a session"))

    val receiver = new Session()
    receiver.load(script)

    // `script` is the canonical statement of session state, so comparing the two scripts
    // compares precision, bindings, definitions and every toggle in one assertion.
    assert(receiver.script == sender.script)
    assert(receiver.execute("f") == sender.execute("f"))
  }
