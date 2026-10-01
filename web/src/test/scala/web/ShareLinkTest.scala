package it.grypho.scala.leonardo
package web

import org.scalatest.flatspec.AnyFlatSpec

import scala.scalajs.js

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

  // --- F_0045: the shared encoder must agree with the browser's own decoder ---------------
  // `cli.ShareEncoding.encodeComponent` re-implements `encodeURIComponent` so the JVM-side
  // documentation helper can build the same fragment. These cases are the agreement check, and
  // they run on Node where the real `decodeURIComponent` is available -- which is the only place
  // the two CAN be compared, and the reason the encoder lives in a cross-built module.

  "the shared encoder" should "produce what the browser's decodeURIComponent reads back" in
  {
    // The space and the newline are the ones that matter: `java.net.URLEncoder` would emit `+`
    // for a space, which `decodeURIComponent` returns as a literal plus.
    val awkward = List(
      "x := 1.0\nf := sin(x) + x^2",
      "a b c",
      "100% of 1+1 = 2 & more",
      "m := [[1, 2], [3, 4]]",
      "q := 1/3 # comment?",
      "theta := pi/2",
      "unicode: θ − é 😀"   // BMP, Latin-1 and a surrogate pair
    )
    for s <- awkward do
      val fragment = ShareLink.encode(s).getOrElse(fail(s"expected a fragment for: $s"))
      assert(ShareLink.decode(fragment).contains(s), s"round trip changed: $s")
  }

  it should "escape exactly the characters the browser escapes" in
  {
    // Byte-identical to the global, so a fragment built on the JVM and one built in the page are
    // the same string -- not merely both decodable.
    for s <- List("x := 1.0\nf := sin(x)", "a b", "~!*'()-_.", "100%&=?#/:;,+$", "θ") do
      assert(cli.ShareEncoding.encodeComponent(s) ==
               js.Dynamic.global.encodeURIComponent(s).asInstanceOf[String],
             s"diverged from encodeURIComponent on: $s")
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
