// A package OUTSIDE it.grypho.scala.leonardo on purpose: this suite compiling is the proof that
// the encoder is reachable from a consumer, which is what F_0236 made it.
package it.grypho.consumer

import it.grypho.scala.leonardo.cli.{Session, ShareEncoding}
import org.scalatest.flatspec.AnyFlatSpec

/** F_0236: `cli.ShareEncoding` is published API, and its round trip is pinned on the JVM.
 *
 *  `java.net.URLDecoder` is form-decoding (it reads `+` as a space), which is safe as the
 *  inverse here only because the encoder escapes `+` as `%2B`: the first case pins that. */
class ShareEncodingTest extends AnyFlatSpec:

  private def decode(s: String): String = java.net.URLDecoder.decode(s, "UTF-8")

  "encodeComponent" should "escape a plus and a space as encodeURIComponent does" in {
    assert(ShareEncoding.encodeComponent("a + b") == "a%20%2B%20b")
  }

  it should "leave the unreserved set alone and write hex in upper case" in {
    assert(ShareEncoding.encodeComponent("Az09-_.!~*'()") == "Az09-_.!~*'()")
    assert(ShareEncoding.encodeComponent("é") == "%C3%A9")
  }

  it should "round-trip any text, astral code points included" in {
    val corpus = List("x := [[1, 2], [3, 4]]", "f := sin(θ) + 2^-x", "% # & ? = / \\ \n\t", "𝔸 ∫ ∑ ≤")
    corpus.foreach(s => assert(decode(ShareEncoding.encodeComponent(s)) == s, s))
  }

  "fragmentFor" should "carry the key and reproduce the session it was built from" in {
    val sender = new Session()
    sender.execute("x := 3")
    sender.execute("f := x^2 + 1")
    ShareEncoding.fragmentFor(sender.script) match
      case Left(why) => fail(why)
      case Right(fragment) =>
        assert(fragment.startsWith(ShareEncoding.Key))
        val receiver = new Session()
        receiver.load(decode(fragment.drop(ShareEncoding.Key.length)))
        assert(receiver.script == sender.script)
  }

  it should "refuse rather than truncate a session past MaxFragment" in {
    assert(ShareEncoding.fragmentFor("x" * (ShareEncoding.MaxFragment + 1)).isLeft)
    assert(ShareEncoding.fragmentFor("x" * ShareEncoding.MaxFragment).isRight)
  }
