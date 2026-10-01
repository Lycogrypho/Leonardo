package it.grypho.scala.leonardo
package cli

/** The URL-fragment format a session travels in, shared by every producer of one (issue F_0045).
 *
 *  **It lives here, in the module that owns `Session.script`, because two components need the
 *  same answer and neither can reach the other.**  `web.ShareLink` builds the fragment in the
 *  browser; `docs.Lesson` builds one on the JVM to put a "run this" link beside an executed
 *  lesson transcript.  `web` is a Scala.js project and the documentation helper is a JVM one, so
 *  the only place both can import from is the cross-built `repl` module — and a second
 *  hand-maintained encoder is exactly the drift `ColorSchemeNamesTest` exists to prevent
 *  elsewhere.
 *
 *  **`encodeComponent` is `encodeURIComponent`, re-implemented rather than delegated, and that is
 *  the substance of this object.**  The browser has the function as a global and the JDK does
 *  not: `java.net.URLEncoder.encode` is **not** the same thing — it is
 *  `application/x-www-form-urlencoded`, which emits `+` for a space, and `decodeURIComponent`
 *  returns that `+` as a literal plus sign.  A script encoded that way reloads as a *valid but
 *  different* session, every space in it silently corrupted, which is the confidently-wrong
 *  failure this project declines everywhere.
 *
 *  So the unreserved set is spelled out and everything else is percent-encoded from its UTF-8
 *  bytes.  **UTF-8 is hand-rolled from code points rather than taken from
 *  `String.getBytes(UTF_8)`**: this file compiles for both platforms, and a charset's breadth of
 *  support is precisely the kind of difference that compiles everywhere and diverges at run time
 *  — the `DoubleRender` lesson.  Hex digits are upper case, as the browser's own function emits
 *  them, so a fragment built here is byte-identical to one built by the page.
 *
 *  Decoding is **not** here.  `decodeURIComponent` is only ever needed in the browser, where it
 *  exists and is the authority; re-implementing it would add a second thing to keep in step for
 *  no caller.  `ShareLinkTest` checks the two agree by decoding this encoder's output with the
 *  real one.
 */
private[leonardo] object ShareEncoding:

  /** The fragment key, so the encoding can gain neighbours without becoming ambiguous. */
  val Key: String = "s="

  /** Browsers do not agree on a URL length limit and the practical floor is around 64k, so a long
   *  session is refused rather than truncated: half a script would `:load` as a valid but
   *  different session, which is worse than a link that plainly says it is too big.
   */
  val MaxFragment: Int = 32000

  /** The characters `encodeURIComponent` leaves alone. */
  private val Unreserved: Set[Char] =
    (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).toSet ++ Set('-', '_', '.', '!', '~', '*', '\'', '(', ')')

  private val HexDigits = "0123456789ABCDEF"

  /** Percent-encodes `s` exactly as JavaScript's `encodeURIComponent` does.
   *
   *  @param s the text to encode
   *  @return the encoded text, safe to carry in a URL fragment
   */
  def encodeComponent(s: String): String =
    val out = new StringBuilder(s.length + 16)
    var i   = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      i += Character.charCount(cp)
      if cp < 0x80 && Unreserved.contains(cp.toChar) then out.append(cp.toChar)
      else utf8Bytes(cp).foreach(appendEscaped(out, _))
    out.toString

  /** The fragment for a session script, or a message when it is too long to carry.
   *
   *  @param script the session script, from `Session.script`
   *  @return the fragment text including its key, or why it could not be built
   */
  def fragmentFor(script: String): Either[String, String] =
    val encoded = encodeComponent(script)
    if encoded.length > MaxFragment then
      Left(s"session is too large to share as a link (${encoded.length} of $MaxFragment characters)")
    else Right(Key + encoded)

  /** Appends one byte as `%XX`, upper case. */
  private def appendEscaped(out: StringBuilder, b: Int): Unit =
    out.append('%').append(HexDigits((b >> 4) & 0xF)).append(HexDigits(b & 0xF))

  /** The UTF-8 bytes of one code point, by the standard four-case encoding. */
  private def utf8Bytes(cp: Int): Seq[Int] =
    if cp < 0x80 then Seq(cp)
    else if cp < 0x800 then Seq(0xC0 | (cp >> 6), 0x80 | (cp & 0x3F))
    else if cp < 0x10000 then Seq(0xE0 | (cp >> 12), 0x80 | ((cp >> 6) & 0x3F), 0x80 | (cp & 0x3F))
    else Seq(0xF0 | (cp >> 18), 0x80 | ((cp >> 12) & 0x3F), 0x80 | ((cp >> 6) & 0x3F), 0x80 | (cp & 0x3F))
