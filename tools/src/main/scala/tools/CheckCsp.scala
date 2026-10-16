package it.grypho.scala.leonardo
package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.security.MessageDigest
import java.util.Base64

/** Fails if the browser page's Content-Security-Policy and its inline scripts disagree (F_0112).
 *
 *  `web/index.html` carries a policy whose `script-src` allows the page's own origin and its
 *  inline scripts **by hash** (Decision A, option 2). A hash covers a script's exact bytes, so
 *  any edit to an inline script makes the browser refuse it, and the page loads, draws its
 *  chrome and does nothing. No Scala suite can see that, because none applies a policy; this
 *  guard makes the hashes a checked fact rather than something to remember.
 *
 *  It flags:
 *  - a page with no policy, or with no `script-src`;
 *  - `'unsafe-inline'` or `'unsafe-eval'` in `script-src`, which would give up what the policy
 *    is for (the owner's measurement found neither needed);
 *  - an inline script whose hash is not listed, naming the hash to add;
 *  - a listed hash no inline script has, left over from an edit;
 *  - **an inline script that spans lines**: its bytes then depend on the checkout, since
 *    `core.autocrlf` turns LF into CRLF on Windows and changes the hash with it;
 *  - an inline event handler (`onclick=` and the like), which the policy blocks without a word.
 *
 *  Inline *styles* are allowed by `'unsafe-inline'` in `style-src` and are not checked: Plotly
 *  inserts a `<style>` at load and MathLive positions every piece of a formula with a `style`
 *  attribute, so no hash could cover them, and a hash in `style-src` would disable
 *  `'unsafe-inline'`.
 */
object CheckCsp:

  private val DefaultPage = Paths.get("web", "index.html")

  /** HTML comments, removed first: the page's comments mention `<script src>` in prose. */
  private val Comment = """(?s)<!--.*?-->""".r

  private val Policy = """<meta\s+http-equiv="Content-Security-Policy"\s+content="([^"]*)"""".r

  /** A `<script>` with no `src` attribute, and its body. */
  private val InlineScript = """(?s)<script(?![^>]*\bsrc\s*=)[^>]*>(.*?)</script>""".r

  private val Tag     = """<[a-zA-Z][^>]*>""".r
  private val Handler = """\s(on[a-z]+)\s*=""".r

  /** The CSP source expression allowing `script`: `'sha256-…'` over its UTF-8 bytes.
   *
   *  @param script an inline script's exact text
   *  @return the quoted hash source
   */
  private def hashSource(script: String): String =
    val digest = MessageDigest.getInstance("SHA-256").digest(script.getBytes(StandardCharsets.UTF_8))
    s"'sha256-${Base64.getEncoder.encodeToString(digest)}'"

  /** Every disagreement between `page`'s policy and its inline scripts and handlers.
   *
   *  @param page the HTML file to check
   *  @return one finding per problem, empty when the page is consistent
   */
  def offences(page: Path): Vector[String] =
    val html = Comment.replaceAllIn(Files.read(page), "")
    Policy.findFirstMatchIn(html).map(_.group(1)) match
      case None => Vector(s"$page: no Content-Security-Policy meta element")
      case Some(policy) =>
        val directives = policy.split(';').toVector.map(_.trim.split("\\s+").toVector).filter(_.nonEmpty)
        directives.find(_.head == "script-src").map(_.tail) match
          case None => Vector(s"$page: the policy has no script-src")
          case Some(sources) =>
            val scripts = InlineScript.findAllMatchIn(html).map(_.group(1)).toVector
            val hashes  = scripts.map(hashSource)
            val listed  = sources.filter(_.startsWith("'sha256-"))
            val unsafe  = sources.filter(s => s == "'unsafe-inline'" || s == "'unsafe-eval'")
              .map(s => s"$page: script-src allows $s, which the policy exists to exclude")
            val spans = scripts.filter(s => s.contains('\n') || s.contains('\r'))
              .map(s => s"$page: an inline script spans lines; keep it on one line, so its hash does not depend on the checkout's line endings: ${s.trim.take(60)}")
            val missing = scripts.zip(hashes).collect { case (s, h) if !listed.contains(h) =>
              s"$page: inline script not allowed by script-src, add $h: ${s.trim.take(60)}" }
            val stale = listed.filterNot(hashes.contains).map(h => s"$page: script-src lists $h, a stale hash no inline script has")
            val withoutScripts = InlineScript.replaceAllIn(html, "")
            val handlers = Tag.findAllIn(withoutScripts).flatMap(t => Handler.findAllMatchIn(t).map(_.group(1))).toVector
              .map(h => s"$page: inline event handler $h=, which the policy blocks; attach it from main.js")
            unsafe ++ spans ++ missing ++ stale ++ handlers

  /** The check, as an exit code, over the given page or `web/index.html`.
   *
   *  @param args one HTML file; empty means the default
   *  @return 1 when anything disagrees, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val page = args.headOption.map(Paths.get(_)).getOrElse(DefaultPage)
    Files.report(
      offences(page),
      n => s"\n$page: $n Content-Security-Policy problem(s)",
      "An inline script is allowed by its SHA-256 hash (F_0112): after editing one, replace its\n" +
      "hash in the policy with the one named above.")
