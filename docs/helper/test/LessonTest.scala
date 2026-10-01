package it.grypho.scala.leonardo
package docs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the lesson transcript helper (issue D_0003).
 *
 *  The property worth protecting is that the transcript is **executed, not transcribed**: every
 *  assertion below names an answer the helper could only produce by running the session, so a
 *  regression that started echoing its input would fail here rather than ship a lesson full of
 *  plausible fiction.
 */
class LessonTest extends AnyFlatSpec with Matchers:

  "render" should "fence the block as text rather than scala" in {
    val out = Lesson().render(Seq("1 + 1"))
    out should startWith("```text\n")
    out should endWith("\n```")
  }

  it should "prefix each command with the prompt" in {
    Lesson().render(Seq("1 + 1")) should include("> 1 + 1")
  }

  it should "carry the computed answer, not the input" in {
    // The answer is the library's, so this fails if the helper ever stops executing.
    Lesson().render(Seq("derive(sin(x), x)")) should include("cos(x)")
  }

  it should "keep session state across lines, so a lesson builds up" in {
    val out = Lesson().render(Seq("x := 3", "x + 1"))
    out should include("4")
  }

  it should "keep session state across separate calls, as mdoc blocks do" in {
    val lesson = Lesson()
    lesson.render(Seq("y := 7"))
    lesson.render(Seq("y + 1")) should include("8")
  }

  it should "preserve every line of a multi-line answer" in {
    // A table is the case that would break a one-line-per-command assumption.
    val out   = Lesson().render(Seq("truth a and b"))
    val lines = out.linesIterator.toSeq
    lines.count(_.nonEmpty) should be > 4
  }

  it should "not leave a blank line under a command with no output" in {
    // `render` drops an empty answer entirely; a blank line would read as an empty result.
    val out = Lesson().render(Seq("pretty off", "1 + 1"))
    out should not include "\n\n"
  }

  // --- F_0045: the run link ------------------------------------------------------------------

  "page" should "put the run link under the block, outside the fence" in {
    val out = Lesson().page(Seq("x := 3"))
    out should include("```text\n")
    // Outside, or the reader would see the markdown source of a link instead of a link.
    out.indexOf("run this session") should be > out.lastIndexOf("```")
    out should include(Lesson.AppUrl)
  }

  "the run link" should "carry the session as it stands, not as it started" in {
    val lesson = Lesson()
    lesson.render(Seq("x := 3", "y := 4"))
    val fragment = lesson.runLink.getOrElse(fail("expected a link"))
    // `%3A%3D` is `:=` encoded; both names must be in the carried script.
    fragment should include("x%20%3A%3D%203.0")
    fragment should include("y%20%3A%3D%204.0")
  }

  it should "RELOAD to the same session, which is the only property that matters" in {
    // The guard the entry asked for: a link that fails to replay is worse than no link. Decoding
    // here is by hand rather than through `web.ShareLink`, which is Scala.js and unreachable from
    // this JVM suite -- `ShareLinkTest` is what proves the encoder agrees with the browser's
    // decoder, so between the two suites the whole path is covered.
    val sender = Lesson()
    sender.render(Seq("precision 7", "x := 3.5", "f := sin(x) + x^2"))
    val link = sender.runLink.getOrElse(fail("expected a link"))

    val fragment = link.substring(link.indexOf("#s=") + 3, link.lastIndexOf(')'))
    // `URLDecoder` would read a literal `+` as a space, which is the very asymmetry F_0045 is
    // about — but it is safe here precisely because the encoder escapes `+` as `%2B`, so no
    // literal one can reach this line. (A test asserting that is in ShareLinkTest.)
    val script = java.net.URLDecoder.decode(fragment, "UTF-8")

    val receiver = cli.Session()
    receiver.load(script)
    // `script` is the canonical statement of session state, so one assertion covers precision,
    // bindings, definitions and every toggle.
    receiver.script shouldBe sender.sessionScript
  }

  it should "be omitted rather than truncated when the session is too large" in {
    val lesson = Lesson()
    // Past ShareEncoding.MaxFragment: half a script would load as a valid but different session.
    for i <- 1 to 1200 do lesson.render(Seq(s"name_this_a_rather_long_identifier_$i := $i"))
    lesson.runLink shouldBe None
  }
