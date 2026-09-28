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
