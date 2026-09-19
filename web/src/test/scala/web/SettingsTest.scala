package it.grypho.scala.leonardo
package web

import org.scalatest.flatspec.AnyFlatSpec

import cli.Session
import core.LogicSemantics

/** Issue F_0016 step 6 — the settings panel's decidable half.
 *
 *  The load-bearing case is the first one: **every control must name a setting the session
 *  actually reports**.  A panel and a `:save` script are two readings of one state, and the
 *  way they go wrong is silently — a control for a setting that no longer exists renders
 *  fine and does nothing.
 */
class SettingsTest extends AnyFlatSpec:

  private def settings = new Session().settings

  "every known control" should "name a setting the session reports" in
  {
    val reported = settings.map(_._1).toSet
    val unknown  = Settings.Known.keys.filterNot(reported.contains)
    assert(unknown.isEmpty, s"controls for settings the session does not report: $unknown")
  }

  "the panel" should "follow the session's own order" in
  {
    val shown = Settings.panel(settings).map(_._1.setting)
    assert(shown == settings.map(_._1).filter(Settings.Known.contains))
  }

  it should "skip a setting it has no widget for, rather than inventing one" in
  {
    // `colors` is reported by the session and deliberately not offered: the page has no
    // syntax highlighting, so the control would change nothing visible.
    assert(settings.exists(_._1 == "colors"))
    assert(!Settings.panel(settings).exists(_._1.setting == "colors"))
  }

  it should "carry each setting's current value" in
  {
    val s = new Session()
    s.execute("latex on")
    s.execute("precision 9")
    val shown = Settings.panel(s.settings).map((c, v) => c.setting -> v).toMap
    assert(shown("latex") == "on")
    assert(shown("precision") == "9")
  }

  "a control's command" should "be the line a user would have typed" in
  {
    val s = new Session()
    s.execute(Settings.command("latex", Settings.toggleValue(true)))
    assert(s.execute("latex") == "latex = on")
    s.execute(Settings.command("logic", "product"))
    assert(s.execute("logic").contains("product"))
  }

  "the t-norm choices" should "be the enum's own cases, not a second list" in
  {
    Settings.Known("logic").widget match
      case Settings.Widget.Choice(options) =>
        assert(options == LogicSemantics.values.toList.map(_.toString.toLowerCase))
      case other => fail(s"logic should be a choice, got $other")
  }

  "every choice option" should "be accepted by the session" in
  {
    Settings.Known.values.foreach { c =>
      c.widget match
        case Settings.Widget.Choice(options) =>
          options.foreach { o =>
            val s   = new Session()
            val out = s.execute(Settings.command(c.setting, o))
            assert(!out.contains("unknown"), s"${c.setting} refused '$o': $out")
          }
        case Settings.Widget.Toggle =>
          List("on", "off").foreach { o =>
            val s   = new Session()
            val out = s.execute(Settings.command(c.setting, o))
            assert(out.contains(o), s"${c.setting} refused '$o': $out")
          }
        case Settings.Widget.Number(min) =>
          val s   = new Session()
          val out = s.execute(Settings.command(c.setting, min.toString))
          assert(!out.contains("expects"), s"${c.setting} refused its own minimum $min: $out")
    }
  }

  "an element id" should "be usable in HTML" in
  {
    assert(Settings.id("logic symmetric") == "set-logic-symmetric")
    Settings.Known.keys.foreach(k => assert(!Settings.id(k).contains(' ')))
  }
