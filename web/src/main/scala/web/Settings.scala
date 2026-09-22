package it.grypho.scala.leonardo
package web

import core.LogicSemantics

/** The settings panel, minus the browser (issue F_0016 step 6).
 *
 *  **Everything here is decidable without a DOM** — which control a setting gets, what its
 *  options are, what command a new value becomes — so it is unit-tested, and [[App]] is left
 *  with element creation only.  The same split [[PlotSpec]] / [[Plot]] and
 *  `latex.ToLatex` / [[MathRender]] make, for the same reason.
 *
 *  **The panel owns no state.**  `cli.Session.settings` is read before every render and a
 *  changed control is written back through `cli.Session.step` as the command a user would
 *  have typed — so the controls and the prompt cannot go out of step, and typing `latex on`
 *  moves the checkbox.  That is the one design point this step had: a panel holding its own
 *  copy of a toggle would be a second source of truth for a setting the command language
 *  already owns.
 */
object Settings:

  /** How a setting is shown. */
  enum Widget:
    /** `on` / `off` — a checkbox. */
    case Toggle
    /** A whole number at or above `min` — a number box. */
    case Number(min: Int)
    /** One of a fixed set of words — a drop-down. */
    case Choice(options: List[String])

  /** One control: the setting it edits, the words on the page, and its widget.
   *
   *  @param setting the `Session.settings` key, which is also the command word
   *  @param label   what the user reads
   *  @param widget  how the value is shown and edited
   */
  final case class Control(setting: String, label: String, widget: Widget)

  /** The t-norm names, read off the enum rather than written out again.
   *
   *  Two hand-maintained lists of one set is how they drift — the fact `ColorSchemeNamesTest`
   *  exists to pin.  Here the second list simply does not exist; the spelling is the enum
   *  case in lower case, which is what `Session` parses back.
   */
  private val SemanticsNames: List[String] =
    LogicSemantics.values.toList.map(_.toString.toLowerCase)

  /** Every setting the panel knows how to show, keyed by its `Session.settings` name.
   *
   *  **A lookup, not an ordering**: the panel walks `Session.settings` and consults this, so
   *  the controls appear in the session's own order and there is no second list to keep in
   *  step with it.  A setting with no entry is simply not offered — which today is exactly
   *  `colors`, because the browser page has no syntax highlighting, and a control that
   *  changes nothing visible is worse than an absent one.
   */
  val Known: Map[String, Control] = List(
    Control("precision",       "digits",     Widget.Number(0)),
    Control("pretty",          "pretty",     Widget.Toggle),
    Control("latex",           "LaTeX",      Widget.Toggle),
    Control("names",           "new names",  Widget.Toggle),
    Control("logic",           "t-norm",     Widget.Choice(SemanticsNames)),
    Control("logic symmetric", "−1 / 0 / 1", Widget.Toggle),
    Control("exact precision", "exact digits", Widget.Number(1)),
    Control("exact",           "exact",      Widget.Toggle)
  ).map(c => c.setting -> c).toMap

  /** The controls to render, in the session's order, for the settings it reports.
   *
   *  @param settings the session's `(command, argument)` pairs
   *  @return each showable setting with its control and its current value
   */
  def panel(settings: List[(String, String)]): List[(Control, String)] =
    settings.flatMap((k, v) => Known.get(k).map(_ -> v))

  /** The command that sets `setting` to `value` — which is the command a user would type.
   *
   *  @param setting the setting name
   *  @param value   its new value
   *  @return the line to hand to `cli.Session.step`
   */
  def command(setting: String, value: String): String = s"$setting $value"

  /** The value a checkbox stands for. */
  def toggleValue(checked: Boolean): String = if checked then "on" else "off"

  /** Whether a toggle's current value reads as set. */
  def isOn(value: String): Boolean = value == "on"

  /** The element id for a setting's control, so a harness can find it and a label can name it.
   *
   *  Spaces become hyphens because `logic symmetric` is two words and an id may not carry one.
   *
   *  @param setting the setting name
   *  @return a stable element id
   */
  def id(setting: String): String = "set-" + setting.replace(' ', '-')
