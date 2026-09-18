package it.grypho.scala.leonardo
package web

/** Builds the Plotly JSON a figure is drawn from — **pure text in, pure text out**.
 *
 *  **No Scala plotting dependency is adopted anywhere in Leonardo, and this file is why that
 *  is affordable** (F_0003 Decision 1).  Plotly's input is plain JSON: an array of *traces*
 *  carrying the data, plus a *layout* carrying the axes.  So the whole binding is a string
 *  builder — nothing to keep in step with an upstream API beyond the handful of keys used here.
 *
 *  Keeping it pure is what makes it testable: a spec is checked by reading it, with no browser,
 *  no `Plotly` global and no rendering involved.  [[Plot]] is the thin impure half that hands
 *  the result to the page.
 *
 *  **Why Plotly rather than Vega-Lite is visible in exactly one line here**: `scaleanchor`, in
 *  [[coordinates]].  Leonardo's characteristic figures — a Nyquist diagram, a pole-zero map, a
 *  two-element vector drawn as a point — are *geometry in the complex plane*, where unequal
 *  axes do not merely look wrong, they **are** wrong: the unit circle becomes an ellipse, and
 *  how close a pole sits to the imaginary axis is a question the reader answers by eye.
 *  Vega-Lite has no aspect lock, and deriving one from the data range stops holding the moment
 *  the reader zooms.  That is a confidently wrong picture rather than a refusal — the failure
 *  mode this project declines everywhere else.
 */
object PlotSpec:

  /** A line through sampled points — what `plot <expr> <var> <lo> <hi>` draws.
   *
   *  @param points the (x, y) pairs, as `cli.Session.samplePoints` returns them
   *  @param label  the expression being drawn; the trace name and the y-axis title
   *  @param xLabel the name of the variable sampled over
   *  @return a Plotly `{data, layout}` document
   */
  def line(points: Seq[(Double, Double)], label: String, xLabel: String): String =
    val trace = obj(
      "type" -> str("scatter"),
      "mode" -> str("lines"),
      "name" -> str(label),
      "x"    -> arr(points.map(_._1)),
      "y"    -> arr(points.map(_._2))
    )
    document(trace, title = label, xTitle = xLabel, yTitle = label, equalAxes = false)

  /** Points in a plane, with **the axes locked to the same scale**.
   *
   *  This is the form every plot whose meaning is *geometric* takes — two-element vectors as
   *  coordinates today, and the Nyquist and pole-zero figures once F_0004 lands.  The lock is
   *  not decoration; see the note on this object.
   *
   *  @param points the (x, y) pairs
   *  @param label  what is being drawn; the trace name and the title
   *  @return a Plotly `{data, layout}` document carrying `yaxis.scaleanchor`
   */
  def coordinates(points: Seq[(Double, Double)], label: String): String =
    val trace = obj(
      "type"   -> str("scatter"),
      "mode"   -> str("markers"),
      "name"   -> str(label),
      "marker" -> obj("size" -> "9"),
      "x"      -> arr(points.map(_._1)),
      "y"      -> arr(points.map(_._2))
    )
    document(trace, title = label, xTitle = "Re", yTitle = "Im", equalAxes = true)

  /** A Bode diagram: magnitude and phase stacked over one shared logarithmic frequency axis.
   *
   *  **Three things make this a Bode plot rather than two line charts** (issue F_0004), and
   *  all three are visible in the spec:
   *
   *   - `xaxis.type = "log"`, because a frequency response is read across decades;
   *   - `yaxis2.matches`/`anchor` pinning the two panels to the *same* x-axis, so panning one
   *     pans the other and a reader can never compare a gain to the wrong frequency;
   *   - the phase in degrees on its own scale, which is why this cannot be a second trace on
   *     one pair of axes: decibels and degrees share no units.
   *
   *  The unwrapping and the geometric grid are upstream, in `control.frequencyResponse`; what
   *  is here is only the presentation of what it returns.
   *
   *  @param points `(omega, dB, degrees)` triples, ascending in omega
   *  @param label  the plant being drawn, used as the title
   *  @return a Plotly `{data, layout}` document with two stacked subplots
   */
  def bode(points: Seq[(Double, Double, Double)], label: String): String =
    val omega = arr(points.map(_._1))
    val gain = obj(
      "type" -> str("scatter"), "mode" -> str("lines"), "name" -> str("gain"),
      "x"    -> omega,          "y"    -> arr(points.map(_._2))
    )
    val phase = obj(
      "type" -> str("scatter"), "mode"  -> str("lines"), "name" -> str("phase"),
      "x"    -> omega,          "y"     -> arr(points.map(_._3)),
      "xaxis" -> str("x2"),     "yaxis" -> str("y2")
    )
    val layout = obj(
      "title"  -> str(label),
      // Two rows: gain on top, phase below, each with 45% of the height.
      "xaxis"  -> obj("type" -> str("log"), "domain" -> "[0,1]", "anchor" -> str("y"),
                      "showticklabels" -> "false"),
      "yaxis"  -> obj("title" -> str("dB"), "domain" -> "[0.55,1]", "anchor" -> str("x"),
                      "zeroline" -> "true"),
      // `matches` is the link: zooming either panel moves both, so the two curves stay
      // registered against one another whatever the reader does.
      "xaxis2" -> obj("type" -> str("log"), "domain" -> "[0,1]", "anchor" -> str("y2"),
                      "matches" -> str("x"), "title" -> str("ω (rad/s)")),
      "yaxis2" -> obj("title" -> str("degrees"), "domain" -> "[0,0.45]", "anchor" -> str("x2"),
                      "zeroline" -> "true"),
      "margin"     -> obj("t" -> "40", "r" -> "20", "b" -> "45", "l" -> "60"),
      "showlegend" -> "false"
    )
    obj("data" -> s"[$gain,$phase]", "layout" -> layout)

  /** Wraps one trace in the shared layout. */
  private def document(trace: String, title: String, xTitle: String, yTitle: String,
                       equalAxes: Boolean): String =
    val yAxis =
      if equalAxes then
        // scaleanchor + scaleratio IS the aspect lock, and it survives the reader's zoom
        // because Plotly re-applies it to the axes rather than to a fixed pixel box.
        obj("title" -> str(yTitle), "zeroline" -> "true",
            "scaleanchor" -> str("x"), "scaleratio" -> "1")
      else obj("title" -> str(yTitle), "zeroline" -> "true")
    val layout = obj(
      "title"      -> str(title),
      "xaxis"      -> obj("title" -> str(xTitle), "zeroline" -> "true"),
      "yaxis"      -> yAxis,
      "margin"     -> obj("t" -> "40", "r" -> "20", "b" -> "45", "l" -> "60"),
      "showlegend" -> "false"
    )
    obj("data" -> s"[$trace]", "layout" -> layout)

  /** Renders a numeric array, dropping any non-finite value.
   *
   *  `sample` already drops non-finite results, so this is belt and braces — but it is cheap
   *  insurance against a failure that is near-impossible to diagnose from the page: **NaN and
   *  Infinity are not JSON**, so one of them anywhere in the data makes the whole document
   *  unparseable and the figure simply never appears, with nothing saying why.
   */
  private def arr(values: Seq[Double]): String =
    values.filter(d => !d.isNaN && !d.isInfinite).mkString("[", ",", "]")

  /** Renders an object from already-rendered values. */
  private def obj(fields: (String, String)*): String =
    fields.map((k, v) => s"${str(k)}:$v").mkString("{", ",", "}")

  /** A JSON string literal.
   *
   *  **Every label reaching this is user-typed expression text**, so the escaping is
   *  load-bearing rather than pro forma: plotting an expression containing a quote or a
   *  backslash would otherwise emit a malformed document and draw nothing.  Control characters
   *  become `\u….` escapes because a raw one is invalid inside a JSON string.
   */
  private[web] def str(s: String): String =
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"'          => sb ++= "\\\""
      case '\\'         => sb ++= "\\\\"
      case '\n'         => sb ++= "\\n"
      case '\r'         => sb ++= "\\r"
      case '\t'         => sb ++= "\\t"
      case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
      case c            => sb += c
    }
    (sb += '"').toString
