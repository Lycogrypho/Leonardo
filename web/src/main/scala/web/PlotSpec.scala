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
    val drawn = finitePairs(points)
    val trace = obj(
      "type" -> str("scatter"),
      "mode" -> str("lines"),
      "name" -> str(label),
      "x"    -> arr(drawn.map(_._1)),
      "y"    -> arr(drawn.map(_._2))
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
    val drawn = finitePairs(points)
    val trace = obj(
      "type"   -> str("scatter"),
      "mode"   -> str("markers"),
      "name"   -> str(label),
      "marker" -> obj("size" -> "9"),
      "x"      -> arr(drawn.map(_._1)),
      "y"      -> arr(drawn.map(_._2))
    )
    document(trace, title = label, xTitle = "Re", yTitle = "Im", equalAxes = true)

  /** A root locus (F_0140): one line per branch, the open-loop poles as crosses and zeros as
   *  circles, **axes locked** — the locus is geometry in the s-plane, where how close a branch
   *  runs to the imaginary axis is read by eye, the reason [[coordinates]] locks them too.
   *
   *  @param branches the closed-loop poles, one `(re, im)` sequence per branch
   *  @param poles    the open-loop poles
   *  @param zeros    the open-loop zeros
   *  @param label    the plant, the title
   *  @return a Plotly `{data, layout}` document
   */
  def locus(branches: Seq[Seq[(Double, Double)]], poles: Seq[(Double, Double)], zeros: Seq[(Double, Double)],
            label: String): String =
    def line(b: Seq[(Double, Double)], i: Int): String =
      val drawn = finitePairs(b)
      obj("type" -> str("scatter"), "mode" -> str("lines"), "name" -> str(s"branch ${i + 1}"),
          "x" -> arr(drawn.map(_._1)), "y" -> arr(drawn.map(_._2)))
    def marks(ps: Seq[(Double, Double)], symbol: String, name: String): String =
      val drawn = finitePairs(ps)
      obj("type" -> str("scatter"), "mode" -> str("markers"), "name" -> str(name),
          "marker" -> obj("symbol" -> str(symbol), "size" -> "11"),
          "x" -> arr(drawn.map(_._1)), "y" -> arr(drawn.map(_._2)))
    val traces = branches.zipWithIndex.map(line) :+ marks(poles, "x", "poles") :+ marks(zeros, "circle-open", "zeros")
    val layout = obj(
      "title"      -> str(label),
      "xaxis"      -> obj("title" -> str("Re"), "zeroline" -> "true"),
      "yaxis"      -> obj("title" -> str("Im"), "zeroline" -> "true", "scaleanchor" -> str("x"), "scaleratio" -> "1"),
      "margin"     -> obj("t" -> "40", "r" -> "20", "b" -> "45", "l" -> "60"),
      "showlegend" -> "false"
    )
    obj("data" -> traces.mkString("[", ",", "]"), "layout" -> layout)

  /** A histogram (F_0135): one bar per bin, centred on the bin and as wide as it, so adjacent
   *  bars touch — the shape a histogram has, and what tells it apart from a bar chart of
   *  categories.  The bins come from `cli.Session.histogramBins`.
   *
   *  @param edges  the `k + 1` bin edges, ascending
   *  @param counts the `k` counts
   *  @param label  the sample as typed; the title
   *  @return a Plotly `{data, layout}` document
   */
  def histogram(edges: Seq[Double], counts: Seq[Int], label: String): String =
    val bins  = edges.zip(edges.drop(1)).zip(counts).filter { case ((a, b), _) => finite(a) && finite(b) }
    val trace = obj(
      "type"  -> str("bar"),
      "name"  -> str(label),
      "x"     -> arr(bins.map { case ((a, b), _) => (a + b) / 2 }),
      "y"     -> arr(bins.map(_._2.toDouble)),
      "width" -> arr(bins.map { case ((a, b), _) => b - a })
    )
    document(trace, title = label, xTitle = label, yTitle = "count", equalAxes = false)

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
    val drawn = finiteTriples(points)
    val omega = arr(drawn.map(_._1))
    val gain = obj(
      "type" -> str("scatter"), "mode" -> str("lines"), "name" -> str("gain"),
      "x"    -> omega,          "y"    -> arr(drawn.map(_._2))
    )
    val phase = obj(
      "type" -> str("scatter"), "mode"  -> str("lines"), "name" -> str("phase"),
      "x"    -> omega,          "y"     -> arr(drawn.map(_._3)),
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

  /** Several layers on shared axes (F_0238): what a line of `;`-separated drawing commands, or
   *  one of `arrows`, `polygon` and `curve`, draws.  The layers are `cli.Session.figurePlot`'s,
   *  the ones `Session.figure` renders as SVG for a lesson, so a lesson's figure line typed here
   *  draws the same figure.  Layer `i` takes palette slot `i` (`cli.Figure.Slots`), as in the
   *  SVG; the axes are locked when any layer is geometric; arrows are annotations, which Plotly
   *  draws with a head, over a marker at each tip so the autorange includes it.  A lone Bode
   *  layer is [[bode]]'s two panels.
   *
   *  @param plot the figure
   *  @return a Plotly `{data, layout}` document
   */
  def figure(plot: cli.Figure.Plot): String =
    import cli.Figure.Layer
    plot.layers match
      case Vector(b: Layer.Bode) => bode(b.points, b.label)
      case layers =>
        def xy(name: String, colour: String, pts: Seq[(Double, Double)], extra: (String, String)*): String =
          val drawn  = finitePairs(pts)
          val fields = Seq("type" -> str("scatter"), "name" -> str(name), "x" -> arr(drawn.map(_._1)),
                           "y" -> arr(drawn.map(_._2))) ++ extra
          obj(fields*)
        def lines(c: String)   = "line" -> obj("color" -> str(c), "width" -> "2")
        def markers(c: String) = "marker" -> obj("color" -> str(c), "size" -> "8")
        val parts = layers.zipWithIndex.map { (l, i) =>
          val c = cli.Figure.Slots(i % cli.Figure.Slots.size)
          l match
            case Layer.Line(n, p, _) => (Seq(xy(n, c, p, "mode" -> str("lines"), lines(c))), Seq.empty)
            case Layer.Markers(n, p) => (Seq(xy(n, c, p, "mode" -> str("markers"), markers(c))), Seq.empty)
            case Layer.Polygon(n, v) =>
              val closed = v ++ v.headOption
              (Seq(xy(n, c, closed, "mode" -> str("lines"), "fill" -> str("toself"),
                      "fillcolor" -> str(translucent(c)), lines(c))), Seq.empty)
            case Layer.Arrows(n, t) =>
              val heads = finitePairs(t).map((x, y) => obj(
                "x" -> x.toString, "y" -> y.toString, "ax" -> "0", "ay" -> "0",
                "xref" -> str("x"), "yref" -> str("y"), "axref" -> str("x"), "ayref" -> str("y"),
                "showarrow" -> "true", "arrowhead" -> "2", "arrowwidth" -> "2", "arrowcolor" -> str(c), "text" -> str("")))
              (Seq(xy(n, c, (0.0, 0.0) +: t, "mode" -> str("markers"), markers(c))), heads)
            case Layer.Bars(n, e, k) =>
              val bins = e.zip(e.drop(1)).zip(k).filter { case ((a, b), _) => finite(a) && finite(b) }
              (Seq(obj("type" -> str("bar"), "name" -> str(n), "marker" -> obj("color" -> str(c)),
                       "x" -> arr(bins.map { case ((a, b), _) => (a + b) / 2 }), "y" -> arr(bins.map(_._2.toDouble)),
                       "width" -> arr(bins.map { case ((a, b), _) => b - a }))), Seq.empty)
            case Layer.Locus(n, bs, ps, zs) =>
              (bs.map(b => xy(n, c, b, "mode" -> str("lines"), lines(c))) :+
               xy(s"$n poles", c, ps, "mode" -> str("markers"), "marker" -> obj("color" -> str(c), "symbol" -> str("x"), "size" -> "11")) :+
               xy(s"$n zeros", c, zs, "mode" -> str("markers"), "marker" -> obj("color" -> str(c), "symbol" -> str("circle-open"), "size" -> "11")),
               Seq.empty)
            case Layer.Bode(_, _) => (Seq.empty, Seq.empty)   // refused beside another layer upstream
        }
        val yAxis =
          if plot.geometric then obj("title" -> str(plot.yTitle), "zeroline" -> "true", "scaleanchor" -> str("x"), "scaleratio" -> "1")
          else obj("title" -> str(plot.yTitle), "zeroline" -> "true")
        val layout = obj(
          "title"       -> str(layers.map(_.label).mkString(" ; ")),
          "xaxis"       -> obj("title" -> str(plot.xTitle), "zeroline" -> "true"),
          "yaxis"       -> yAxis,
          "annotations" -> parts.flatMap(_._2).mkString("[", ",", "]"),
          "margin"      -> obj("t" -> "40", "r" -> "20", "b" -> "45", "l" -> "60"),
          "showlegend"  -> (layers.sizeIs > 1).toString
        )
        obj("data" -> parts.flatMap(_._1).mkString("[", ",", "]"), "layout" -> layout)

  /** `#rrggbb` as an `rgba` at the polygon fill's opacity, the SVG's `fill-opacity`. */
  private def translucent(hex: String): String =
    val c = Integer.parseInt(hex.drop(1), 16)
    s"rgba(${(c >> 16) & 0xFF},${(c >> 8) & 0xFF},${c & 0xFF},0.18)"

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

  /** Whether a coordinate can be written to JSON at all. */
  private def finite(d: Double): Boolean = !d.isNaN && !d.isInfinite

  /** Keeps the points whose coordinates are *all* finite.
   *
   *  **The filter belongs to the point, never to the axis** (issue F_0022), and that is a
   *  correctness matter rather than tidiness: filtering each array as it is rendered leaves
   *  `x` longer than `y`, so every later point pairs with the wrong partner and the figure is
   *  **sheared** — a confidently wrong picture, which is the failure the aspect lock exists to
   *  prevent in the first place.  For a Bode diagram it is worse still: the two panels are
   *  pinned to one frequency axis, so dropping a sample from one and not the other registers a
   *  gain against another frequency's phase.
   *
   *  It is reached at all because **NaN and Infinity are not JSON**: one of either anywhere in
   *  the data makes the whole document unparseable, so the figure simply never appears with
   *  nothing saying why.  `sample` and the sweeps already drop non-finite points upstream, so
   *  this is belt and braces — but a guard that is safe only because a *neighbouring* tier
   *  currently behaves is the issue-1.4 shape, and this one costs one pass.
   */
  private def finitePairs(points: Seq[(Double, Double)]): Seq[(Double, Double)] =
    points.filter((x, y) => finite(x) && finite(y))

  /** [[finitePairs]] for a swept response, where a sample is three numbers. */
  private def finiteTriples(points: Seq[(Double, Double, Double)]): Seq[(Double, Double, Double)] =
    points.filter((w, gain, phase) => finite(w) && finite(gain) && finite(phase))

  /** Renders a numeric array.
   *
   *  It does **no** filtering of its own: every caller has already dropped whole points
   *  through [[finitePairs]] or [[finiteTriples]], and a second, weaker filter here would only
   *  be a way for the two to disagree about which points a figure contains.
   */
  private def arr(values: Seq[Double]): String = values.mkString("[", ",", "]")

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
