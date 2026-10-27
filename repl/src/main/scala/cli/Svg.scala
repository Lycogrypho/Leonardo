package it.grypho.scala.leonardo
package cli

import core._Number
import Figure.{Layer, Plot, Point}

/** Draws a [[Figure.Plot]] as a static SVG image — **pure data in, pure text out** (F_0238).
 *
 *  The build-time counterpart of the browser's `web.PlotSpec`: a page rendered ahead of time
 *  (a lesson, through `docs.Lesson.figure`) cannot run Plotly, so it carries an image instead.
 *  It follows `PlotSpec`'s rules, each for the same reason:
 *
 *   - **the aspect lock**: when a layer is geometric the axes share one scale, so the unit circle
 *     stays a circle — the ranges are widened, never the picture stretched;
 *   - **points are dropped whole** (F_0022), already by [[Figure.finite]] upstream;
 *   - **every label is user-typed text and is escaped** — for XML here, where `PlotSpec` escapes
 *     for JSON — or one `<` in an expression makes the whole image unreadable.
 *
 *  **Output is identical on the JVM and on Node**: every number reaches the text through
 *  `_Number.render` or a `BigDecimal`, never a `Double`'s own `toString`, whose form differs
 *  between the platforms (the `DoubleRender` rule).  No font is embedded: the text names the
 *  system sans, so nothing licensed is shipped.
 *
 *  The colours are the validated reference palette's light steps, in its fixed slot order (layer
 *  `i` takes slot `i`, never a cycled hue), on its light chart surface: the documentation site is
 *  light, and an image cannot follow the page's theme.  A figure of two or more layers carries a
 *  legend, so identity is never colour alone; each layer carries a `<title>`, the tooltip a
 *  static image can offer.
 */
private[leonardo] object Svg:

  private val Width  = 560
  private val Height = 400
  private val PadL   = 60.0
  private val PadRight  = 20.0
  private val PadBottom = 44.0

  private val Surface   = "#fcfcfb"
  private val Ink       = "#0b0b0b"
  private val Muted     = "#898781"
  private val Grid      = "#e1e0d9"
  private val Axis      = "#c3c2b7"
  private val FontStack = "system-ui, -apple-system, &quot;Segoe UI&quot;, sans-serif"

  private val Slots = Figure.Slots

  /** The SVG text of `plot`.
   *  @param plot the figure; a [[Figure.Layer.Bode]] is drawn alone, as its two panels
   *  @return the `<svg>` document */
  def render(plot: Plot): String =
    val body = plot.layers match
      case Vector(b: Layer.Bode) => bode(b)
      case layers                => cartesian(plot, layers)
    val title = plot.layers.map(_.label).mkString(" ; ")
    val head =
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="$Width" height="$Height" viewBox="0 0 $Width $Height" """ +
      s"""role="img" aria-label="${esc(title)}" font-family="$FontStack" font-size="11">"""
    (head +: s"<title>${esc(title)}</title>" +: rect(0, 0, Width, Height, s"""fill="$Surface"""") +: body :+ "</svg>").mkString("\n")

  // ── one panel ──

  /** A plot area and the data range it shows. */
  private final case class Frame(x0: Double, y0: Double, w: Double, h: Double,
                                 xMin: Double, xMax: Double, yMin: Double, yMax: Double, logX: Boolean):
    private def xs(x: Double): Double = if logX then math.log10(x) else x
    def px(x: Double): Double = x0 + (xs(x) - xs(xMin)) / (xs(xMax) - xs(xMin)) * w
    def py(y: Double): Double = y0 + h - (y - yMin) / (yMax - yMin) * h

  private def cartesian(plot: Plot, layers: Vector[Layer]): Vector[String] =
    val legend = layers.sizeIs >= 2
    // With two or more layers the legend names them, in place of a title that would repeat it,
    // on as many rows as its entries need.
    val (keys, rows) = if legend then legendRows(layers) else (Vector.empty, 1)
    val top    = 34.0 + 16 * (rows - 1)
    val (w, h) = (Width - PadL - PadRight, Height - top - PadBottom)
    val pts    = layers.flatMap(extent)
    val (xr, yr) = (padded(pts.map(_._1), layers.exists(isBars)), padded(pts.map(_._2), layers.exists(isBars)))
    val (xMin, xMax, yMin, yMax) =
      if plot.geometric then locked(xr, yr, w, h) else (xr._1, xr._2, yr._1, yr._2)
    val f = Frame(PadL, top, w, h, xMin, xMax, yMin, yMax, logX = false)
    val heading =
      if legend then keys
      else Vector(text(PadL, 18, esc(plot.layers.map(_.label).mkString(" ; ")), s"""fill="$Ink" font-size="13""""))
    heading ++ axes(f, plot.xTitle, plot.yTitle) ++ layers.zipWithIndex.map((l, i) => layer(l, Slots(i % Slots.size), f))

  /** The points a layer's extent must include: an arrow its origin, a bar its baseline. */
  private def extent(l: Layer): Vector[Point] = l match
    case Layer.Line(_, p, _)  => p
    case Layer.Markers(_, p)  => p
    case Layer.Arrows(_, t)   => t :+ (0.0, 0.0)
    case Layer.Polygon(_, v)  => v
    case Layer.Bars(_, e, c)  => e.headOption.fold(Vector.empty[Point])(x0 => e.map(x => (x, 0.0)) ++ c.map(k => (x0, k.toDouble)))
    case Layer.Locus(_, b, p, z) => b.flatten ++ p ++ z
    case Layer.Bode(_, p)     => p.map((w, g, _) => (w, g))

  private def isBars(l: Layer): Boolean = l match
    case Layer.Bars(_, _, _) => true
    case _                   => false

  /** A range around `vs` with a 5% margin; a bar chart's starts at its baseline. */
  private def padded(vs: Vector[Double], bars: Boolean): (Double, Double) =
    if vs.isEmpty then (-1.0, 1.0)
    else
      val (lo, hi) = (vs.min, vs.max)
      if lo == hi then (lo - math.max(1.0, math.abs(lo) / 2), hi + math.max(1.0, math.abs(hi) / 2))
      else
        val m = (hi - lo) * 0.05
        (if bars && lo == 0.0 then 0.0 else lo - m, hi + m)

  /** The two ranges widened about their centres until one unit is as long on both axes. */
  private def locked(xr: (Double, Double), yr: (Double, Double), w: Double, h: Double): (Double, Double, Double, Double) =
    val unit = math.max((xr._2 - xr._1) / w, (yr._2 - yr._1) / h)
    val (cx, cy) = ((xr._1 + xr._2) / 2, (yr._1 + yr._2) / 2)
    (cx - unit * w / 2, cx + unit * w / 2, cy - unit * h / 2, cy + unit * h / 2)

  /** Gridlines, the zero lines, tick labels and axis titles: recessive ink, the data in front. */
  private def axes(f: Frame, xTitle: String, yTitle: String): Vector[String] =
    val (xStep, yStep) = (step(f.xMax - f.xMin), step(f.yMax - f.yMin))
    val xTicks = ticks(f.xMin, f.xMax, xStep)
    val yTicks = ticks(f.yMin, f.yMax, yStep)
    val grid = xTicks.map(x => line(f.px(x), f.y0, f.px(x), f.y0 + f.h, Grid, 1)) ++
               yTicks.map(y => line(f.x0, f.py(y), f.x0 + f.w, f.py(y), Grid, 1))
    val zero = Vector(
      Option.when(f.yMin <= 0 && 0 <= f.yMax)(line(f.x0, f.py(0), f.x0 + f.w, f.py(0), Axis, 1)),
      Option.when(f.xMin <= 0 && 0 <= f.xMax)(line(f.px(0), f.y0, f.px(0), f.y0 + f.h, Axis, 1))).flatten
    val labels =
      xTicks.map(x => text(f.px(x), f.y0 + f.h + 16, tickLabel(x, xStep), s"""fill="$Muted" text-anchor="middle"""")) ++
      yTicks.map(y => text(f.x0 - 6, f.py(y) + 4, tickLabel(y, yStep), s"""fill="$Muted" text-anchor="end""""))
    val titles = Vector(
      Option.when(xTitle.nonEmpty)(text(f.x0 + f.w / 2, f.y0 + f.h + 34, esc(xTitle), s"""fill="$Muted" text-anchor="middle"""")),
      Option.when(yTitle.nonEmpty)(
        s"""<text x="${n(14)}" y="${n(f.y0 + f.h / 2)}" fill="$Muted" text-anchor="middle" transform="rotate(-90 ${n(14)} ${n(f.y0 + f.h / 2)})">${esc(yTitle)}</text>""")
    ).flatten
    grid ++ zero ++ labels ++ titles

  /** One layer in its slot's colour, grouped under a `<title>` so a pointer over it names it. */
  private def layer(l: Layer, colour: String, f: Frame): String =
    val marks: Vector[String] = l match
      case Layer.Line(_, p, _) => Vector(polyline(p, f, colour))
      case Layer.Markers(_, p) => p.map(dot(_, f, colour))
      case Layer.Arrows(_, t)  => t.flatMap(arrow(_, f, colour))
      case Layer.Polygon(_, v) =>
        Vector(s"""<polygon points="${coords(v, f)}" fill="$colour" fill-opacity="0.18" stroke="$colour" stroke-width="2" stroke-linejoin="round"/>""")
      case Layer.Bars(_, e, c) =>
        e.zip(e.drop(1)).zip(c).map { case ((a, b), k) =>
          val (x, y) = (f.px(a), f.py(k.toDouble))
          rect(x, y, f.px(b) - x, f.py(0) - y, s"""fill="$colour" stroke="$Surface" stroke-width="1"""")
        }
      case Layer.Locus(_, b, p, z) =>
        b.map(polyline(_, f, colour)) ++ p.map(cross(_, f, colour)) ++ z.map(ring(_, f, colour))
      case Layer.Bode(_, _) => Vector.empty   // drawn alone, by `bode`
    (s"<g><title>${esc(l.label)}</title>" +: marks :+ "</g>").mkString("\n")

  /** One swatch and label per layer above the plot area, wrapping to a new row rather than past
   *  the image's edge.
   *  @return the elements and the number of rows */
  private def legendRows(layers: Vector[Layer]): (Vector[String], Int) =
    val (_, row, items) = layers.zipWithIndex.foldLeft((PadL, 0, Vector.empty[String])) { case ((x0, r0, acc), (l, i)) =>
      val label = clip(l.label)
      // A generous 7 px a character at 11 px: the system sans is not measured here, and two
      // labels that overlap read as one (seen at 6.2 with bracket-heavy matrix labels).
      val width  = 28 + 7.0 * label.length
      val (x, r) = if x0 > PadL && x0 + width > Width - PadRight then (PadL, r0 + 1) else (x0, r0)
      val y  = 18.0 + 16 * r
      val sw = rect(x, y - 8, 10, 10, s"""fill="${Slots(i % Slots.size)}" rx="2"""")
      val tx = text(x + 14, y + 1, esc(label), s"""fill="$Ink"""")
      (x + width, r, acc :+ sw :+ tx)
    }
    (items, row + 1)

  // ── the Bode diagram ──

  /** Gain over phase, one logarithmic frequency axis shared by both panels, as `PlotSpec.bode`. */
  private def bode(b: Layer.Bode): Vector[String] =
    val pts = b.points.filter((w, g, p) => w > 0 && w.isFinite && g.isFinite && p.isFinite)
    val (w, ph) = (Width - PadL - PadRight, (Height - 34.0 - PadBottom - 28) / 2)
    val (wMin, wMax) = if pts.isEmpty then (1.0, 10.0) else (pts.map(_._1).min, pts.map(_._1).max)
    val (wLo, wHi) = if wMin < wMax then (wMin, wMax) else (wMin / 10, wMin * 10)
    def panel(y0: Double, values: Vector[Double], title: String, xLabels: Boolean): (Frame, Vector[String]) =
      val (lo, hi) = padded(values, bars = false)
      val f = Frame(PadL, y0, w, ph, wLo, wHi, lo, hi, logX = true)
      val yStep = step(hi - lo)
      val decades = (math.ceil(math.log10(wLo)).toInt to math.floor(math.log10(wHi)).toInt).map(k => math.pow(10, k))
      val grid = decades.map(x => line(f.px(x), y0, f.px(x), y0 + ph, Grid, 1)).toVector ++
                 ticks(lo, hi, yStep).map(y => line(PadL, f.py(y), PadL + w, f.py(y), Grid, 1))
      val labels = ticks(lo, hi, yStep).map(y => text(PadL - 6, f.py(y) + 4, tickLabel(y, yStep), s"""fill="$Muted" text-anchor="end"""")) ++
        (if xLabels then decades.map(x => text(f.px(x), y0 + ph + 16, decadeLabel(x), s"""fill="$Muted" text-anchor="middle"""")).toVector
         else Vector.empty)
      val t = s"""<text x="${n(14)}" y="${n(y0 + ph / 2)}" fill="$Muted" text-anchor="middle" transform="rotate(-90 ${n(14)} ${n(y0 + ph / 2)})">$title</text>"""
      (f, grid ++ labels :+ t)
    val (fg, gainAxes)  = panel(34, pts.map(_._2), "dB", xLabels = false)
    val (fp, phaseAxes) = panel(34 + ph + 28, pts.map(_._3), "degrees", xLabels = true)
    val title = text(PadL, 18, esc(b.label), s"""fill="$Ink" font-size="13"""")
    val xTitle = text(PadL + w / 2, Height - 8, "ω (rad/s)", s"""fill="$Muted" text-anchor="middle"""")
    val curves = s"<g><title>${esc(b.label)}</title>\n${polyline(pts.map(p => (p._1, p._2)), fg, Slots(0))}\n" +
                 s"${polyline(pts.map(p => (p._1, p._3)), fp, Slots(0))}\n</g>"
    (title +: gainAxes) ++ phaseAxes :+ xTitle :+ curves

  // ── marks ──

  private def polyline(p: Vector[Point], f: Frame, colour: String): String =
    s"""<polyline points="${coords(p, f)}" fill="none" stroke="$colour" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>"""

  private def dot(p: Point, f: Frame, colour: String): String =
    s"""<circle cx="${n(f.px(p._1))}" cy="${n(f.py(p._2))}" r="4" fill="$colour" stroke="$Surface" stroke-width="1.5"/>"""

  private def ring(p: Point, f: Frame, colour: String): String =
    s"""<circle cx="${n(f.px(p._1))}" cy="${n(f.py(p._2))}" r="5" fill="$Surface" stroke="$colour" stroke-width="2"/>"""

  private def cross(p: Point, f: Frame, colour: String): String =
    val (x, y) = (f.px(p._1), f.py(p._2))
    line(x - 5, y - 5, x + 5, y + 5, colour, 2) + line(x - 5, y + 5, x + 5, y - 5, colour, 2)

  /** An arrow from the origin to `tip`: a shaft and a filled head, drawn in pixels so the head
   *  keeps its shape whatever the scale.  A zero vector is a dot, having no direction. */
  private def arrow(tip: Point, f: Frame, colour: String): Vector[String] =
    val (x0, y0) = (f.px(0), f.py(0))
    val (x1, y1) = (f.px(tip._1), f.py(tip._2))
    val len = math.hypot(x1 - x0, y1 - y0)
    if len < 1e-9 then Vector(dot(tip, f, colour))
    else
      val (ux, uy) = ((x1 - x0) / len, (y1 - y0) / len)
      val head = math.min(10.0, len * 0.6)
      val (bx, by) = (x1 - ux * head, y1 - uy * head)
      val (nx, ny) = (-uy * head * 0.45, ux * head * 0.45)
      Vector(
        line(x0, y0, bx, by, colour, 2),
        s"""<polygon points="${n(x1)},${n(y1)} ${n(bx + nx)},${n(by + ny)} ${n(bx - nx)},${n(by - ny)}" fill="$colour"/>""")

  private def line(x1: Double, y1: Double, x2: Double, y2: Double, colour: String, width: Double): String =
    s"""<line x1="${n(x1)}" y1="${n(y1)}" x2="${n(x2)}" y2="${n(y2)}" stroke="$colour" stroke-width="${n(width)}"/>"""

  private def rect(x: Double, y: Double, w: Double, h: Double, attrs: String): String =
    s"""<rect x="${n(x)}" y="${n(y)}" width="${n(math.max(w, 0))}" height="${n(math.max(h, 0))}" $attrs/>"""

  private def text(x: Double, y: Double, content: String, attrs: String): String =
    s"""<text x="${n(x)}" y="${n(y)}" $attrs>$content</text>"""

  private def coords(p: Vector[Point], f: Frame): String =
    p.map((x, y) => s"${n(f.px(x))},${n(f.py(y))}").mkString(" ")

  // ── numbers and text ──

  /** A tick spacing near a fifth of `range`: 1, 2 or 5 times a power of ten. */
  private def step(range: Double): Double =
    val raw  = range / 5
    val unit = math.pow(10, math.floor(math.log10(raw)))
    Vector(1.0, 2.0, 5.0, 10.0).map(_ * unit).find(_ >= raw).getOrElse(10 * unit)

  /** The multiples of `step` within `[lo, hi]`. */
  private def ticks(lo: Double, hi: Double, step: Double): Vector[Double] =
    val first = math.ceil(lo / step).toLong
    val last  = math.floor(hi / step).toLong
    if last - first > 50 then Vector.empty else (first to last).map(_ * step).toVector

  /** A tick value with exactly the decimals its spacing needs, so `0.30000000000000004` reads 0.3. */
  private def tickLabel(v: Double, step: Double): String =
    val decimals = math.max(0, -math.floor(math.log10(step)).toInt)
    val s = BigDecimal(v).setScale(decimals, BigDecimal.RoundingMode.HALF_UP).bigDecimal.toPlainString
    if s.matches("-0(\\.0*)?") then s.drop(1) else s.replace('-', '−')

  /** A decade on the Bode axis: `0.01`, `1`, `100`, or `10^k` past a thousand either way. */
  private def decadeLabel(x: Double): String =
    val k = math.round(math.log10(x)).toInt
    if k >= 0 && k <= 3 then "1" + "0" * k
    else if k < 0 && k >= -3 then "0." + "0" * (-k - 1) + "1"
    else s"1e$k"

  /** A pixel coordinate to one decimal, through the platform-neutral renderer. */
  private def n(d: Double): String =
    val r = math.round(d * 10) / 10.0
    if r == 0 then "0"
    else
      val s = _Number.render(r)
      if s.endsWith(".0") then s.dropRight(2) else s

  /** A long label cut for the legend, where it shares a row. */
  private def clip(s: String): String = if s.length <= 40 then s else s.take(39) + "…"

  /** XML text and attribute escaping: every label is user-typed. */
  private[leonardo] def esc(s: String): String =
    s.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&apos;"
      case c if c < ' ' => " "
      case c    => c.toString
    }
