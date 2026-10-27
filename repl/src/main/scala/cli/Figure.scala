package it.grypho.scala.leonardo
package cli

/** A figure as data: the layers a drawing line produces, before anything renders them (F_0238).
 *
 *  **One reading of the drawing commands, two renderers.**  `Session` turns each command into a
 *  [[Figure.Layer]] from the same session data the browser's figures read (`samplePoints`,
 *  `bodePoints`, …), so a figure is never rebuilt from printed text; [[Svg]] draws the layers as
 *  a static image for a page built ahead of time, and the browser's `web.PlotSpec` draws the same
 *  layers with Plotly.  A layer carries numbers, never text to re-parse, which is the
 *  `samplePoints` split extended to the new primitives.
 *
 *  Points are kept or dropped **whole** (F_0022): a layer never holds a coordinate pair one of
 *  whose halves was not finite.
 */
private[leonardo] object Figure:

  /** A point of the plane. */
  type Point = (Double, Double)

  /** One drawing command's contribution to a figure. */
  enum Layer:
    /** A curve through its points: `plot` (a graph), or `curve` (geometry in the plane). */
    case Line(label: String, points: Vector[Point], inPlane: Boolean)
    /** Points in the plane: `points` and `nyquist`. */
    case Markers(label: String, points: Vector[Point])
    /** Arrows from the origin to each tip: `arrows`, one per column of a `2×n` matrix. */
    case Arrows(label: String, tips: Vector[Point])
    /** A closed, filled polygon: `polygon`, its vertices the columns of a `2×n` matrix. */
    case Polygon(label: String, vertices: Vector[Point])
    /** A histogram: `hist`, `k + 1` edges and `k` counts. */
    case Bars(label: String, edges: Vector[Double], counts: Vector[Int])
    /** A root locus: `rlocus`, its branches with the open-loop poles and zeros. */
    case Locus(label: String, branches: Vector[Vector[Point]], poles: Vector[Point], zeros: Vector[Point])
    /** A Bode diagram: `bode`, `(omega, dB, degrees)`.  Two panels, so it is drawn alone. */
    case Bode(label: String, points: Vector[(Double, Double, Double)])

    /** What the layer draws, as the user typed it: the legend's text and the tooltip's. */
    def label: String

    /** Whether the layer is geometry in the plane, so the figure's axes must share one scale:
     *  on unequal axes a circle becomes an ellipse and an angle lies (the `PlotSpec` aspect lock). */
    def geometric: Boolean = this match
      case Line(_, _, g)                                  => g
      case Markers(_, _) | Arrows(_, _) | Polygon(_, _)  => true
      case Locus(_, _, _, _)                              => true
      case Bars(_, _, _) | Bode(_, _)                     => false

  /** The layers of one figure, drawn on shared axes, with the axis titles.
   *  @param layers the layers, in the order written; layer `i` takes palette slot `i`
   *  @param xTitle the horizontal axis's title, empty for none
   *  @param yTitle the vertical axis's title, empty for none */
  final case class Plot(layers: Vector[Layer], xTitle: String, yTitle: String):
    /** Whether the axes are locked to one scale: any geometric layer decides it. */
    def geometric: Boolean = layers.exists(_.geometric)

  /** The colour of layer `i`: the validated reference palette's categorical slots (light steps),
   *  in their fixed order — the slot follows the layer, never a cycled or generated hue.  One
   *  definition for [[Svg]] and the browser's `web.PlotSpec`, so a figure and its run link agree. */
  val Slots: Vector[String] = Vector("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

  /** The arguments each drawing word takes, for its usage line. */
  val Usage: Map[String, String] = Map(
    "plot"    -> "<expr> <var> <lo> <hi> [<n>]",
    "points"  -> "<expr> <var> <lo> <hi> [<n>]",
    "curve"   -> "<2×1 column expr> <var> <lo> <hi> [<n>]",
    "arrows"  -> "<2×n matrix>",
    "polygon" -> "<2×n matrix>",
    "hist"    -> "<sample> [<bins>]",
    "bode"    -> "<expr> <var> <wMin> <wMax> [<n>]",
    "nyquist" -> "<expr> <var> <wMin> <wMax> [<n>]",
    "rlocus"  -> "<G> <var> <kMin> <kMax> [<n>]")

  /** The drawing words a figure line may start with, in the order the messages list them. */
  val Words: List[String] = List("plot", "points", "curve", "arrows", "polygon", "hist", "bode", "nyquist", "rlocus")

  /** Keeps the points whose coordinates are both finite. */
  def finite(points: Seq[Point]): Vector[Point] =
    points.iterator.filter((x, y) => x.isFinite && y.isFinite).toVector
