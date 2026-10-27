package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0238: `Session.figure` draws the drawing commands as an SVG image, from the session's data.
 *  Shared, so it runs on Node too: the text must not depend on how a platform prints a `Double`. */
class FigureTest extends AnyFlatSpec:

  private def svg(s: Session, lines: String*): String = s.figure(lines*) match
    case Right(text) => text
    case Left(why)   => fail(why)

  private val Polyline = """<polyline points="([^"]*)"""".r

  /** The pixel coordinates of the first polyline. */
  private def firstPolyline(text: String): Vector[(Double, Double)] =
    Polyline.findFirstMatchIn(text).fold(fail("no polyline"))(m =>
      m.group(1).split(' ').toVector.map { p => val Array(x, y) = p.split(','); (x.toDouble, y.toDouble) })

  "a plot" should "be an SVG document with a line and no non-finite number" in {
    val out = svg(new Session(), "plot 1/x x -1 1")
    assert(out.startsWith("<svg xmlns=\"http://www.w3.org/2000/svg\"") && out.endsWith("</svg>"))
    assert(out.contains("<polyline") && out.contains("<title>1/x</title>"))
    assert(!out.contains("NaN") && !out.contains("Infinity"))
  }

  "a curve" should "keep a circle round: the axes are locked to one scale" in {
    val pts = firstPolyline(svg(new Session(), "curve [[cos(t)], [sin(t)]] t 0 6.2832"))
    val (w, h) = (pts.map(_._1).max - pts.map(_._1).min, pts.map(_._2).max - pts.map(_._2).min)
    assert(math.abs(w - h) <= 1.0, s"$w x $h")
  }

  it should "read the session's bindings" in {
    val s = new Session()
    s.execute("A := [[2, 0], [0, 1]]")
    val pts = firstPolyline(svg(s, "curve A*[[cos(t)], [sin(t)]] t 0 6.2832"))
    val (w, h) = (pts.map(_._1).max - pts.map(_._1).min, pts.map(_._2).max - pts.map(_._2).min)
    assert(math.abs(w / h - 2) < 0.02, s"$w x $h")
  }

  "arrows" should "draw one shaft and one head per column" in {
    val out = svg(new Session(), "arrows [[1, 0], [0, 2]]")
    assert("<polygon ".r.findAllIn(out).size == 2)
  }

  "a polygon" should "be filled and need three vertices" in {
    val s = new Session()
    assert(svg(s, "polygon [[0, 1, 1, 0], [0, 0, 1, 1]]").contains("fill-opacity=\"0.18\""))
    assert(s.figure("polygon [[0, 1], [0, 1]]").left.exists(_.contains("at least 3 columns")))
  }

  "layers separated by ;" should "share one figure, with a legend" in {
    val out = svg(new Session(), "arrows [[1, 0], [0, 1]] ; polygon [[0, 1, 1, 0], [0, 0, 1, 1]]")
    assert("rx=\"2\"".r.findAllIn(out).size == 2)
    assert(out.contains("#2a78d6") && out.contains("#eb6834"))
  }

  it should "come from several lines alike" in {
    val s = new Session()
    assert(svg(s, "arrows [[1], [0]]", "arrows [[0], [1]]") == svg(s, "arrows [[1], [0]] ; arrows [[0], [1]]"))
  }

  "a histogram" should "draw one bar per bin" in {
    val out = svg(new Session(), "hist [[1, 2, 2, 3, 3, 3]] 3")
    assert("stroke=\"#fcfcfb\" stroke-width=\"1\"".r.findAllIn(out).size == 3)
  }

  "a Bode diagram" should "have its two panels, and be drawn alone" in {
    val s = new Session()
    val out = svg(s, "bode 1/(s+1) s 0.01 100")
    assert(out.contains(">dB<") && out.contains(">degrees<"))
    assert(s.figure("bode 1/(s+1) s 0.01 100 ; plot x x 0 1").left.exists(_.contains("drawn alone")))
  }

  "a refusal" should "name what is wrong" in {
    val s = new Session()
    assert(s.figure("draw x").left.exists(_.contains("not a drawing command")))
    assert(s.figure("arrows [[1, 2, 3]]").left.exists(_.contains("2×n matrix")))
    assert(s.figure("curve t t 0 1").left.exists(_.contains("2×1 column")))
    assert(s.figure("plot").left.exists(_.startsWith("usage: plot")))
    assert(s.figure().left.exists(_.contains("nothing to draw")))
  }

  "a label" should "be escaped for XML" in {
    assert(Svg.esc("a<b & \"c\"") == "a&lt;b &amp; &quot;c&quot;")
  }
