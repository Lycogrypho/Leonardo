package it.grypho.scala.leonardo
package web

import org.scalatest.flatspec.AnyFlatSpec

import scala.scalajs.js

/** Tests for the plotting facade's pure half (F_0003 phase 3).
 *
 *  **Every case ends at `js.JSON.parse`.**  Asserting on the spec's *text* would pin the key
 *  order and the spacing, neither of which means anything; parsing pins what actually matters,
 *  which is that a browser can read the document at all.  A malformed spec is the failure mode
 *  worth guarding, because its symptom in the page is simply that no figure appears.
 */
class PlotSpecTest extends AnyFlatSpec:

  /** Parses a spec, failing the test with the offending text rather than throwing. */
  private def parsed(spec: String): js.Dynamic =
    try js.JSON.parse(spec)
    catch case e: Throwable => fail(s"spec is not valid JSON (${e.getMessage}): $spec")

  "a line spec" should "be valid JSON carrying the sampled points" in
  {
    val doc = parsed(PlotSpec.line(Vector((0.0, 0.0), (1.0, 1.0), (2.0, 4.0)), "x^2", "x"))
    val trace = doc.data.asInstanceOf[js.Array[js.Dynamic]](0)
    assert(trace.x.asInstanceOf[js.Array[Double]].toVector == Vector(0.0, 1.0, 2.0))
    assert(trace.y.asInstanceOf[js.Array[Double]].toVector == Vector(0.0, 1.0, 4.0))
    assert(trace.mode.asInstanceOf[String] == "lines")
  }

  it should "title the axes with the expression and the variable" in
  {
    val doc = parsed(PlotSpec.line(Vector((0.0, 1.0)), "sin(t)", "t"))
    assert(doc.layout.xaxis.title.asInstanceOf[String] == "t")
    assert(doc.layout.yaxis.title.asInstanceOf[String] == "sin(t)")
  }

  it should "NOT lock the aspect ratio" in
  {
    // A function plot is not geometry: y is in the units of f and x in the units of the
    // variable, so forcing them to the same scale would squash every ordinary plot.
    val doc = parsed(PlotSpec.line(Vector((0.0, 1000.0)), "f", "x"))
    assert(js.isUndefined(doc.layout.yaxis.scaleanchor),
           "a function plot must not carry an aspect lock")
  }

  "a coordinates spec" should "lock the aspect ratio" in
  {
    // THE decisive property, and the reason Plotly was chosen over Vega-Lite: a point plot in
    // the complex plane is geometry, so unequal axes make the picture WRONG rather than ugly.
    val doc = parsed(PlotSpec.coordinates(Vector((1.0, 2.0), (-3.0, 0.5)), "poles"))
    assert(doc.layout.yaxis.scaleanchor.asInstanceOf[String] == "x")
    assert(doc.layout.yaxis.scaleratio.asInstanceOf[Double] == 1.0)
  }

  it should "draw markers rather than a line" in
  {
    val doc   = parsed(PlotSpec.coordinates(Vector((1.0, 2.0)), "v"))
    val trace = doc.data.asInstanceOf[js.Array[js.Dynamic]](0)
    assert(trace.mode.asInstanceOf[String] == "markers")
  }

  "a Bode spec" should "stack two panels on one shared logarithmic frequency axis" in
  {
    // The three properties that make this a Bode diagram rather than two line charts
    // (F_0004): a log frequency axis, two panels, and the second pinned to the first so a
    // reader can never compare a gain against the wrong frequency.
    val doc    = parsed(PlotSpec.bode(Vector((0.1, 20.0, -90.0), (1.0, 0.0, -135.0)), "1/s"))
    val traces = doc.data.asInstanceOf[js.Array[js.Dynamic]]
    assert(traces.length == 2, s"expected gain and phase, got ${traces.length}")
    assert(doc.layout.xaxis.`type`.asInstanceOf[String] == "log")
    assert(doc.layout.xaxis2.`type`.asInstanceOf[String] == "log")
    assert(doc.layout.xaxis2.matches.asInstanceOf[String] == "x",
           "the panels must share an x-axis, or panning one desynchronises the other")
    assert(traces(1).yaxis.asInstanceOf[String] == "y2",
           "phase belongs on its own scale: decibels and degrees share no units")
  }

  it should "carry the frequencies on both panels and the two curves apart" in
  {
    val doc    = parsed(PlotSpec.bode(Vector((0.1, 20.0, -90.0), (10.0, -20.0, -180.0)), "g"))
    val traces = doc.data.asInstanceOf[js.Array[js.Dynamic]]
    val gainX  = traces(0).x.asInstanceOf[js.Array[Double]].toVector
    val phaseX = traces(1).x.asInstanceOf[js.Array[Double]].toVector
    assert(gainX == Vector(0.1, 10.0) && phaseX == gainX, "both panels read the same grid")
    assert(traces(0).y.asInstanceOf[js.Array[Double]].toVector == Vector(20.0, -20.0))
    assert(traces(1).y.asInstanceOf[js.Array[Double]].toVector == Vector(-90.0, -180.0))
  }

  "a label containing JSON metacharacters" should "survive as itself" in
  {
    // Labels are user-typed expression text, so this is reachable input, not a contrived one.
    // Unescaped, it produces a malformed document and the page silently draws nothing.
    val nasty = """a "b" \ c"""
    val doc   = parsed(PlotSpec.line(Vector((0.0, 0.0)), nasty, "x"))
    assert(doc.layout.yaxis.title.asInstanceOf[String] == nasty)
  }

  it should "survive control characters too" in
  {
    val doc = parsed(PlotSpec.line(Vector((0.0, 0.0)), "a\nb\tc", "x"))
    assert(doc.layout.yaxis.title.asInstanceOf[String] == "a\nb\tc")
  }

  "a non-finite value" should "be dropped rather than emitted" in
  {
    // NaN and Infinity are not JSON. One of them anywhere makes the WHOLE document
    // unparseable, so the figure vanishes with nothing explaining why -- which is exactly
    // the diagnosis this test exists to spare.
    val spec = PlotSpec.line(Vector((0.0, 0.0), (1.0, Double.NaN), (2.0, 1.0 / 0.0)), "f", "x")
    val doc  = parsed(spec)
    val trace = doc.data.asInstanceOf[js.Array[js.Dynamic]](0)
    assert(trace.y.asInstanceOf[js.Array[Double]].toVector == Vector(0.0))
  }

  "an empty point set" should "still produce a readable document" in
  {
    val doc   = parsed(PlotSpec.line(Vector.empty, "f", "x"))
    val trace = doc.data.asInstanceOf[js.Array[js.Dynamic]](0)
    assert(trace.x.asInstanceOf[js.Array[Double]].isEmpty)
  }
