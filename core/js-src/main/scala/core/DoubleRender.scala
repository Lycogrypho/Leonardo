package it.grypho.scala.leonardo
package core

import scala.scalajs.js

/** Renders a `Double` in the JDK's format — **the Scala.js implementation** (issue F_0003).
 *
 *  **Scala.js renders a `Double` the way JavaScript does, not the way Java does**, and naming
 *  the Java method does not help: `d.toString`, `String.valueOf(d)` and
 *  `java.lang.Double.toString(d)` all produce the JavaScript form (verified, not assumed).
 *  The two disagree in two ways, and both surfaced in the cross-build's test run:
 *
 *   - an integral value prints as `1` rather than `1.0` — 74 failures;
 *   - the threshold for exponent notation differs.  Java switches below `1e-3` and at or above
 *     `1e7`; JavaScript switches below `1e-6` and at or above `1e21`.  So `3.0E-5` in Java is
 *     `0.00003` in JavaScript — the same number, a different language.
 *
 *  **This is not cosmetic.**  `toString` output is what `:save` writes and what the round-trip
 *  invariant re-parses, so without this a browser build would serialise a subtly different
 *  dialect from the JVM one, and a session saved in one would not be byte-identical in the
 *  other.  Reproducing the JDK's format is therefore the requirement, not an approximation of
 *  it.
 *
 *  The digits themselves need no work: JavaScript's conversion is already the shortest
 *  representation that round-trips, which is exactly what the JDK specifies.  Only the
 *  *presentation* has to be rebuilt.
 */
private[core] object DoubleRender:

  /** Java switches to exponent notation below this magnitude. */
  private val PlainLower = 1e-3

  /** Java switches to exponent notation at and above this magnitude. */
  private val PlainUpper = 1e7

  /** @param d the value to render
   *  @return `d` rendered exactly as the JDK's `Double.toString` would render it
   */
  def render(d: Double): String =
    if d.isNaN then "NaN"
    else if d.isInfinite then (if d > 0 then "Infinity" else "-Infinity")
    else if d == 0.0 then (if 1.0 / d < 0 then "-0.0" else "0.0")
    else
      val magnitude = math.abs(d)
      if magnitude < PlainLower || magnitude >= PlainUpper then scientific(d) else plain(d)

  /** Plain notation, with the fractional digit Java always emits and JavaScript omits. */
  private def plain(d: Double): String =
    val s = d.toString
    if s.indexOf('.') >= 0 then s else s + ".0"

  /** Java's "computerized scientific notation": one digit before the point, at least one
   *  after, an upper-case `E`, and no `+` on a positive exponent — `1.0E10`, `3.0E-5`.
   *
   *  `toExponential()` with no argument is the right source: it is specified to use as many
   *  digits as are needed to identify the value uniquely, which is the same shortest-
   *  round-trip rule the JDK follows, so the digits already agree and only the shape differs.
   */
  private def scientific(d: Double): String =
    val s   = d.asInstanceOf[js.Dynamic].toExponential().asInstanceOf[String]
    val at  = s.indexOf('e')
    val mantissa = s.substring(0, at)
    val exponent = s.substring(at + 1)
    val withPoint = if mantissa.indexOf('.') >= 0 then mantissa else mantissa + ".0"
    val withoutPlus = if exponent.startsWith("+") then exponent.substring(1) else exponent
    withPoint + "E" + withoutPlus
