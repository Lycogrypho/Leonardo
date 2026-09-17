package it.grypho.scala.leonardo
package core

/** Renders a `Double` in the JDK's format — **the JVM implementation** (issue F_0003).
 *
 *  Trivial here because the JDK's format is, by definition, what `toString` produces.  The
 *  Scala.js counterpart is where the work is: see `core/js-src`.
 */
private[core] object DoubleRender:

  /** @param d the value to render
   *  @return the JDK's `Double.toString` rendering of `d`
   */
  inline def render(d: Double): String = d.toString
