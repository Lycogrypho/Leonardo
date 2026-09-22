package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** The shared scheme-name list and the JVM style table must agree (issue F_0003 phase 2).
 *
 *  `ColorSchemes.Names` lives in the shared tree because `Session` validates the `colors`
 *  argument against it; `ColorScheme.All` lives on the JVM because its values are JLine
 *  styles.  Two hand-maintained lists of the same set is how they drift apart — add a scheme
 *  to one and the REPL either accepts a name it cannot render, or refuses one it can.
 *
 *  This is the standing guard for that, in the spirit of the `CheckCharset` guard: the split
 *  was a one-time repair, and nothing else would keep it true.
 */
class ColorSchemeNamesTest extends AnyFlatSpec:

  "the shared scheme names" should "match the JVM style table exactly" in
  {
    assert(ColorSchemes.Names.toSet == ColorScheme.All.keySet,
           s"shared names ${ColorSchemes.Names.sorted} vs styled ${ColorScheme.All.keySet.toList.sorted}")
  }

  "the default scheme" should "be one the style table can render" in
  {
    assert(ColorScheme.All.contains(ColorSchemes.Default))
  }
