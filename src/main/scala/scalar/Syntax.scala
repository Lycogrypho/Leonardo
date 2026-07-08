package it.grypho.scala.leonardo
package scalar

import core.*


// Optional method-call syntax for the scalar algorithms, so callers can write
// e.simplify() instead of simplify(e). Kept in an object (and out of the core
// _Expression trait) so core depends on no domain, and so these names do not
// collide with the top-level package functions they forward to. Opt in with
// `import scalar.Syntax.*`.
object Syntax:
  extension (e: _Expression)
    def simplify(): _Expression           = it.grypho.scala.leonardo.scalar.simplify(e)
    def simplifyFully(): _Expression      = it.grypho.scala.leonardo.scalar.simplifyFully(e)
    def expand(): _Expression             = it.grypho.scala.leonardo.scalar.expand(e)
    def derive(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.derive(e, v)
    def integrate(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.integrate(e, v)
    def dependsOn(v: _Variable): Boolean   = it.grypho.scala.leonardo.scalar.dependsOn(e, v)
    def substitute(definitions: Map[String, _Expression]): _Expression =
      it.grypho.scala.leonardo.scalar.substitute(e, definitions)
