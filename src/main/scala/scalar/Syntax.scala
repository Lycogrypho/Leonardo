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
    def derive(v1: _Variable, v2: _Variable, rest: _Variable*): _Expression =
      it.grypho.scala.leonardo.scalar.derive(e, v1, v2, rest*)
    def deriveN(v: _Variable, n: Int): _Expression =
      it.grypho.scala.leonardo.scalar.deriveN(e, v, n)
    def integrate(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.integrate(e, v)
    def dependsOn(v: _Variable): Boolean   = it.grypho.scala.leonardo.scalar.dependsOn(e, v)
    def normalize(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.normalize(e, v)
    def collect(v: _Variable): Option[Vector[_Expression]] =
      it.grypho.scala.leonardo.scalar.collect(e, v)
    def substitute(definitions: Map[String, _Expression]): _Expression =
      it.grypho.scala.leonardo.scalar.substitute(e, definitions)
    def compile(v: _Variable): Option[Double => Double] =
      it.grypho.scala.leonardo.scalar.compile(e, v, new Environment())
    def sample(v: _Variable, lo: Double, hi: Double, n: Int = 200, env: Environment = new Environment()): Vector[(Double, Double)] =
      it.grypho.scala.leonardo.scalar.sample(e, v, lo, hi, n, env)
