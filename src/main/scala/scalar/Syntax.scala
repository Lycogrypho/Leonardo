package it.grypho.scala.leonardo
package scalar

import core.*


/** Optional method-call syntax for the scalar algorithms.
 *
 *  Lets callers write `e.simplify()` instead of `simplify(e)`.  Kept in an `object`
 *  (and out of the `_Expression` trait) so `core` depends on no domain and the
 *  method names do not collide with the top-level package functions they forward to.
 *  Opt in with `import scalar.Syntax.*`.
 */
object Syntax:
  extension (e: _Expression)
    /** Single-pass structural simplification; see [[simplify]]. */
    def simplify(): _Expression           = it.grypho.scala.leonardo.scalar.simplify(e)
    /** Fixpoint simplification; see [[simplifyFully]]. */
    def simplifyFully(): _Expression      = it.grypho.scala.leonardo.scalar.simplifyFully(e)
    /** Distributes `*` over `+` and expands integer powers; see [[expand]]. */
    def expand(): _Expression             = it.grypho.scala.leonardo.scalar.expand(e)
    /** Single-variable derivative; see [[derive]]. */
    def derive(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.derive(e, v)
    /** Multi-variable or higher-order derivative (left-to-right fold); see [[derive]]. */
    def derive(v1: _Variable, v2: _Variable, rest: _Variable*): _Expression =
      it.grypho.scala.leonardo.scalar.derive(e, v1, v2, rest*)
    /** n-th derivative; see [[deriveN]]. */
    def deriveN(v: _Variable, n: Int): _Expression =
      it.grypho.scala.leonardo.scalar.deriveN(e, v, n)
    /** Symbolic indefinite integration; see [[integrate]]. */
    def integrate(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.integrate(e, v)
    /** Free-variable occurrence test; see [[dependsOn]]. */
    def dependsOn(v: _Variable): Boolean   = it.grypho.scala.leonardo.scalar.dependsOn(e, v)
    /** Like-term collection as a polynomial in `v`; see [[normalize]]. */
    def normalize(v: _Variable): _Expression = it.grypho.scala.leonardo.scalar.normalize(e, v)
    /** Dense polynomial coefficient vector in `v`; see [[collect]]. */
    def collect(v: _Variable): Option[Vector[_Expression]] =
      it.grypho.scala.leonardo.scalar.collect(e, v)
    /** Variable substitution; see [[substitute]]. */
    def substitute(definitions: Map[String, _Expression]): _Expression =
      it.grypho.scala.leonardo.scalar.substitute(e, definitions)
    /** Compile to a `Double => Double` closure; see [[compile]]. */
    def compile(v: _Variable): Option[Double => Double] =
      it.grypho.scala.leonardo.scalar.compile(e, v, new Environment())
    /** Uniform-grid numerical sampling; see [[sample]]. */
    def sample(v: _Variable, lo: Double, hi: Double, n: Int = 200, env: Environment = new Environment()): Vector[(Double, Double)] =
      it.grypho.scala.leonardo.scalar.sample(e, v, lo, hi, n, env)
