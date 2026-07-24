package it.grypho.scala.leonardo
package core


/** Companion holding the global precision constant. */
object Environment:
  /** Canonical default decimal precision for display rounding; single source of truth
   *  across all packages that convert numbers to strings.
   */
  val DefaultPrecision: Int = 5


/** Immutable variable-binding context shared across all domains.
 *
 *  Bindings map names to fully-reduced [[_Value]]s (a number, a matrix, a boolean) —
 *  never to a symbolic expression.  Kept in `core` because `eval` takes one, and `core`
 *  must not depend on any domain.
 *
 *  `withBinding` returns a new `Environment`; the original is unchanged (structural sharing).
 *
 *  @param precision decimal places used when rendering numbers
 */
class Environment(val precision: Int = Environment.DefaultPrecision,
                  private val variables: Map[String, _Value] = Map()):

  /** Returns the value bound to `variable`, or `None` if it is free.
   *  @param variable the name to look up
   */
  def get(variable: String): Option[_Value] =
    variables.get(variable)

  /** Returns `true` when `variable` has a binding in this environment.
   *  @param variable the name to test
   */
  def isBound(variable: String): Boolean =
    variables.contains(variable)

  /** Returns a new environment with `variable` bound to `value`.
   *  @param variable the name to bind
   *  @param value    the concrete value to associate
   */
  def withBinding(variable: String, value: _Value): Environment =
    new Environment(precision, variables + (variable -> value))
