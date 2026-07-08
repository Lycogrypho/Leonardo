package it.grypho.scala.leonardo
package core


object Environment:
  // Canonical default decimal precision for rounding values; single source of truth.
  val DefaultPrecision: Int = 5


// Immutable variable-binding context shared across all domains. Bindings map names
// to fully reduced values (_Value): a number now, a matrix/boolean/… later — never
// a symbolic expression. Kept in core (eval takes one) so it cannot depend on any
// one domain. withBinding returns a new Environment; the original is unchanged.
class Environment(val precision: Int = Environment.DefaultPrecision,
                  private val variables: Map[String, _Value] = Map()):

  def get(variable: String): Option[_Value] =
    variables.get(variable)

  def isBound(variable: String): Boolean =
    variables.contains(variable)

  def withBinding(variable: String, value: _Value): Environment =
    new Environment(precision, variables + (variable -> value))
