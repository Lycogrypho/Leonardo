package it.grypho.scala.leonardo
package core


object Environment:
  // Canonical default decimal precision for rounding values; single source of truth.
  val DefaultPrecision: Int = 5


// Variable-binding context shared across all domains. Bindings map names to fully
// reduced values (_Value): a number now, a matrix/boolean/… later — never a symbolic
// expression. Kept in core (eval takes one) so it cannot depend on any one domain.
class Environment(val precision: Int = Environment.DefaultPrecision):
  private var variables: Map[String, _Value] = Map()

  def get(variable: String): Option[_Value] =
    variables.get(variable)

  def assign(variable: String, value: _Value): Unit =
    variables = variables + (variable -> value)

  def unset(variable: String): Unit =
    variables = variables - variable

  def isBound(variable: String): Boolean =
    variables.contains(variable)

  def reset(): Unit =
    variables = Map()

  def withBinding(variable: String, value: _Value): Environment =
    val copy = new Environment(precision)
    variables.foreach((k, v) => copy.assign(k, v))
    copy.assign(variable, value)
    copy
