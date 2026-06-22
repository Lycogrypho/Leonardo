package it.grypho.scala.leonardo
package parser

import expr._Number


class Environment(val precision: Int = _Number.DefaultPrecision):
  private var variables: Map[String, _Number] = Map()

  def get(variable: String): Option[_Number] =
    variables.get(variable)

  def assign(variable: String, value: _Number): Unit =
    variables = variables + (variable -> value)

  def unset(variable: String): Unit =
    variables = variables - variable

  def isBound(variable: String): Boolean =
    variables.contains(variable)

  def reset(): Unit =
    variables = Map()
