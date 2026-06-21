package it.grypho.scala.leonardo
package parser

import expr._Number


class Environment(var precision: Int = 5):
  var variables: Map[String, Option[_Number]] = Map()

  def get(variable: String): Option[_Number] =
    variables.get(variable).flatten

  def set(variable: String, value: Option[_Number]): Unit =
    variables = variables + (variable -> value)
