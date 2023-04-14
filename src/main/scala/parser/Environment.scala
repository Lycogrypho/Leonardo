package it.grypho.scala.leonardo
package parser

import java.util.Dictionary

class Environment
{
  var variables: Map[String, Option[Number]] = Map()

  def get(variable: String): Option[Number] =
    variables(variable)

  def set(variable: String, value: Option[Number]) =
    variables + (variable -> value)

}
