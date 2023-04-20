package it.grypho.scala.leonardo
package parser

import java.util.Dictionary
import expr._Number

class Environment
{
  var variables: Map[String, Option[_Number]] = Map()

  def get(variable: String): Option[_Number] =
    variables(variable)

  def set(variable: String, value: Option[_Number]) =
    variables + (variable -> value)

}
