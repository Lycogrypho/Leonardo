package it.grypho.scala.leonardo
package parser

import java.util.Dictionary
import expr._Number

class Environment(var precision: Int = 5)
{
  //TODO: how should constants be managed?
  var variables: Map[String, Option[_Number]] = Map()

  def get(variable: String): Option[_Number] =
    variables(variable)

  def set(variable: String, value: Option[_Number]) =
  {
    //TODO: at the moment no check is done wether the variable already exists
    variables = variables + (variable -> value)
    //print(s"added $variable \r\n actual list: \r\n\t${variables.mkString(";\r\n\t")}\r\n")
  }
  //verify if variable has to be updated instead

  // TODO: precision should be set at creation time or a var to be updated - evaluate alternatives
  //val precision: Int = 5 // number of decimals used in calculation

}
