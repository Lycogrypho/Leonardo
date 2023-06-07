package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Expression(implicit env: Environment)
{
  def eval(): Either[_Expression, Double]

}
