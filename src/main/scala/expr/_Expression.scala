package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Expression:
  def eval(env: Environment): Either[_Expression, Double]