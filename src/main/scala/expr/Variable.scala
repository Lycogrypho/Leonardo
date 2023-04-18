package it.grypho.scala.leonardo
package expr

case class Variable(vriable: String, value: Option[Number]) extends Value

