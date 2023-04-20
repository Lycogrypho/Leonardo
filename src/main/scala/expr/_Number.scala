package it.grypho.scala.leonardo
package expr

case class _Number(d: String) extends _Value
{
  override def toString(): String = d

  val value = d.toDouble

}
