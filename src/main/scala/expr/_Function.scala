package it.grypho.scala.leonardo
package expr

class _Function extends _Expression
{
  override def toString: String = super.toString

}
case class Exp(e: _Expression) extends _Expression
{
  override def toString: String = f"exp($e)"
}

case class Log(e: _Expression) extends _Expression
{
  override def toString: String = f"log($e)"
}

case class Sin(e: _Expression) extends _Expression
{
  override def toString: String = f"sin($e)"
}

case class Cos(e: _Expression) extends _Expression
{
  override def toString: String = f"cos($e)"
}

case class Tg(e: _Expression) extends _Expression
{
  override def toString: String = f"tg($e)"
}