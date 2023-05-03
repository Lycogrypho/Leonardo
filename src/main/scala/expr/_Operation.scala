package it.grypho.scala.leonardo
package expr

class _Operation(a: _Expression, b: _Expression) extends _Expression

case class Sum(a: _Expression, b: _Expression) extends _Operation(a, b)
{
  override def toString: String = f"($a + $b)"
}

//no need for subtraction, it's just the sum with the opposite
//case class Difference(a: Expr, b: expr) extends _Operation(a, b)
case class Product(a: _Expression, b: _Expression) extends _Operation(a, b)
{
  override def toString: String = f"($a * $b)"
}

case class Ratio(a: _Expression, b: _Expression) extends _Operation(a, b)
{
  override def toString: String = f"($a / $b)"
}
