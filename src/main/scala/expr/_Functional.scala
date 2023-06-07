package it.grypho.scala.leonardo
package expr

import it.grypho.scala.leonardo.expr._Value
import it.grypho.scala.leonardo.expr._Variable
import it.grypho.scala.leonardo.parser.Environment


abstract class _Functional(implicit env: Environment) extends _Expression
{

}

case class _Derivative(e: _Expression, v: _Variable)(implicit env: Environment) extends _Functional
{
  override def toString: String = f"deriv($e in $v)"

  override def eval(): Either[_Expression, Double] = ???
}

case class _Integral(e: _Expression, v: _Variable)(implicit env: Environment) extends _Functional
{
  override def toString: String = f"integral($e in $v)"

  override def eval(): Either[_Expression, Double] = ???
}

case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression)(implicit env: Environment)
  extends _Functional
{
  override def toString: String = f"integral($e in $v from $low_limit to $up_limit)"

  override def eval(): Either[_Expression, Double] = ???
}

