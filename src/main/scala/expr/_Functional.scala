package it.grypho.scala.leonardo
package expr

import it.grypho.scala.leonardo.expr._Value
import it.grypho.scala.leonardo.expr._Variable


class _Functional extends _Expression
{

}

case class _Derivative(e: _Expression, v: _Variable) extends _Functional
{
  override def toString: String = f"deriv($e in $v)"
}

case class _Integral(e: _Expression, v: _Variable) extends _Functional
{
  override def toString: String = f"integral($e in $v)"
}

case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional
{
  override def toString: String = f"integral($e in $v from $low_limit to $up_limit)"
}

