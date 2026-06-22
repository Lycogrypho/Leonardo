package it.grypho.scala.leonardo
package expr

import parser.Environment


abstract class _Functional extends _Expression


case class _Derivative(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"deriv($e in $v)"
  override def eval(env: Environment): Either[_Expression, Double] = ???


case class _Integral(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"integral($e in $v)"
  override def eval(env: Environment): Either[_Expression, Double] = ???


case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional:
  override def toString: String = s"integral($e in $v from $low_limit to $up_limit)"
  override def eval(env: Environment): Either[_Expression, Double] = ???
