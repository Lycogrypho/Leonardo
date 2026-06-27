package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Expression:
  def eval(env: Environment): Either[_Expression, Double]
  def simplify(): _Expression            = it.grypho.scala.leonardo.expr.simplify(this)
  def expand(): _Expression              = it.grypho.scala.leonardo.expr.expand(this)
  def derive(v: _Variable): _Expression = it.grypho.scala.leonardo.expr.derive(this, v)