package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Expression:
  // eval reduces an expression under an environment. Right holds a fully-reduced
  // concrete value (_Value: a number now, a matrix/boolean/… later); Left holds an
  // expression that could not be fully reduced and stays symbolic.
  def eval(env: Environment): Either[_Expression, _Value]
  def simplify(): _Expression                  = it.grypho.scala.leonardo.expr.simplify(this)
  def expand(): _Expression                    = it.grypho.scala.leonardo.expr.expand(this)
  def derive(v: _Variable): _Expression        = it.grypho.scala.leonardo.expr.derive(this, v)
  def dependsOn(v: _Variable): Boolean         = it.grypho.scala.leonardo.expr.dependsOn(this, v)


// Collapse an eval result back to a plain expression. Both branches are already
// _Expression (a _Value is one too), so this just discards the Left/Right tag —
// used when rebuilding a symbolic node from partially-reduced operands.
extension (result: Either[_Expression, _Value])
  def toExpression: _Expression = result match
    case Left(e)  => e
    case Right(v) => v