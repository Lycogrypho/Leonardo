package it.grypho.scala.leonardo
package core


// Foundation of every expression tree, shared across all domains (scalar algebra,
// linear algebra, logic, …). It knows only how to evaluate itself; domain-specific
// operations (simplify, derive, …) are added as extension methods in each domain
// package so that core depends on nothing downstream.
trait _Expression:
  // eval reduces an expression under an environment. Right holds a fully-reduced
  // concrete value (_Value: a number now, a matrix/boolean/… later); Left holds an
  // expression that could not be fully reduced and stays symbolic.
  def eval(env: Environment): Either[_Expression, _Value]


// A fully-reduced, concrete result of evaluation: a number now, later a matrix,
// boolean, truth value, … Distinct from a symbolic atom such as a free variable.
trait _Value extends _Expression


// Collapse an eval result back to a plain expression. Both branches are already
// _Expression (a _Value is one too), so this just discards the Left/Right tag —
// used when rebuilding a symbolic node from partially-reduced operands.
extension (result: Either[_Expression, _Value])
  def toExpression: _Expression = result match
    case Left(e)  => e
    case Right(v) => v
