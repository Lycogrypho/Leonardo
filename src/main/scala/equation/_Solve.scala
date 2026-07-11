package it.grypho.scala.leonardo
package equation

import core.*
import matrix.*


// AST node for the solve(eq, v) functional (parser: "solve(lhs = rhs, v)").
// A solution set is never a concrete _Value, so eval always answers Left:
//   - no solution found/known  → Left(this)                (stays symbolic)
//   - exactly one solution     → Left(x = expr)            (an _Equation)
//   - several solutions        → Left([[x = e₁, x = e₂]])  (a row-vector _Matrix)
// The row-vector display is an eval RESULT, like _Bool's "true" — it is not
// required to re-parse; the _Solve node itself round-trips through toString.
//
// children exposes the equation's two sides (the binder v is excluded, exactly as
// in _Derivative), so substitute/dependsOn work through the equation while the
// solve variable is never rewritten.
case class _Solve(eq: _Equation, v: _Variable) extends _Expression:
  override def toString: String = s"solve($eq, $v)"
  override def children: List[_Expression] = List(eq.lhs, eq.rhs)
  override def rebuild(c: List[_Expression]): _Expression = _Solve(_Equation(c.head, c(1)), v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    solve(eq, v, env) match
      case Nil           => Left(this)
      case single :: Nil => Left(single)
      case many          => Left(_Matrix(1, many.size, many.toVector))
