package it.grypho.scala.leonardo
package equation

import core.*
import matrix.*


// AST node for the solve(eq, v) functional (parser: "solve(expr, v)").
// eq is _Expression, not _Equation, so a named equation (h := x = 5) can be
// passed directly: after Session.substitute expands h → _Equation(x, 5), eval
// pattern-matches on the concrete _Equation and delegates to solve(). If eq is
// not an _Equation at eval time (e.g. a plain expression or an _EqualityCheck),
// the node stays symbolic — only "=" builds a solvable relation, not "==".
//
// A solution set is never a concrete _Value, so eval always answers Left:
//   - no solution found/known  → Left(this)                (stays symbolic)
//   - exactly one solution     → Left(x = expr)            (an _Equation)
//   - several solutions        → Left([[x = e₁, x = e₂]])  (a row-vector _Matrix)
//
// children exposes eq as a single child (binder v excluded), so substitute/
// dependsOn/rebuild walk into it — if eq is a _Variable naming a definition,
// substitute replaces it with its body before eval runs.
case class _Solve(eq: _Expression, v: _Variable) extends _Expression:
  override def toString: String = s"solve($eq, $v)"
  override def children: List[_Expression] = List(eq)
  override def rebuild(c: List[_Expression]): _Expression = _Solve(c.head, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    eq match
      case e: _Equation =>
        solve(e, v, env) match
          case Nil           => Left(this)
          case single :: Nil => Left(single)
          case many          => Left(_Matrix(1, many.size, many.toVector))
      case _ => Left(this)
