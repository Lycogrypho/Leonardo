package it.grypho.scala.leonardo
package equation

import core.*
import matrix.*


// AST node for the solveSystem(equations, v₁, v₂, …) functional.
// `equations` is expected to be a _Matrix whose elements are _Equation nodes —
// e.g. [[2*x + y = 5, x + 3*y = 10]]. After Session.substitute expands any named
// matrix binding, eval extracts the equations and delegates to solveSystem().
//
// Variables are binders (excluded from children / substitute traversal), exactly
// as in _Derivative and _Integral — solving for x does not substitute x itself.
//
// eval result shape (mirrors _Solve):
//   - no solution (singular, nonlinear, …) → Left(this)  (stays symbolic)
//   - single variable                       → Left(v = expr)
//   - multiple variables                    → Left([[v₁ = e₁, v₂ = e₂, …]])
//
// toString: "solveSystem(equations, v₁, v₂, …)" — round-trips through the parser.
case class _SolveSystem(equations: _Expression, variables: List[_Variable]) extends _Expression:
  override def toString: String =
    s"solveSystem($equations, ${variables.mkString(", ")})"
  override def children: List[_Expression] = List(equations)
  override def rebuild(c: List[_Expression]): _Expression = _SolveSystem(c.head, variables)

  override def eval(env: Environment): Either[_Expression, _Value] =
    // equations must be a _Matrix whose every element is an _Equation node.
    // We do NOT call equations.eval(env) here — the equation sides must stay
    // symbolic in the solve variables (same reason _Solve doesn't pre-evaluate
    // its _Equation: doing so might reduce the equation to _Bool).
    equations match
      case m: _Matrix =>
        val eqs = m.elems.collect { case e: _Equation => e }
        if eqs.size != m.elems.size || eqs.size != variables.size then Left(this)
        else
          solveSystem(eqs.toList, variables, env) match
            case None                      => Left(this)
            case Some(sol :: Nil)          => Left(sol)
            case Some(sols)                => Left(_Matrix(1, sols.size, sols.toVector))
      case _ => Left(this)
