package it.grypho.scala.leonardo
package equation

import core.*
import matrix.*


/** AST node for `solveSystem(equations, v1, v2, ...)` in the grammar.
 *
 *  `equations` is expected to be a `_Matrix` whose elements are [[_Equation]] nodes --
 *  e.g. `[[2*x + y = 5, x + 3*y = 10]]`.  After the REPL substitutes any named
 *  matrix binding, `eval` extracts the equations and delegates to [[solveSystem]].
 *
 *  The solve variables are binders (excluded from `children` / `substitute` traversal),
 *  exactly as in `_Derivative` and `_Integral` -- solving for `x` does not substitute
 *  `x` itself.
 *
 *  The equations may outnumber the unknowns (F_0242): a consistent system is solved, and
 *  [[declineReason]] says why any other stays unevaluated.
 *
 *  `eval` result shape (mirrors [[_Solve]]):
 *  - no solution (singular, nonlinear, inconsistent, ...) -> `Left(this)` (stays symbolic)
 *  - single variable                        -> `Left(v = expr)`
 *  - multiple variables                     -> `Left([[v1 = e1, v2 = e2, ...]])`
 *
 *  `toString` is `"solveSystem(equations, v1, v2, ...)"` -- round-trips through the parser.
 *
 *  @param equations a `_Matrix` expression whose elements must be [[_Equation]] nodes
 *  @param variables the solve variables (binders -- excluded from `children`)
 */
case class _SolveSystem(equations: _Expression, variables: List[_Variable]) extends _Expression:
  override def toString: String =
    s"solveSystem($equations, ${variables.mkString(", ")})"
  override def children: List[_Expression] = List(equations)
  override def rebuild(c: List[_Expression]): _Expression = _SolveSystem(c.head, variables)

  /** Why the system is left unevaluated, or `None` when it is solved (F_0242): fewer equations
   *  than unknowns, not linear, no unique solution, inconsistent (naming the equation), or
   *  over-determined with symbolic coefficients.  Read by the REPL's decline note.
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = equations match
    case m: _Matrix =>
      val eqs = m.elems.collect { case e: _Equation => e }
      if eqs.size != m.elems.size then Some("every entry of the first argument must be an equation")
      else solveSystemDetailed(eqs.toList, variables, env).left.toOption
    case _ => None

  override def eval(env: Environment): Either[_Expression, _Value] =
    // equations must be a _Matrix whose every element is an _Equation node.
    // We do NOT call equations.eval(env) here -- the equation sides must stay
    // symbolic in the solve variables (same reason _Solve doesn't pre-evaluate
    // its _Equation: doing so might reduce the equation to _Bool).
    equations match
      case m: _Matrix =>
        val eqs = m.elems.collect { case e: _Equation => e }
        // More equations than unknowns are solved when consistent (F_0242); `solveSystem`
        // decides, so the counts are not compared here.
        if eqs.size != m.elems.size then Left(this)
        else
          solveSystem(eqs.toList, variables, env) match
            case None                      => Left(this)
            case Some(sol :: Nil)          => Left(sol)
            case Some(sols)                => Left(_Matrix(1, sols.size, sols.toVector))
      case _ => Left(this)
