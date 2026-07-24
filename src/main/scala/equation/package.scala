package it.grypho.scala.leonardo
/** Equation domain: relations, equality checks, and solvers.
 *
 *  The core types are:
 *  - [[_Equation]] -- a `lhs = rhs` relation; reduces to `_Bool` when both sides are
 *    concrete; marked `_ElementWise` so `derive`/`simplify`/`expand` apply to both sides.
 *  - [[_EqualityCheck]] -- a `lhs == rhs` test; always reduces to `_Bool` on concrete
 *    operands but is NOT solvable (use `=` for equations to solve).
 *  - [[_Solve]] -- AST node for `solve(eq, v)` in the grammar; delegates to [[solve]].
 *  - [[_SolveSystem]] -- AST node for `solveSystem(eqs, v1, v2, ...)`;
 *    delegates to [[solveSystem]].
 *  - [[solve]] -- multi-tier solver: linear, quadratic, matrix-unknown (inverse kernel
 *    and Kronecker vectorization), and numeric bisection fallback.
 *  - [[solveSystem]] -- n-by-n linear system solver: dense Gaussian elimination with
 *    partial pivoting, or symbolic Gaussian elimination when coefficients are not numeric.
 *
 *  Layering: `equation` imports `core.*`, `scalar.*`, and `matrix.*`.
 *  Nothing in those packages imports `equation`; `parser` and `cli` are the consumers.
 */
package equation
