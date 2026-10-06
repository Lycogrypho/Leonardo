package it.grypho.scala.leonardo

/** Optimization: finding, classifying and certifying optima (F_0010).
 *
 *  **The symbolic half is the point of the package.**  Almost every optimization library is
 *  numeric; what a CAS can do that they cannot is *derive* the conditions and solve them
 *  exactly.  So most of this package is symbolic:
 *
 *  - [[stationaryPoints]] solves `∇f = 0` and [[classifyStationary]] reads the Hessian there;
 *  - [[convexity]] decides convexity from the principal minors of the symbolic Hessian;
 *  - [[lagrangePoints]] handles equality constraints with Lagrange multipliers;
 *  - [[kktConditions]] states the Karush–Kuhn–Tucker conditions in the language, and
 *    [[kktPoints]] solves them by active-set enumeration.
 *
 *  Every one of them rests on [[eliminate]], a solver for the *nonlinear* polynomial systems
 *  those conditions produce — `equation.solveSystem` is linear-only, which was the survey
 *  finding that shaped the whole entry.
 *
 *  A modest numeric tier, [[minimize]], covers what has no closed form: gradient descent,
 *  Newton, BFGS and a projected BFGS for box constraints, each named explicitly, and each
 *  **declining** rather than returning a point it could not certify.  Large-scale numerical
 *  optimization is out of scope, and exact linear programming belongs to F_0011.
 *
 *  **Refusal is the common thread.**  A list of stationary points is read as *all* of them,
 *  so an incomplete list is a confident wrong answer; the solver declines when it cannot
 *  prove completeness.  `convex` answers `false` only with a witness.  `minimize` checks a
 *  gradient certificate before answering.
 *
 *  Layering: imports `core`, `scalar`, `matrix`, `equation`, `vector` and `logic`, and
 *  nothing imports it except `parser` (and, later, F_0066's SQP).
 */
package object optimize
