package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


/** Closed-form tier for a constant-coefficient linear **system** `y' = A·y + b`, `y(t0) = y0`.
 *
 *  `y` is an `n×1` column, `A` a square `n×n` matrix and `b` an `n×1` column, both constant.
 *  The solution is the augmented matrix exponential
 *  {{{
 *  exp([[A, b], [0, 0]]·τ) = [[Φ, Γ], [0, 1]],   y(target) = Φ·y0 + Γ,   τ = target − t0
 *  }}}
 *  computed by [[core._MatrixValue.augmentedExp]], the kernel `control.c2dExact` also uses, so
 *  the two agree by construction (F_0006).  The homogeneous `y' = A·y` is the `b = 0` case.
 *
 *  **The vector-ness is read from `y0`**, the one operand that must be a vector: the tier runs
 *  only when `y0` evaluates to a column, so a scalar problem never reaches it, and the `_ODE`
 *  node, its binders and its `toString` need no change.
 *
 *  **Recognition is structural** and accepts `A·y`, `A·y + b` and `b + A·y` (`A·y − b` is
 *  `A·y + (−1)·b`).  A product is either the scalar `Product` — what a coefficient *bound* to
 *  a matrix builds — or a node carrying [[core._MatrixProductShaped]], which is what a literal
 *  matrix builds; sums likewise through [[core._MatrixSumShaped]].  The `core` traits are what
 *  keep `ode` free of a `matrix` import.
 *
 *  **Declines**, returning `None` so the caller falls through (a vector problem reaches F_0051's
 *  certified RK4, `odeCertified`): a time-varying `A(t)` or `b(t)`
 *  (no closed form in general — it needs the Magnus series), a coefficient that is not a square
 *  dense matrix (a *scalar* coefficient is left to the scalar tier, which is correct for it),
 *  any non-conforming `b` or `y0`, and a `τ` that does not fold to a number, since
 *  [[core._MatrixValue.expm]] is a numeric kernel.  Backward time needs nothing special:
 *  `e^(Aτ)` is defined for either sign.  An exact operand demotes to `Double`, the
 *  `lu`/`qr`/`expm` precedent.
 *
 *  @param rhs      the right-hand side of `y' = rhs`
 *  @param depVar   the dependent variable (`y`)
 *  @param indepVar the independent variable (`t`)
 *  @param t0       the initial time
 *  @param y0       the initial state, an `n×1` column
 *  @param target   the evaluation point
 *  @param env      the evaluation environment
 *  @return `Some(y(target))` as an `n×1` matrix, or `None` when the problem is not such a system
 */
def solveODESystem(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                   t0: _Expression, y0: _Expression, target: _Expression,
                   env: Environment): Option[_MatrixValue] =
  for
    state           <- denseOf(y0, env)
    if state.cols == 1
    n                = state.rows
    (coef, forcing) <- linearSystemShape(rhs, depVar)
    if !dependsOn(coef, indepVar) && !forcing.exists(dependsOn(_, indepVar))
    a               <- denseOf(coef, env)
    if a.rows == n && a.cols == n
    b               <- forcing.fold(Option(_MatrixValue(n, 1, new Array[Double](n))))(denseOf(_, env))
    if b.rows == n && b.cols == 1
    tau             <- finiteNumber(Sum(target, Product(_Number(-1), t0)), env)
    (phi, gamma)    <- _MatrixValue.augmentedExp(a, b, tau)
    result           = phi.multiply(state).add(gamma)
    if result.isFinite
  yield result

/** Splits `rhs` into `(A, Some(b))` for `A·y + b` / `b + A·y`, or `(A, None)` for `A·y`.
 *
 *  `A` and `b` must be free of `depVar`; the right factor of the product must be `depVar`
 *  itself, because a matrix product does not commute and `y·A` is a different system.
 */
private def linearSystemShape(rhs: _Expression,
                              depVar: _Variable): Option[(_Expression, Option[_Expression])] =
  def coefficientOf(e: _Expression): Option[_Expression] = e match
    case Product(c, v) if v == depVar && !dependsOn(c, depVar)                         => Some(c)
    case p: _MatrixProductShaped if p.right == depVar && !dependsOn(p.left, depVar) => Some(p.left)
    case _                                                                          => None
  def operands(e: _Expression): Option[(_Expression, _Expression)] = e match
    case Sum(l, r)           => Some((l, r))
    case s: _MatrixSumShaped => Some((s.left, s.right))
    case _                   => None
  def withForcing(term: _Expression, forcing: _Expression) =
    coefficientOf(term).filter(_ => !dependsOn(forcing, depVar)).map(c => (c, Some(forcing)))

  coefficientOf(rhs).map(c => (c, None)).orElse(
    operands(rhs).flatMap((l, r) => withForcing(l, r).orElse(withForcing(r, l))))

/** Evaluates `e` to a dense matrix: a `_MatrixValue`, or a symbolic matrix whose every cell is
 *  a number — the shape an **exact** matrix keeps, demoted here as `expm` demotes it anyway. */
private def denseOf(e: _Expression, env: Environment): Option[_MatrixValue] = e.eval(env) match
  case Right(m: _MatrixValue) => Some(m)
  case Left(m: _MatrixShaped) =>
    val cells = m.children.collect { case _Number(d) => d }
    if cells.size == m.rows * m.cols then Some(_MatrixValue(m.rows, m.cols, cells.toArray)) else None
  case _ => None

/** Evaluates `e` to a finite `Double`, or `None`. */
private def finiteNumber(e: _Expression, env: Environment): Option[Double] = e.eval(env) match
  case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
  case _                                              => None
