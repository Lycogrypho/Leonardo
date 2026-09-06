package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


/** Closed-form tier for linear first-order IVPs `y' = a(t)*y + b(t)`.
 *
 *  The right-hand side must be linear in the dependent variable (`collect(rhs, depVar)`
 *  yields at most a degree-1 coefficient vector `[b, a]`); non-linear shapes such as
 *  `sin(y)` or `y^2` return `None` (the caller falls back to RK4).
 *
 *  Two sub-tiers:
 *
 *  1. **Constant coefficients** (`a`, `b` free of the independent variable `t`) — the
 *     exact `tau = target - t0` forms, avoiding any substitution:
 *     - `y' = a*y`      (b = 0): `y = y0 * exp(a*tau)`
 *     - `y' = b`        (a = 0): `y = y0 + b*tau`
 *     - `y' = a*y + b`  (general): `y = (y0 + b/a) * exp(a*tau) - b/a`
 *
 *  2. **Variable coefficients** (`a` or `b` depends on `t`) — the integrating-factor
 *     method.  With `A = integral(a dt)` and `mu = exp(-A)` the equation `(mu*y)' = mu*b`
 *     integrates to `mu*y = Q + C`, `Q = integral(mu*b dt)`; the initial condition fixes
 *     `C = mu(t0)*y0 - Q(t0)`, giving
 *     {{{
 *     y(target) = (Q(target) - Q(t0) + y0*mu(t0)) / mu(target)
 *     }}}
 *     This closes whenever both `integral(a dt)` and `integral(mu*b dt)` reduce to a
 *     closed form (it reuses the full indefinite-integration engine, so e.g. `mu*b = t*e^t`
 *     is handled by integration by parts); if either stays symbolic, this tier returns
 *     `None` and the caller falls back to RK4.
 *
 *  In both tiers the result stays symbolic when `target`, `y0`, `t0`, or a coefficient is
 *  free, and folds to a `_Number` once everything is concrete.
 *
 *  @param rhs      the right-hand side of the ODE `y' = rhs`
 *  @param depVar   the dependent variable (`y`)
 *  @param indepVar the independent variable (`t`)
 *  @param t0       the initial time (may be symbolic)
 *  @param y0       the initial value (may be symbolic)
 *  @param target   the evaluation point (may be symbolic)
 *  @param env      the evaluation environment
 *  @return `Some(closed-form expression for y(target))`, or `None` if not recognised
 */
def solveODESymbolic(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                     t0: _Expression, y0: _Expression, target: _Expression,
                     env: Environment): Option[_Expression] =
  collect(rhs, depVar) match
    case Some(coeffs) if coeffs.length <= 2 =>
      val b = coeffs.headOption.getOrElse(_Number(0))          // constant term  (free of y)
      val a = if coeffs.length == 2 then coeffs(1) else _Number(0)  // coefficient of y
      if dependsOn(a, indepVar) || dependsOn(b, indepVar) then
        // Variable coefficients: integrating-factor method (falls through to None -> RK4
        // when either required integral stays symbolic).
        integratingFactorSolution(a, b, indepVar, t0, y0, target)
      else
        // Constant coefficients: the exact tau-form closed solutions.
        constantCoefficientSolution(a, b, t0, y0, target)
    // Not linear in y: try separation of variables (issue 3.6).
    case _ => separableSolution(rhs, depVar, indepVar, t0, y0, target, env)


/** Closed form for a separable equation `y' = f(t)·g(y)` — issue 3.6.
 *
 *  Separate, integrate both sides, fix the constant from the initial condition, then solve
 *  for `y`:
 *
 *  {{{
 *  G(y) = integral(dy / g(y))      F(t) = integral(f dt)
 *  C    = G(y0) - F(t0)
 *  y(target) solves  G(y) = F(target) + C
 *  }}}
 *
 *  **Separation divides by `g(y)`, and that is the correctness trap.**  If `g(y0) = 0` then
 *  `y ≡ y0` is an *equilibrium* solution: the trajectory never moves, and the separated form
 *  is invalid because it divides by zero.  The integrals would still produce *something*,
 *  and that something would be confidently wrong — so the equilibrium is checked **before**
 *  either integral is attempted.  A `g(y0)` whose sign cannot be determined refuses outright
 *  rather than dividing by a possible zero.
 *
 *  The final solve is a **root-find**, not a symbolic inversion: with numeric `t0`/`y0`/
 *  `target` — the usual call — `scalar.compile` plus bisection closes it without `ode`
 *  needing to reach into `equation`.  A fully symbolic case declines and falls through to
 *  RK4, which is the existing behaviour rather than a regression.
 */
private def separableSolution(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                              t0: _Expression, y0: _Expression, target: _Expression,
                              env: Environment): Option[_Expression] =
  for
    (f, g) <- splitSeparable(rhs, depVar, indepVar)
    result <- equilibriumOr(g, depVar, y0, env) {
                separateAndSolve(f, g, depVar, indepVar, t0, y0, target, env)
              }
  yield result

/** Returns the equilibrium solution when `g(y0) = 0`, refuses when its sign is unknown, and
 *  otherwise runs `whenMoving`.
 *
 *  Gating on [[scalar.sign]] rather than on a bare numeric test is what makes this correct
 *  for a symbolic `g`: `Some(0)` is *provably* zero, `None` means undeterminable, and only a
 *  strict sign licenses the division that separation performs.
 */
private def equilibriumOr(g: _Expression, depVar: _Variable, y0: _Expression,
                          env: Environment)(whenMoving: => Option[_Expression]): Option[_Expression] =
  val gAtY0 = simplifyFully(substitute(g, Map(depVar.variable -> y0)))
  sign(gAtY0, env) match
    case Some(0) => Some(simplifyFully(y0))   // equilibrium: the solution is the constant y0
    case Some(_) => whenMoving
    case None    => None                      // may be zero -- do not divide by it

/** The separation proper, once `g(y0)` is known non-zero. */
private def separateAndSolve(f: _Expression, g: _Expression, depVar: _Variable,
                             indepVar: _Variable, t0: _Expression, y0: _Expression,
                             target: _Expression, env: Environment): Option[_Expression] =
  val bigG = integrate(simplifyFully(Ratio(_Number(1), g)), depVar)
  if hasIntegral(bigG) then None
  else
    val bigF = integrate(f, indepVar)
    if hasIntegral(bigF) then None
    else
      val dv = depVar.variable
      val iv = indepVar.variable
      def atY(x: _Expression): _Expression = substitute(bigG, Map(dv -> x))
      def atT(x: _Expression): _Expression = substitute(bigF, Map(iv -> x))
      // G(y) - (F(target) + C) = 0, with C = G(y0) - F(t0).
      val c      = Sum(atY(y0), Product(_Number(-1), atT(t0)))
      val rhsVal = simplifyFully(Sum(atT(target), c))
      solveForY(bigG, depVar, rhsVal, y0, env)

/** Solves `G(y) = rhs` for `y` numerically, bracketing outward from `y0`.
 *
 *  A root-find rather than a symbolic inversion: it needs only `scalar.compile`, so the
 *  separable tier stays inside `ode` + `scalar`.  Symbolic inversion of `G` is the deferred
 *  half of 3.6 and would be what requires reaching into `equation`.
 */
private def solveForY(bigG: _Expression, depVar: _Variable, rhs: _Expression,
                      y0: _Expression, env: Environment): Option[_Expression] =
  for
    start  <- foldNumber(y0, env)
    target <- foldNumber(rhs, env)
    fn     <- compile(bigG, depVar, env)
    root   <- bracketAndBisect(y => fn(y) - target, start)
  yield _Number(root)

/** Evaluates to a finite number, or `None`. */
private def foldNumber(e: _Expression, env: Environment): Option[Double] =
  e.eval(env) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
    case _                                              => None

/** Brackets a sign change outward from `start`, then bisects.
 *
 *  Expanding from `y0` rather than scanning a fixed window matters: the solution is the
 *  branch *through the initial condition*, and a global scan could return a root on a
 *  different branch — `y² = …` admits `±√`, and those are different solutions.
 */
private def bracketAndBisect(h: Double => Double, start: Double): Option[Double] =
  val h0 = h(start)
  if h0.isNaN then None
  else if math.abs(h0) < BisectTolerance then Some(start)
  else
    // Grow a bracket geometrically on both sides, stopping at the first sign change; the
    // nearest one is the branch containing y0.
    val steps = LazyList.iterate(BisectInitialStep)(_ * 2).take(BisectBrackets)
    val found = steps.flatMap(d => List(start + d, start - d))
                     .find(y => { val hy = h(y); !hy.isNaN && hy * h0 < 0 })
    found.map { other =>
      var lo = math.min(start, other)
      var hi = math.max(start, other)
      var i  = 0
      while i < BisectIterations && hi - lo > BisectTolerance do
        val mid = (lo + hi) / 2
        if h(lo) * h(mid) <= 0 then hi = mid else lo = mid
        i += 1
      (lo + hi) / 2
    }

private val BisectInitialStep = 1e-3
private val BisectBrackets    = 40
private val BisectIterations  = 200
private val BisectTolerance   = 1e-12

/** Recognises `y' = f(t)·g(y)`, returning the two factors.
 *
 *  **Structural, deliberately not `collect`**: `collect(rhs, depVar)` yields *polynomial
 *  coefficients in `y`*, which is the wrong decomposition entirely — it answers "is this
 *  linear in y", not "does this factor into a `t` part and a `y` part".
 *
 *  Handles the degenerate ends too: `y' = f(t)` alone is separable with `g = 1`, and
 *  `y' = g(y)` alone with `f = 1` (which is also autonomous).
 */
private def splitSeparable(rhs: _Expression, depVar: _Variable,
                           indepVar: _Variable): Option[(_Expression, _Expression)] =
  val one = _Number(1)
  rhs match
    case Product(a, b) if !dependsOn(a, depVar) && !dependsOn(b, indepVar) => Some((a, b))
    case Product(a, b) if !dependsOn(b, depVar) && !dependsOn(a, indepVar) => Some((b, a))
    case Ratio(a, b)   if !dependsOn(a, depVar) && !dependsOn(b, indepVar) =>
      Some((a, simplifyFully(Ratio(one, b))))
    case e if !dependsOn(e, depVar)   => Some((e, one))    // y' = f(t)
    case e if !dependsOn(e, indepVar) => Some((one, e))    // y' = g(y), autonomous
    case _                            => None

/** Closed form for the constant-coefficient case `y' = a*y + b` (`a`, `b` free of `t`). */
private def constantCoefficientSolution(a: _Expression, b: _Expression,
                                        t0: _Expression, y0: _Expression,
                                        target: _Expression): Option[_Expression] =
  val aZero = simplifyFully(a) == _Number(0.0)
  val bZero = simplifyFully(b) == _Number(0.0)
  val tau   = Sum(target, Product(_Number(-1), t0))       // target - t0
  val sol =
    if bZero then Product(y0, Exp(Product(a, tau)))        // y0 * exp(a * tau)
    else if aZero then Sum(y0, Product(b, tau))            // y0 + b * tau
    else                                                   // (y0 + b/a) * exp(a * tau) - b/a
      val boa = Ratio(b, a)
      Sum(Product(Sum(y0, boa), Exp(Product(a, tau))), Product(_Number(-1), boa))
  Some(simplifyFully(sol))

/** Closed form for the variable-coefficient case `y' = a(t)*y + b(t)` via the integrating
 *  factor `mu = exp(-integral(a dt))`; `None` when either integral does not close.
 *
 *  @param a        the coefficient of `y` (may depend on `indepVar`, free of `depVar`)
 *  @param b        the forcing term (may depend on `indepVar`, free of `depVar`)
 *  @param indepVar the independent variable `t`
 *  @param t0       the initial time
 *  @param y0       the initial value
 *  @param target   the evaluation point
 *  @return `Some(y(target))` when both integrals close, `None` otherwise
 */
private def integratingFactorSolution(a: _Expression, b: _Expression, indepVar: _Variable,
                                      t0: _Expression, y0: _Expression,
                                      target: _Expression): Option[_Expression] =
  val bigA = integrate(a, indepVar)                        // A(t) = integral(a dt)
  if hasIntegral(bigA) then None
  else
    val mu   = simplifyFully(Exp(Product(_Number(-1), bigA)))   // mu(t) = exp(-A(t))
    val bigQ = integrate(simplifyFully(Product(mu, b)), indepVar)   // Q(t) = integral(mu*b dt)
    if hasIntegral(bigQ) then None
    else
      val iv = indepVar.variable
      def at(e: _Expression, x: _Expression): _Expression = substitute(e, Map(iv -> x))
      // y(target) = (Q(target) - Q(t0) + y0*mu(t0)) / mu(target)
      val numer = Sum(Sum(at(bigQ, target), Product(_Number(-1), at(bigQ, t0))),
                      Product(y0, at(mu, t0)))
      Some(simplifyFully(Ratio(numer, at(mu, target))))

/** True when `e` still contains an unresolved `scalar._Integral` node (an integral that
 *  the integration engine could not reduce to a closed form). */
private def hasIntegral(e: _Expression): Boolean = e match
  case _: _Integral => true
  case _            => e.children.exists(hasIntegral)
