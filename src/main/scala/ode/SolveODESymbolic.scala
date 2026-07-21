package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


// Symbolic (closed-form) tier for the first-order IVP  y' = rhs(t, y),  y(t₀) = y₀.
// Recognises the constant-coefficient linear shape  y' = a·y + b  where a and b are
// free of the independent variable t (they may hold other free parameters). This
// subsumes the pure-exponential case y' = a·y (b = 0) and the trivial y' = b (a = 0):
//
//   y' = a·y      →  y(t) = y₀·e^{a(t−t₀)}
//   y' = b        →  y(t) = y₀ + b·(t−t₀)
//   y' = a·y + b  →  y(t) = (y₀ + b/a)·e^{a(t−t₀)} − b/a        (a ≠ 0)
//
// The independent-variable slot t is filled with `target`, so the result is y(target)
// directly (τ = target − t₀). Returns Some(expr) — numeric once target/y₀/t₀ fold,
// symbolic (e.g. in a free target or a parameter) otherwise — or None when the shape
// is not recognised, so the caller falls back to RK4.
//
// Linearity in y comes from `collect` (dense coefficients of rhs in y): a degree ≤ 1
// result gives (b, a) directly, and a degree ≥ 2 result (or a form collect cannot
// expand, e.g. sin(y)) yields None. The constant-coefficient requirement is enforced
// by rejecting any a or b that depends on the independent variable.
def solveODESymbolic(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                     t0: _Expression, y0: _Expression, target: _Expression,
                     env: Environment): Option[_Expression] =
  collect(rhs, depVar) match
    case Some(coeffs) if coeffs.length <= 2 =>
      val b = coeffs.headOption.getOrElse(_Number(0))          // constant term
      val a = if coeffs.length == 2 then coeffs(1) else _Number(0)  // coefficient of y
      // Constant-coefficient guard: a, b must not depend on t (dependence on y is
      // already excluded — collect gives a linear-in-y form).
      if dependsOn(a, indepVar) || dependsOn(b, indepVar) then None
      else
        val aZero = simplifyFully(a) == _Number(0.0)
        val bZero = simplifyFully(b) == _Number(0.0)
        val tau   = Sum(target, Product(_Number(-1), t0))       // target − t₀
        val sol =
          if bZero then Product(y0, Exp(Product(a, tau)))        // y₀·e^{a·τ}
          else if aZero then Sum(y0, Product(b, tau))            // y₀ + b·τ
          else                                                   // (y₀ + b/a)·e^{a·τ} − b/a
            val boa = Ratio(b, a)
            Sum(Product(Sum(y0, boa), Exp(Product(a, tau))), Product(_Number(-1), boa))
        Some(simplifyFully(sol))
    case _ => None
