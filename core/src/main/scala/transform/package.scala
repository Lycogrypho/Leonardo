package it.grypho.scala.leonardo
/** Transform domain: the Laplace and Fourier transforms, the z-transform, and their inverses.
 *
 *  **Every transform here is one-sided (unilateral)**, and deliberately so: `laplace` integrates
 *  from `0`, `fourier` is derived from it, and the z-transform sums from `n = 0` (issue 6.33
 *  Decision A).  A bilateral transform would have to carry a **region of convergence** on every
 *  result — without one the same `X(z)` inverts to a causal or an anti-causal signal, so the
 *  inverse would have to guess — which is a new piece of state on a value and is therefore its
 *  own issue (6.37) rather than a flag here.
 *
 *  The AST nodes and their algorithm functions are:
 *  - [[_Laplace]] / [[laplaceOf]] -- `laplace(e, t, s)` computes `L{e(t)}` as a
 *    function of the complex frequency `s` via a rule table (constant, linearity,
 *    power, exponential, sine/cosine, first-shift, second-shift, derivative-of-transform).
 *  - [[_Fourier]] / [[fourierOf]] -- `fourier(e, t, w)` computes `F{e(t)}` by
 *    evaluating the Laplace transform and substituting `s -> i*w`.
 *    Results are generally complex-valued.
 *  - [[_InverseLaplace]] / [[inverseLaplaceOf]] -- `invlaplace(f, s, t)` computes
 *    `L^-1{f(s)}` as a function of `t` via partial-fraction inversion: linear,
 *    quadratic (completing the square), and higher-degree rational denominators
 *    (companion-matrix root-finding + residue formula).
 *  - [[_ZTransform]] / [[zTransformOf]] -- `ztrans(x, n, z)` computes the one-sided
 *    `X(z) = sum(k >= 0) x[k]*z^-k` via a rule table (constant, linearity, unit step,
 *    geometric, exponential, sine/cosine, multiply-by-`n`).
 *  - [[_InverseZTransform]] / [[inverseZTransformOf]] -- `invztrans(X, z, n)` recovers `x[n]`
 *    by decomposing `X(z)/z` over real poles, each term `z/(z-r)^j` inverting to
 *    `C(n, j-1) * r^(n-j+1)`.
 *
 *  **`Z{c} = c*z/(z-1)`, not `c/z`.**  The Laplace analogue `L{c} = c/s` does not carry over:
 *  a constant *sequence* is `c` at every index, so its transform is `c` times the unit step's.
 *  It is the entry most easily got wrong when reading the two tables side by side.
 *
 *  All of them stay symbolic when no rule applies (the fixpoint convention shared with
 *  `scalar.derive` / `scalar.integrate`).
 *
 *  Layering: `transform` imports `core.*` and `scalar.*`; nothing in those packages
 *  imports `transform`.  The `parser` package is the consumer.
 */
package transform
