package it.grypho.scala.leonardo
/** Integral transform domain: Laplace, Fourier, and inverse Laplace transforms.
 *
 *  The three AST nodes and their algorithm functions are:
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
 *
 *  All three stay symbolic when no rule applies (the fixpoint convention shared with
 *  `scalar.derive` / `scalar.integrate`).
 *
 *  Layering: `transform` imports `core.*` and `scalar.*`; nothing in those packages
 *  imports `transform`.  The `parser` package is the consumer.
 */
package transform
