package it.grypho.scala.leonardo
/** Control theory: transfer functions, time and frequency response, stability, state space
 *  and discretisation (issue 6.29).
 *
 *  **A transfer function is a `Ratio` in the frequency variable, not a carrier of its own**
 *  (6.29 Decision A).  A state-space model is a 1x4 `matrix._Matrix` of matrices, exactly the
 *  shape `lu`/`qr`/`eig`/`jordan` already return.  Nothing here introduces a `_Value`, because
 *  nothing needs to *read* one: every operator takes its variable explicitly, which is the
 *  house convention already used by `laplace(e, t, s)` and the ordered coordinate tuple of the
 *  `vector` package.
 *
 *  That decision moves the weight onto **documentation**, and deliberately so.  The cost of
 *  having no carrier is that nothing announces itself: there is no `_TransferFunction` in the
 *  Scaladoc for a reader to land on, and a user cannot discover [[series]] or [[feedback]]
 *  from a type.  So **every function here states which of its arguments is the frequency
 *  variable**, since the signature cannot.
 *
 *  Most of the domain is assembly rather than new mathematics — the table below is the point
 *  of the issue:
 *
 *  | Concept | Machinery it is built from |
 *  |---|---|
 *  | step / impulse response | `transform.inverseLaplaceOf`, repeated poles since 3.17 |
 *  | poles, zeros, stability | `scalar.polyRoots` over `scalar.rationalCoeffs` |
 *  | Bode / Nyquist | substitute `s -> i*w`, the `core._Complex` closure |
 *  | controllability | matrix products and `A^n` |
 *  | exact discretisation | `core._MatrixValue.expm` (issue 6.39) |
 *  | discrete time | `transform.zTransformOf` (issue 6.33) |
 *
 *  **What it refuses matters as much as what it computes.**  A non-rational `G` — a dead-time
 *  `exp(-s*T)` — stays symbolic rather than being Padé-approximated, because approximating
 *  dead time is the *user's* choice and `scalar.padeApproximant` exists for them to make it.
 *  An improper `G` declines rather than dropping the impulsive term its response carries at
 *  `t = 0`.  A stability claim resting on a coefficient whose sign is undeterminable is
 *  refused, the boundary `equation.solveInequality` draws in 3.2.
 *
 *  Layering: `control` imports `core`, `scalar`, `matrix` and `transform`; it is a leaf, like
 *  `vector` and `cli`, so nothing imports it back.
 */
package control
