package it.grypho.scala.leonardo

/** Descriptive statistics, linear regression and elementary inference over samples
 *  (issue 4.Q).
 *
 *  Imports `core` + `scalar` + `matrix`, and — for the inference tier — `probability`.  The
 *  arrow runs that way and only that way: **`probability` must never import `statistics`**.
 *
 *  **A sample is a matrix.**  `_MatrixValue` row and column vectors are the natural carrier,
 *  so the descriptive kernels sit beside `add`/`multiply` rather than introducing a value
 *  type of their own.  All entries are read in row-major order, so the shape does not matter
 *  — `[[1, 2, 3]]` and `[[1], [2], [3]]` are the same sample.
 *
 *  **`n` versus `n − 1`, which is the one thing to get straight before reading further.**
 *  `variance(sample)` and `covariance` are the **unbiased** estimators, dividing by `n − 1`.
 *  That is what "variance" means of a *dataset* — it is R's `var`, Python's
 *  `statistics.variance` — and it is the form the t-test needs.  `pvariance` and `pstddev`
 *  are the population forms, dividing by `n`.
 *
 *  `variance(distribution)` is necessarily the population variance: a distribution is not a
 *  sample, so there is no `n` to correct for.  The two are different objects, not an
 *  inconsistency.  `correlation` is unaffected either way — the `n − 1` cancels between the
 *  covariance and the two standard deviations.
 *
 *  **Exactness.**  Mean, variance and covariance are pure field operations — sums, products
 *  and one division — so they are closed over the rationals and stay exact when the sample
 *  is exact.  Standard deviation and correlation take a square root and therefore are not;
 *  they go through the exact tier's `exactSqrt` at the working precision, which is the same
 *  contract every other irrational result in the library has.
 */
package statistics
