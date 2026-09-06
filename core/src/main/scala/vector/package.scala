package it.grypho.scala.leonardo

/** Vector calculus — scalar and vector fields over an ordered coordinate tuple (issue 6.24).
 *
 *  The differential operators `grad`, `div`, `curl` and `laplacian`, plus the multivariate
 *  derivative surface `jacobian` and `hessian`.  Imports `core + scalar + matrix`, which is
 *  acyclic: nothing imports this package but `parser`.  No `equation` import is needed —
 *  every operator returns a matrix or a scalar, never a relation.
 *
 *  **A vector field is an n×1 `matrix._Matrix`, not a new carrier.**  The symbolic matrix
 *  already holds arbitrary `_Expression` cells, so every existing matrix operation applies to
 *  a field unchanged, and `derive` already distributes over it element-wise (`_Matrix` is
 *  `_ElementWise`).  This is the argument that kept distributions out of a bespoke container
 *  in 4.P: a new carrier is only worth it when something must read the value *as* one.
 *
 *  **The operators are compositions of `derive`; the content here is the shape rules.**  Each
 *  one is a line of arithmetic over partial derivatives, so what the code actually spends its
 *  effort on is *refusing* the ill-shaped cases — a `curl` outside three dimensions, a
 *  component/coordinate count mismatch, a repeated coordinate.  That is issue 3.3's rule that
 *  an analysis must describe what the library computes, applied to a domain where the wrong
 *  answer would look perfectly plausible.
 *
 *  **The coordinate tuple is explicit and ordered, never inferred.**  `grad(f)` alone is
 *  meaningless: the order of the result's components has to come from somewhere, and reading
 *  it off `freeVars` would have to invent a convention (alphabetical? first-seen?) under
 *  which `grad(x*y)` and `grad(y*x)` could disagree.  Every node therefore carries its
 *  `Vector[_Variable]`.
 *
 *  Those variables follow the **`scalar._Taylor` convention, not the binder convention**:
 *  they appear *free in the result* — a gradient is a function of position — so they are
 *  excluded from `children` (so `substitute` cannot rewrite the coordinates the answer is
 *  phrased in) but are not binders in the `_Derivative` sense.  `_SolveSystem` supplies only
 *  the list-shaped plumbing; `_Taylor` and `domain._Domain` supply the semantics.
 *
 *  **Cartesian only for now** ([[CoordinateSystem]]).  The field ships from the start so the
 *  node shape is final and issue 6.26 — cylindrical and spherical, via the general orthogonal
 *  curvilinear formulas — lands as an `eval` change and nothing else.  Any other system stays
 *  symbolic rather than being answered with the Cartesian formula, which would be a
 *  confidently wrong result rather than an obviously missing one.
 */
package object vector
