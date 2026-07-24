package it.grypho.scala.leonardo
/** Matrix domain: the `_Matrix` symbolic node and the `_MatrixOperation` AST hierarchy.
 *
 *  The two concrete types that callers normally work with are:
 *  - `_Matrix(rows, cols, elems)` -- a symbolic matrix whose elements are arbitrary
 *    `_Expression` values.  Collapses to a `_MatrixValue` (from `core`) when all
 *    elements fold to concrete numbers.
 *  - The [[_MatrixOperation]] subtypes ([[MatSum]], [[MatProduct]], [[MatScale]],
 *    [[Transpose]], [[Determinant]], [[Inverse]], [[IdentityMatrix]], [[ZeroMatrix]],
 *    and the decomposition nodes) -- each reduces to a dense `_MatrixValue` or a
 *    symbolic `_Matrix` of matrices when the operand(s) are concrete.
 *
 *  Layering: `matrix` imports `core.*` and `scalar.{Sum, Product, Ratio}` for
 *  element-level arithmetic.  Nothing in `core` or `scalar` imports `matrix`;
 *  `equation` and `parser` are the next consumers up the stack.
 */
package matrix
