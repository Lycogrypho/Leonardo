package it.grypho.scala.leonardo

/** Scalar algebra domain: AST nodes for operations and functions, and the algorithm
 *  functions that operate on them (differentiation, integration, simplification, expansion,
 *  normalisation, substitution, limit evaluation, numerical sampling).
 *
 *  Imports `core` for the shared expression hierarchy; no other Leonardo package is
 *  imported here.  Downstream packages (`matrix`, `equation`, `transform`, `ode`,
 *  `parser`, `cli`) import `scalar`.
 */
package scalar
