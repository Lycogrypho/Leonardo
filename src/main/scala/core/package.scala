package it.grypho.scala.leonardo

/** Foundation types shared across all Leonardo domains: the expression hierarchy,
 *  concrete values, the evaluation environment, and cross-domain markers.
 *
 *  Nothing in `core` imports any other Leonardo package; all other packages
 *  (`scalar`, `matrix`, `equation`, `transform`, `ode`, `parser`, `cli`) import
 *  `core` — never the reverse.
 */
package core
