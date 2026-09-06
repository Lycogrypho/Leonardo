package it.grypho.scala.leonardo

/** Domain analysis as a user-facing question — issue 3.3 slice E.
 *
 *  The analysis itself lives in `scalar` (`scalar.domainOf`, `scalar.singularitiesOf`),
 *  because `scalar.integrate` and `scalar.Limit` have to be able to call it as a guard.  It
 *  therefore answers in a **neutral** `DomainSet`, which is not a predicate in the language.
 *  This package is the other half of that split: it renders a `DomainSet` into the relation
 *  nodes a user reads — `_Comparison`, `and`, `or`, `_Bool`.
 *
 *  A separate package rather than nodes in `equation`, for two reasons: a domain is not a
 *  relation, and the project's convention is one package per domain.  It imports
 *  `core + scalar + equation + logic`, which is acyclic — nothing imports it but `parser`.
 *
 *  **What cannot be rendered stays symbolic.**  `tan`'s excluded points and the `Gamma`
 *  poles are infinite sets, and the language has no quantifier, so `domain(tan(x), x)` does
 *  not reduce.  Emitting a truncated list of exclusions would read as exhaustive, which is
 *  the failure mode this whole issue exists to avoid.
 */
package object domain
