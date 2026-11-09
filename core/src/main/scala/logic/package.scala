package it.grypho.scala.leonardo
/** Logic domain: boolean and three-valued (Kleene) connectives over the shared
 *  expression hierarchy.
 *
 *  The core types are:
 *  - [[_Connective]] -- marker trait of the six connective nodes [[And]], [[Or]],
 *    [[Not]], [[Implies]], [[Xor]], [[Iff]].  Operands are untyped `_Expression`s, so equations
 *    and other domains compose without cross-domain imports; `eval` reduces to
 *    `core._Bool` or `core._Truth` when the operands do.
 *  - [[asTruth]] and the [[kleeneAnd]] / [[kleeneOr]] / [[kleeneNot]] /
 *    [[kleeneImplies]] / [[kleeneXor]] / [[kleeneIff]] kernels -- the single shared rule table.
 *  - [[simplifyLogic]] / [[simplifyLogicFully]] -- structural simplification: constant
 *    folding, double negation, idempotence, complement, absorption, the combining law and
 *    De Morgan where it removes a negation.
 *  - [[_Equivalent]] -- `equivalent(f, g)`, whether two formulas agree on every assignment of
 *    their free names as booleans.
 *  - [[toCNF]] / [[toDNF]] -- conjunctive / disjunctive normal forms (crisp-only).
 *  - [[truthTable]] / [[kleeneTable]] -- exhaustive enumeration of the two- and
 *    three-valued assignments of a variable list, with the evaluation result per row.
 *
 *  The rule table is the Kleene/Zadeh min-max semantics: `AND` is min, `OR` is max,
 *  `NOT` is `1 - a`.  The *values* select the logic -- `{0, 1}` (`core._Bool`) is
 *  boolean and `{0, 1/2, 1}` (adding `core._Truth.Unknown`) is three-valued -- so the
 *  connectives and the table are shared, not duplicated, and the `[0, 1]` carrier is
 *  already in place for the fuzzy tier.
 *
 *  Layering: `logic` imports `core.*` only.  Nothing in `core`/`scalar`/`matrix`/
 *  `equation` imports `logic`; `parser` and `cli` are the consumers.
 */
package logic
