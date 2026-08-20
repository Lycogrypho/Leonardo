package it.grypho.scala.leonardo
/** Boolean logic domain: connectives over the shared expression hierarchy.
 *
 *  The core types are:
 *  - [[_Connective]] -- marker trait of the five connective nodes [[And]], [[Or]],
 *    [[Not]], [[Implies]], [[Xor]].  Operands are untyped `_Expression`s, so equations
 *    and other domains compose without cross-domain imports; `eval` reduces to
 *    `core._Bool` when the operands do.
 *  - [[simplifyLogic]] / [[simplifyLogicFully]] -- structural simplification: constant
 *    folding, double negation, idempotence, complement, and absorption.
 *  - [[toCNF]] / [[toDNF]] -- conjunctive / disjunctive normal forms (crisp-only).
 *  - [[truthTable]] -- exhaustive enumeration of the boolean assignments of a
 *    variable list, with the evaluation result per row.
 *
 *  The rule table is the Kleene/Zadeh min-max semantics specialised to the crisp
 *  values {0, 1}: `AND` is min, `OR` is max, `NOT` is `1 - a`.  Written so the same
 *  connectives stay valid unchanged when the value carrier later widens to ternary
 *  ({0, 1/2, 1}) and fuzzy ([0, 1]) truth values.
 *
 *  Layering: `logic` imports `core.*` only.  Nothing in `core`/`scalar`/`matrix`/
 *  `equation` imports `logic`; `parser` and `cli` are the consumers.
 */
package logic
