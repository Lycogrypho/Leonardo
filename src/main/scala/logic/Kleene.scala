package it.grypho.scala.leonardo
package logic

import core.*


/** The symmetric ternary digits, read as truth values only under the symmetric encoding. */
private val SymmetricDigits: Set[Double] = Set(-1.0, 0.0, 1.0)


/** Widens any truth-valued result to its degree in `[0, 1]`.
 *
 *  `_Bool` auto-widens in connective positions (the `Int` → `Double` analogy), so the
 *  connectives need no per-node type tests and the boolean rule table is literally the
 *  same table as the graded one.  Every other `_Value` (a `_Number`, a `_MatrixValue`,
 *  a `_Complex`) yields `None`, which keeps the connective symbolic.
 *
 *  Under the symmetric ternary encoding (`symmetric = true`, driven by
 *  `Environment.symmetricLogic`) the digits `-1`, `0`, `1` additionally read as
 *  `false`, `unknown`, `true` via `_Truth.fromSymmetric`'s affine map.  The word
 *  spellings keep working, so the encoding only ever *adds* a way to write a truth
 *  value.  The guard matters: outside symmetric mode a bare `0` must stay a number,
 *  or `0 and x` would silently become `unknown and x`.
 *
 *  @param v         the concrete value to widen
 *  @param symmetric whether the symmetric ternary digits are in scope
 *  @return the truth degree, or `None` when `v` is not truth-valued
 */
private[leonardo] def asTruth(v: _Value, symmetric: Boolean = false): Option[Double] = v match
  case _Bool(b)                                    => Some(if b then 1.0 else 0.0)
  case _Truth(d)                                   => Some(d)
  case _Number(d) if symmetric && SymmetricDigits.contains(d) => Some((d + 1.0) / 2.0)
  case _                                           => None


/** Returns `true` when no graded (neither-true-nor-false) truth value occurs in `e`.
 *
 *  The classical rewrite rules that fail in many-valued logic (complement,
 *  `a implies a`, `a xor a`) are gated on this test.  Free variables count as crisp:
 *  the classical rules assume boolean-valued atoms, which is the documented domain
 *  restriction of [[simplifyLogic]], [[toCNF]], and [[toDNF]].
 *
 *  Under the symmetric encoding the digit `0` spells `unknown` and is therefore graded
 *  too — without that case `0 and not 0` would wrongly fold to `false` by complement.
 *  The digits `-1` and `1` stay crisp.
 *
 *  @param e         the expression to test
 *  @param symmetric whether the symmetric ternary digits are in scope
 */
private[logic] def isCrisp(e: _Expression, symmetric: Boolean = false): Boolean = e match
  case _: _Truth                          => false
  case _Number(0.0) if symmetric          => false
  case other                              => other.children.forall(isCrisp(_, symmetric))


/** Conjunction kernel: the t-norm `min(a, b)`.
 *  @param a left truth degree
 *  @param b right truth degree
 *  @return the conjunction degree
 */
private[logic] def kleeneAnd(a: Double, b: Double): Double = math.min(a, b)

/** Disjunction kernel: the t-conorm `max(a, b)`.
 *  @param a left truth degree
 *  @param b right truth degree
 *  @return the disjunction degree
 */
private[logic] def kleeneOr(a: Double, b: Double): Double = math.max(a, b)

/** Negation kernel: `1 - a`.  `0.5` is its fixpoint, so `not unknown = unknown`.
 *  @param a the truth degree to negate
 *  @return the negated degree
 */
private[logic] def kleeneNot(a: Double): Double = 1.0 - a

/** Implication kernel: `max(1 - a, b)`, the Kleene reading of `(not a) or b`.
 *  @param a antecedent degree
 *  @param b consequent degree
 *  @return the implication degree
 */
private[logic] def kleeneImplies(a: Double, b: Double): Double = math.max(1.0 - a, b)

/** Exclusive-disjunction kernel: `max(min(a, 1 - b), min(1 - a, b))`.
 *
 *  This is the lattice reading of `(a and not b) or (not a and b)` — the same
 *  desugaring [[toCNF]] / [[toDNF]] apply — so the eval kernel and the normal-form
 *  rewrite agree on every input.  On `{0, 1}` it is exactly classical xor; at
 *  `unknown xor unknown` it yields `unknown`, since two unknown operands cannot be
 *  known to differ.  (The naive `|a - b|` would answer `false` there, contradicting
 *  both the Kleene reading and the desugaring.)
 *
 *  @param a left truth degree
 *  @param b right truth degree
 *  @return the exclusive-disjunction degree
 */
private[logic] def kleeneXor(a: Double, b: Double): Double =
  math.max(math.min(a, 1.0 - b), math.min(1.0 - a, b))
