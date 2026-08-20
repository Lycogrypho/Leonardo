package it.grypho.scala.leonardo
package logic

import core.*


/** Widens any truth-valued result to its degree in `[0, 1]`.
 *
 *  `_Bool` auto-widens in connective positions (the `Int` → `Double` analogy), so the
 *  connectives need no per-node type tests and the boolean rule table is literally the
 *  same table as the graded one.  Every other `_Value` (a `_Number`, a `_MatrixValue`,
 *  a `_Complex`) yields `None`, which keeps the connective symbolic.
 *
 *  @param v the concrete value to widen
 *  @return the truth degree, or `None` when `v` is not truth-valued
 */
private[leonardo] def asTruth(v: _Value): Option[Double] = v match
  case _Bool(b)  => Some(if b then 1.0 else 0.0)
  case _Truth(d) => Some(d)
  case _         => None


/** Returns `true` when no [[core._Truth]] degree occurs anywhere in `e`.
 *
 *  The classical rewrite rules that fail in many-valued logic (complement,
 *  `a implies a`) are gated on this test.  Free variables count as crisp: the classical
 *  rules assume boolean-valued atoms, which is the documented domain restriction of
 *  [[simplifyLogic]], [[toCNF]], and [[toDNF]].
 *
 *  @param e the expression to test
 */
private[logic] def isCrisp(e: _Expression): Boolean = e match
  case _: _Truth => false
  case other     => other.children.forall(isCrisp)


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
