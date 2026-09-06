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
 *  A [[_Membership]] node counts as graded whatever its arguments: a hedge or a curve
 *  produces an arbitrary degree, and `truth(0.3)` is a graded value that simply has not
 *  been evaluated yet.  Erring towards "graded" only ever *blocks* a rewrite, which is
 *  the safe direction.
 *
 *  @param e         the expression to test
 *  @param symmetric whether the symmetric ternary digits are in scope
 */
private[logic] def isCrisp(e: _Expression, symmetric: Boolean = false): Boolean = e match
  case _: _Truth                          => false
  case _: _Membership                     => false
  case _Number(0.0) if symmetric          => false
  case other                              => other.children.forall(isCrisp(_, symmetric))


/** Reads a membership *degree* from a concrete value.
 *
 *  Wider than [[asTruth]] on purpose: a bare number in `[0, 1]` is accepted regardless of
 *  the encoding, because a hedge or a membership curve has no other domain — `very(0.5)`
 *  is unambiguous in a way `0.5 and x` is not.  The connectives keep using the strict
 *  [[asTruth]].
 *
 *  @param v         the concrete value to read
 *  @param symmetric whether the symmetric ternary digits are in scope
 *  @return the degree in `[0, 1]`, or `None` when `v` is not a degree
 */
private[logic] def asDegree(v: _Value, symmetric: Boolean = false): Option[Double] =
  asTruth(v, symmetric).orElse(v match
    case _Number(d) if d >= 0.0 && d <= 1.0 => Some(d)
    case _                                  => None)


/** Conjunction kernel (t-norm) for `semantics`.
 *
 *  `min` under `LogicSemantics.MinMax`, `a * b` under `Product`,
 *  `max(0, a + b - 1)` under `Lukasiewicz`.  All three agree on `{0, 1}` and all three
 *  have `0` as annihilator and `1` as unit, which is what keeps the `And` short-circuit
 *  sound under each.
 *
 *  @param semantics the active t-norm family
 *  @param a         left truth degree
 *  @param b         right truth degree
 *  @return the conjunction degree
 */
private[logic] def kleeneAnd(semantics: LogicSemantics)(a: Double, b: Double): Double =
  semantics match
    case LogicSemantics.MinMax      => math.min(a, b)
    case LogicSemantics.Product     => a * b
    case LogicSemantics.Lukasiewicz => math.max(0.0, a + b - 1.0)

/** Disjunction kernel (t-conorm) for `semantics`: `max`, `a + b - a * b`, or
 *  `min(1, a + b)`.  Dual of [[kleeneAnd]]; `1` annihilates each, keeping the `Or`
 *  short-circuit sound.
 *
 *  @param semantics the active t-conorm family
 *  @param a         left truth degree
 *  @param b         right truth degree
 *  @return the disjunction degree
 */
private[logic] def kleeneOr(semantics: LogicSemantics)(a: Double, b: Double): Double =
  semantics match
    case LogicSemantics.MinMax      => math.max(a, b)
    case LogicSemantics.Product     => a + b - a * b
    case LogicSemantics.Lukasiewicz => math.min(1.0, a + b)

/** Negation kernel: the strong negation `1 - a`, shared by all three semantics.
 *  `0.5` is its fixpoint, so `not unknown = unknown`.
 *
 *  @param a the truth degree to negate
 *  @return the negated degree
 */
private[logic] def kleeneNot(a: Double): Double = 1.0 - a

/** Implication kernel: the S-implication `tconorm(1 - a, b)` of the active semantics,
 *  i.e. the reading of `(not a) or b` in that algebra.
 *
 *  @param semantics the active semantics
 *  @param a         antecedent degree
 *  @param b         consequent degree
 *  @return the implication degree
 */
private[logic] def kleeneImplies(semantics: LogicSemantics)(a: Double, b: Double): Double =
  kleeneOr(semantics)(kleeneNot(a), b)

/** Exclusive-disjunction kernel: the reading of `(a and not b) or (not a and b)` in the
 *  active semantics.
 *
 *  Defining it through the same desugaring [[toCNF]] / [[toDNF]] apply keeps the eval
 *  kernel and the normal-form rewrite in agreement for every semantics.  On `{0, 1}` it
 *  is exactly classical xor; under min–max `unknown xor unknown` is `unknown`, since two
 *  unknown operands cannot be known to differ.  (The naive `|a - b|` would answer `false`
 *  there, contradicting both the Kleene reading and the desugaring.)
 *
 *  @param semantics the active semantics
 *  @param a         left truth degree
 *  @param b         right truth degree
 *  @return the exclusive-disjunction degree
 */
private[logic] def kleeneXor(semantics: LogicSemantics)(a: Double, b: Double): Double =
  kleeneOr(semantics)(
    kleeneAnd(semantics)(a, kleeneNot(b)),
    kleeneAnd(semantics)(kleeneNot(a), b))