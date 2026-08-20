package it.grypho.scala.leonardo
package logic

import core.*


/** Marker trait for the five logical connectives: [[And]], [[Or]], [[Not]], [[Implies]],
 *  and [[Xor]].
 *
 *  Operands are untyped `core._Expression`s so equations and other domains compose
 *  without cross-domain imports.  `eval` runs the shared Kleene/Zadeh min–max rule table
 *  ([[kleeneAnd]], [[kleeneOr]], [[kleeneNot]], [[kleeneImplies]], [[kleeneXor]]) over
 *  operands widened by [[asTruth]], so `core._Bool` and `core._Truth` operands mix freely
 *  and the boolean behaviour is a special case of the graded table rather than a separate
 *  branch.  The result is rebuilt through `_Truth.of`, which collapses a crisp degree back
 *  to `_Bool`.  Any other concrete operand (a `_Number`, a matrix) leaves the node
 *  symbolic.
 *
 *  Deliberately NOT marked `core._ElementWise`: that marker means derive/simplify/
 *  expand/integrate distribute over children (linear containers only), and the
 *  connectives are not linear -- `derive(a and b, x)` must stay symbolic.
 */
sealed trait _Connective extends _Expression


/** Applies a binary rule to two evaluated operands.
 *
 *  Yields `Right` only when both operands reduced to truth-valued results; otherwise the
 *  node is rebuilt symbolically from the most-reduced operands via `wrap`.
 *
 *  @param ra   the evaluated left operand
 *  @param rb   the evaluated right operand
 *  @param rule the min–max kernel to apply to the two degrees
 *  @param wrap factory rebuilding the symbolic residual node
 *  @return `Right(value)` when both operands are truth-valued, `Left(residual)` otherwise
 */
private def combine(
    ra:   Either[_Expression, _Value],
    rb:   Either[_Expression, _Value],
    rule: (Double, Double) => Double,
    wrap: (_Expression, _Expression) => _Expression
): Either[_Expression, _Value] =
  (ra, rb) match
    case (Right(x: _Value), Right(y: _Value)) =>
      (asTruth(x), asTruth(y)) match
        case (Some(p), Some(q)) => Right(_Truth.of(rule(p, q)))
        case _                  => Left(wrap(x, y))
    case (l, r) => Left(wrap(l.toExpression, r.toExpression))


/** Conjunction: `a and b`, evaluated as `min` over the operand degrees.
 *
 *  Short-circuits on a false left operand exactly as `Product.eval` does on a zero left
 *  operand: `false and X` is `false` without evaluating `X`.  The rule is sound in the
 *  graded table too — `0` annihilates `min` — so no many-valued special case is needed.
 *
 *  @param a left operand
 *  @param b right operand
 */
case class And(a: _Expression, b: _Expression) extends _Connective:
  override def toString: String = s"($a and $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = And(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    a.eval(env) match
      // short-circuit: false and X = false, X never evaluated (the Product zero rule)
      case Right(v) if asTruth(v).contains(0.0) => Right(_Bool(false))
      case ra                                   => combine(ra, b.eval(env), kleeneAnd, And.apply)


/** Disjunction: `a or b`, evaluated as `max` over the operand degrees.
 *
 *  Short-circuits on a true left operand (the dual of [[And]]'s rule): `true or X` is
 *  `true` without evaluating `X`.  Sound in the graded table — `1` annihilates `max`.
 *
 *  @param a left operand
 *  @param b right operand
 */
case class Or(a: _Expression, b: _Expression) extends _Connective:
  override def toString: String = s"($a or $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Or(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    a.eval(env) match
      // short-circuit: true or X = true, X never evaluated
      case Right(v) if asTruth(v).contains(1.0) => Right(_Bool(true))
      case ra                                   => combine(ra, b.eval(env), kleeneOr, Or.apply)


/** Negation: `not a`, evaluated as `1 - a`.  `unknown` is the fixpoint.
 *  @param a the operand
 */
case class Not(a: _Expression) extends _Connective:
  override def toString: String = s"(not $a)"
  override def children: List[_Expression] = List(a)
  override def rebuild(c: List[_Expression]): _Expression = Not(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    a.eval(env) match
      case Right(v) => asTruth(v) match
        case Some(p) => Right(_Truth.of(kleeneNot(p)))
        case None    => Left(Not(v))
      case ra       => Left(Not(ra.toExpression))


/** Material implication: `a implies b`, evaluated as `max(1 - a, b)`.
 *
 *  Short-circuits on a false left operand: `false implies X` is `true` without
 *  evaluating `X` (sound in the graded table, since `1` annihilates `max`).
 *
 *  @param a antecedent
 *  @param b consequent
 */
case class Implies(a: _Expression, b: _Expression) extends _Connective:
  override def toString: String = s"($a implies $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Implies(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    a.eval(env) match
      // short-circuit: false implies X = true, X never evaluated
      case Right(v) if asTruth(v).contains(0.0) => Right(_Bool(true))
      case ra                                   => combine(ra, b.eval(env), kleeneImplies, Implies.apply)


/** Exclusive disjunction: `a xor b`, evaluated as the lattice reading of
 *  `(a and not b) or (not a and b)` (see [[kleeneXor]]).  No short-circuit is possible:
 *  both operands always matter.
 *
 *  @param a left operand
 *  @param b right operand
 */
case class Xor(a: _Expression, b: _Expression) extends _Connective:
  override def toString: String = s"($a xor $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Xor(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    combine(a.eval(env), b.eval(env), kleeneXor, Xor.apply)
