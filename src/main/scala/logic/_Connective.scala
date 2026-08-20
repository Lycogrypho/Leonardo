package it.grypho.scala.leonardo
package logic

import core.*


/** Marker trait for the five boolean connectives: [[And]], [[Or]], [[Not]], [[Implies]],
 *  and [[Xor]].
 *
 *  Operands are untyped `core._Expression`s so equations and other domains compose
 *  without cross-domain imports.  `eval` reduces to `core._Bool` when the operands do;
 *  any other concrete operand (a `_Number`, a matrix) leaves the node symbolic -- widening
 *  numeric truth degrees into the connectives belongs to the ternary/fuzzy tiers.
 *
 *  Deliberately NOT marked `core._ElementWise`: that marker means derive/simplify/
 *  expand/integrate distribute over children (linear containers only), and the
 *  connectives are not linear -- `derive(a and b, x)` must stay symbolic.
 */
sealed trait _Connective extends _Expression


/** Conjunction: `a and b`.
 *
 *  Short-circuits on a false left operand exactly as `Product.eval` does on a zero left
 *  operand: `false and X` is `false` without evaluating `X`.  The rule remains valid
 *  unchanged under the ternary and fuzzy min-max semantics (0 is the annihilator of
 *  every t-norm), which is why it is written this way now.
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
      case Right(_Bool(false)) => Right(_Bool(false))
      case ra =>
        (ra, b.eval(env)) match
          case (Right(_Bool(x)), Right(_Bool(y))) => Right(_Bool(x && y))
          case (l, r)                             => Left(And(l.toExpression, r.toExpression))


/** Disjunction: `a or b`.
 *
 *  Short-circuits on a true left operand (the dual of [[And]]'s rule): `true or X` is
 *  `true` without evaluating `X`.  Remains valid under min-max ternary/fuzzy semantics
 *  (1 is the annihilator of every t-conorm).
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
      case Right(_Bool(true)) => Right(_Bool(true))
      case ra =>
        (ra, b.eval(env)) match
          case (Right(_Bool(x)), Right(_Bool(y))) => Right(_Bool(x || y))
          case (l, r)                             => Left(Or(l.toExpression, r.toExpression))


/** Negation: `not a`.
 *  @param a the operand
 */
case class Not(a: _Expression) extends _Connective:
  override def toString: String = s"(not $a)"
  override def children: List[_Expression] = List(a)
  override def rebuild(c: List[_Expression]): _Expression = Not(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    a.eval(env) match
      case Right(_Bool(x)) => Right(_Bool(!x))
      case ra              => Left(Not(ra.toExpression))


/** Material implication: `a implies b`.
 *
 *  Short-circuits on a false left operand: `false implies X` is `true` without
 *  evaluating `X` (sound under min-max, product, and Lukasiewicz semantics alike,
 *  since the co-norm of 1 with anything is 1).
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
      case Right(_Bool(false)) => Right(_Bool(true))
      case ra =>
        (ra, b.eval(env)) match
          case (Right(_Bool(x)), Right(_Bool(y))) => Right(_Bool(!x || y))
          case (l, r)                             => Left(Implies(l.toExpression, r.toExpression))


/** Exclusive disjunction: `a xor b`.  No short-circuit is possible: both operands
 *  always matter.
 *
 *  @param a left operand
 *  @param b right operand
 */
case class Xor(a: _Expression, b: _Expression) extends _Connective:
  override def toString: String = s"($a xor $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Xor(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(_Bool(x)), Right(_Bool(y))) => Right(_Bool(x != y))
      case (l, r)                             => Left(Xor(l.toExpression, r.toExpression))
