package it.grypho.scala.leonardo
package logic

import core.*


/** Folds two concrete truth-valued operands through a min–max kernel.
 *  `None` when either operand is not a truth-valued `_Value` under the active encoding.
 */
private def foldConstants(
    x: _Expression, y: _Expression, rule: (Double, Double) => Double, symmetric: Boolean
): Option[_Expression] = (x, y) match
  case (xv: _Value, yv: _Value) =>
    for p <- asTruth(xv, symmetric); q <- asTruth(yv, symmetric) yield _Truth.of(rule(p, q))
  case _ => None


/** Single-pass structural simplification of the logical connectives.
 *
 *  Rules: constant folding through the shared min–max kernels, double negation
 *  (`not not a = a`), idempotence (`a and a = a`), complement (`a and not a = false`),
 *  and absorption (`a or (a and b) = a`).  De Morgan and the `implies`/`xor` desugarings
 *  are deliberately NOT applied here -- they reshape rather than shrink, and belong to
 *  the normal forms ([[toCNF]]/[[toDNF]]).
 *
 *  Many-valued soundness: every rule above except four is valid in the full `[0, 1]`
 *  min–max lattice, so it applies unchanged to `_Truth` operands.  The four that are
 *  not — complement in `and` and in `or`, `a implies a = true`, and `a xor a = false` —
 *  all fail at `unknown` (`unknown and not unknown = unknown`, not `false`) and are gated
 *  on [[isCrisp]].  Free variables count as crisp: the classical rules assume
 *  boolean-valued atoms, the documented domain restriction of this pass (the `Asin`
 *  convention).  Bind a variable to `unknown` and use `eval`, not `simplify`, to get the
 *  three-valued answer.
 *
 *  Non-connective sub-expressions are handed to `simplifyLeaf`.  The default is
 *  `identity`; the REPL passes `scalar.simplifyFully` so a scalar body nested inside a
 *  connective is simplified too -- `logic` itself imports `core` only, so the scalar
 *  pass is injected rather than imported.  `simplifyLeaf` must be idempotent for
 *  [[simplifyLogicFully]]'s fixpoint to terminate.
 *
 *  @param e            the expression to simplify
 *  @param simplifyLeaf pass applied to non-connective sub-expressions (default: `identity`)
 *  @param symmetric    whether the symmetric ternary digits `{-1, 0, 1}` are in scope
 *                      (the REPL passes its session toggle); this pass takes no
 *                      `Environment`, by the same design that makes `scalar.simplify`
 *                      ignore bindings
 *  @return the simplified expression; never larger than the input
 */
def simplifyLogic(
    e: _Expression,
    simplifyLeaf: _Expression => _Expression = identity,
    symmetric: Boolean = false
): _Expression =
  def rec(x: _Expression): _Expression = simplifyLogic(x, simplifyLeaf, symmetric)
  e match
    case And(a, b) =>
      val (x, y) = (rec(a), rec(b))
      foldConstants(x, y, kleeneAnd, symmetric).getOrElse((x, y) match
        case (_Bool(false), _) | (_, _Bool(false))          => _Bool(false)
        case (_Bool(true), r)                               => r
        case (l, _Bool(true))                               => l
        case (l, r) if l == r                               => l               // idempotence
        case (l, Not(r)) if l == r && isCrisp(l, symmetric) => _Bool(false)    // complement (crisp only)
        case (Not(l), r) if l == r && isCrisp(l, symmetric) => _Bool(false)
        case (l, Or(p, q)) if l == p || l == q              => l               // absorption
        case (Or(p, q), r) if r == p || r == q              => r
        case (l, r)                                         => And(l, r))

    case Or(a, b) =>
      val (x, y) = (rec(a), rec(b))
      foldConstants(x, y, kleeneOr, symmetric).getOrElse((x, y) match
        case (_Bool(true), _) | (_, _Bool(true))            => _Bool(true)
        case (_Bool(false), r)                              => r
        case (l, _Bool(false))                              => l
        case (l, r) if l == r                               => l               // idempotence
        case (l, Not(r)) if l == r && isCrisp(l, symmetric) => _Bool(true)     // complement (crisp only)
        case (Not(l), r) if l == r && isCrisp(l, symmetric) => _Bool(true)
        case (l, And(p, q)) if l == p || l == q             => l               // absorption
        case (And(p, q), r) if r == p || r == q             => r
        case (l, r)                                         => Or(l, r))

    case Not(a) =>
      val x = rec(a)
      val folded = x match
        case v: _Value => asTruth(v, symmetric).map(p => _Truth.of(kleeneNot(p)))
        case _         => None
      folded.getOrElse(x match
        case Not(y) => y                                                       // double negation
        case _      => Not(x))

    case Implies(a, b) =>
      val (x, y) = (rec(a), rec(b))
      foldConstants(x, y, kleeneImplies, symmetric).getOrElse((x, y) match
        case (_Bool(false), _)                              => _Bool(true)
        case (_Bool(true), r)                               => r
        case (_, _Bool(true))                               => _Bool(true)
        case (l, _Bool(false))                              => rec(Not(l))
        case (l, r) if l == r && isCrisp(l, symmetric)      => _Bool(true)     // crisp only
        case (l, r)                                         => Implies(l, r))

    case Xor(a, b) =>
      val (x, y) = (rec(a), rec(b))
      foldConstants(x, y, kleeneXor, symmetric).getOrElse((x, y) match
        case (l, _Bool(false))                              => l
        case (_Bool(false), r)                              => r
        case (l, _Bool(true))                               => rec(Not(l))
        case (_Bool(true), r)                               => rec(Not(r))
        case (l, r) if l == r && isCrisp(l, symmetric)      => _Bool(false)    // crisp only
        case (l, r)                                         => Xor(l, r))

    // Non-connective: scalar simplification does not recurse into connectives (its
    // fallback is `case other => other`), so the injected leaf pass is the only chance
    // a scalar body nested inside a connective gets.
    case other => simplifyLeaf(other)


/** Iterates [[simplifyLogic]] until the result stops changing (fixpoint).
 *
 *  Terminates because [[simplifyLogic]] never increases expression size (given an
 *  idempotent `simplifyLeaf`).
 *
 *  @param e            the expression to simplify
 *  @param simplifyLeaf pass applied to non-connective sub-expressions (default: `identity`)
 *  @param symmetric    whether the symmetric ternary digits `{-1, 0, 1}` are in scope
 *  @return the fully simplified expression
 */
@annotation.tailrec
def simplifyLogicFully(
    e: _Expression,
    simplifyLeaf: _Expression => _Expression = identity,
    symmetric: Boolean = false
): _Expression =
  val s = simplifyLogic(e, simplifyLeaf, symmetric)
  if s == e then e else simplifyLogicFully(s, simplifyLeaf, symmetric)
