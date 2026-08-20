package it.grypho.scala.leonardo
package logic

import core.*


/** Single-pass structural simplification of the boolean connectives.
 *
 *  Rules: constant folding (`a and true = a`, `a or true = true`, ...), double negation
 *  (`not not a = a`), idempotence (`a and a = a`), complement (`a and not a = false`),
 *  and absorption (`a or (a and b) = a`).  De Morgan and the `implies`/`xor` desugarings
 *  are deliberately NOT applied here -- they reshape rather than shrink, and belong to
 *  the normal forms ([[toCNF]]/[[toDNF]]).
 *
 *  Crisp-only (the `Asin` convention): the complement and absorption rules are valid
 *  over `_Bool` operands only; they must not fire once intermediate truth degrees widen
 *  the carrier.
 *
 *  Non-connective sub-expressions are handed to `simplifyLeaf`.  The default is
 *  `identity`; the REPL passes `scalar.simplifyFully` so a scalar body nested inside a
 *  connective is simplified too -- `logic` itself imports `core` only, so the scalar
 *  pass is injected rather than imported.  `simplifyLeaf` must be idempotent for
 *  [[simplifyLogicFully]]'s fixpoint to terminate.
 *
 *  @param e            the expression to simplify
 *  @param simplifyLeaf pass applied to non-connective sub-expressions (default: `identity`)
 *  @return the simplified expression; never larger than the input
 */
def simplifyLogic(e: _Expression, simplifyLeaf: _Expression => _Expression = identity): _Expression = e match
  case And(a, b) => (simplifyLogic(a, simplifyLeaf), simplifyLogic(b, simplifyLeaf)) match
    case (_Bool(false), _) | (_, _Bool(false)) => _Bool(false)
    case (_Bool(true), y)                      => y
    case (x, _Bool(true))                      => x
    case (x, y) if x == y                      => x               // idempotence
    case (x, Not(y)) if x == y                 => _Bool(false)    // complement
    case (Not(x), y) if x == y                 => _Bool(false)
    case (x, Or(p, q)) if x == p || x == q     => x               // absorption
    case (Or(p, q), y) if y == p || y == q     => y
    case (x, y)                                => And(x, y)

  case Or(a, b) => (simplifyLogic(a, simplifyLeaf), simplifyLogic(b, simplifyLeaf)) match
    case (_Bool(true), _) | (_, _Bool(true))   => _Bool(true)
    case (_Bool(false), y)                     => y
    case (x, _Bool(false))                     => x
    case (x, y) if x == y                      => x               // idempotence
    case (x, Not(y)) if x == y                 => _Bool(true)     // complement
    case (Not(x), y) if x == y                 => _Bool(true)
    case (x, And(p, q)) if x == p || x == q    => x               // absorption
    case (And(p, q), y) if y == p || y == q    => y
    case (x, y)                                => Or(x, y)

  case Not(a) => simplifyLogic(a, simplifyLeaf) match
    case _Bool(x) => _Bool(!x)
    case Not(x)   => x                                            // double negation
    case x        => Not(x)

  case Implies(a, b) => (simplifyLogic(a, simplifyLeaf), simplifyLogic(b, simplifyLeaf)) match
    case (_Bool(false), _)  => _Bool(true)
    case (_Bool(true), y)   => y
    case (_, _Bool(true))   => _Bool(true)
    case (x, _Bool(false))  => simplifyLogic(Not(x), simplifyLeaf)
    case (x, y) if x == y   => _Bool(true)
    case (x, y)             => Implies(x, y)

  case Xor(a, b) => (simplifyLogic(a, simplifyLeaf), simplifyLogic(b, simplifyLeaf)) match
    case (_Bool(x), _Bool(y)) => _Bool(x != y)
    case (x, _Bool(false))    => x
    case (_Bool(false), y)    => y
    case (x, _Bool(true))     => simplifyLogic(Not(x), simplifyLeaf)
    case (_Bool(true), y)     => simplifyLogic(Not(y), simplifyLeaf)
    case (x, y) if x == y     => _Bool(false)
    case (x, y)               => Xor(x, y)

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
 *  @return the fully simplified expression
 */
@annotation.tailrec
def simplifyLogicFully(e: _Expression, simplifyLeaf: _Expression => _Expression = identity): _Expression =
  val s = simplifyLogic(e, simplifyLeaf)
  if s == e then e else simplifyLogicFully(s, simplifyLeaf)
