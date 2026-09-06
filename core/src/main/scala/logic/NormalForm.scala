package it.grypho.scala.leonardo
package logic

import core.*


/** Hard cap on the number of clauses a normal-form distribution may produce.
 *
 *  Distribution is exponential in the worst case (an n-term xor chain yields 2^(n-1)
 *  clauses); past this cap [[toCNF]]/[[toDNF]] return the input unchanged rather than
 *  exhausting the heap -- the same give-up-stay-symbolic convention as `Expand`'s
 *  integer-power cap.
 */
private val MaxNormalFormClauses = 1024


/** Rewrites `e` into conjunctive normal form: a conjunction of disjunctions of literals.
 *
 *  Pipeline: [[simplifyLogic]] fixpoint (folds connective-level constants away), desugar
 *  `implies`/`xor`, push `not` to the leaves via De Morgan, then distribute `or` over
 *  `and`.  Trivially-true clauses (containing a complementary literal pair) and duplicate
 *  literals/clauses are dropped; when every clause is trivially true the result is
 *  `_Bool(true)`.
 *
 *  Crisp-only (the `Asin` convention): valid for `_Bool` operands only.  Non-connective
 *  sub-expressions (equations, scalar bodies) are treated as opaque atoms.  Returns the
 *  input unchanged when distribution would exceed [[MaxNormalFormClauses]], or when the
 *  expression carries a `_Truth` degree (normal forms are a two-valued notion).
 *
 *  @param e         the expression to normalise
 *  @param symmetric whether the symmetric ternary digits `{-1, 0, 1}` are in scope
 *  @return the CNF of `e`, or `e` unchanged when it is not crisp or the distribution blows up
 */
def toCNF(e: _Expression, symmetric: Boolean = false): _Expression = toNormalForm(e, conjunctive = true, symmetric)


/** Rewrites `e` into disjunctive normal form: a disjunction of conjunctions of literals.
 *
 *  The exact dual of [[toCNF]] -- `and` distributes over `or`, trivially-false terms are
 *  dropped, and an empty term set collapses to `_Bool(false)`.  Same crisp-only domain
 *  restriction and clause cap.
 *
 *  @param e         the expression to normalise
 *  @param symmetric whether the symmetric ternary digits `{-1, 0, 1}` are in scope
 *  @return the DNF of `e`, or `e` unchanged when it is not crisp or the distribution blows up
 */
def toDNF(e: _Expression, symmetric: Boolean = false): _Expression = toNormalForm(e, conjunctive = false, symmetric)


/** Shared CNF/DNF pipeline; `conjunctive` selects which connective is the outer level.
 *
 *  Returns `e` untouched when it carries a `_Truth` degree: clause-level cleanup rests on
 *  the complement law (`a or not a` is a trivially-true clause), which fails at `unknown`.
 */
private def toNormalForm(e: _Expression, conjunctive: Boolean, symmetric: Boolean): _Expression =
  if !isCrisp(e, symmetric) then e
  else
    val simplified = simplifyLogicFully(e, identity, symmetric)
    clauseSet(nnf(simplified, negated = false), conjunctive) match
      case Some(clauses) => rebuildNormal(clauses, conjunctive)
      case None          => e


/** Negation normal form: desugars `implies`/`xor` and pushes `not` down to the leaves
 *  via De Morgan, tracking the pending negation in `negated`.
 */
private def nnf(e: _Expression, negated: Boolean): _Expression = e match
  case Not(a)        => nnf(a, !negated)
  case And(a, b)     => if negated then Or(nnf(a, true), nnf(b, true))
                        else And(nnf(a, false), nnf(b, false))
  case Or(a, b)      => if negated then And(nnf(a, true), nnf(b, true))
                        else Or(nnf(a, false), nnf(b, false))
  case Implies(a, b) => nnf(Or(Not(a), b), negated)
  case Xor(a, b)     => nnf(Or(And(a, Not(b)), And(Not(a), b)), negated)
  case _Bool(b)      => _Bool(b != negated)
  case atom          => if negated then Not(atom) else atom


/** Distributes an NNF expression into clause sets: `Some(outer clauses of inner literals)`
 *  -- conjunction of disjunctions for CNF, disjunction of conjunctions for DNF -- or
 *  `None` when the clause count exceeds [[MaxNormalFormClauses]].
 */
private def clauseSet(e: _Expression, conjunctive: Boolean): Option[Vector[Vector[_Expression]]] =
  def merge(a: _Expression, b: _Expression): Option[Vector[Vector[_Expression]]] =
    for x <- clauseSet(a, conjunctive); y <- clauseSet(b, conjunctive)
        r <- Option.when(x.size + y.size <= MaxNormalFormClauses)(x ++ y)
    yield r
  // the product size is checked BEFORE materialising it, so an over-cap distribution
  // costs nothing but the sub-results
  def cross(a: _Expression, b: _Expression): Option[Vector[Vector[_Expression]]] =
    for x <- clauseSet(a, conjunctive); y <- clauseSet(b, conjunctive)
        r <- Option.when(x.size.toLong * y.size <= MaxNormalFormClauses)(
               for cx <- x; cy <- y yield (cx ++ cy).distinct)
    yield r
  e match
    case And(a, b) => if conjunctive then merge(a, b) else cross(a, b)
    case Or(a, b)  => if conjunctive then cross(a, b) else merge(a, b)
    case literal   => Some(Vector(Vector(literal)))


/** Rebuilds the AST from a clause set, dropping trivial clauses and duplicates.
 *
 *  A clause containing a complementary pair (`x` and `not x`) is trivially true in CNF
 *  (drop it) and trivially false in DNF (drop it).  An empty clause set is the neutral
 *  element of the outer connective: `true` for CNF, `false` for DNF.
 */
private def rebuildNormal(clauses: Vector[Vector[_Expression]], conjunctive: Boolean): _Expression =
  def trivial(clause: Vector[_Expression]): Boolean =
    clause.exists(lit => clause.contains(Not(lit))) ||
    clause.contains(_Bool(conjunctive))       // `true` in a disjunction / `false` in a conjunction
  def dropNeutral(clause: Vector[_Expression]): Vector[_Expression] =
    clause.filterNot(_ == _Bool(!conjunctive))  // `false` in a disjunction / `true` in a conjunction
  val kept = clauses.map(c => dropNeutral(c.distinct)).filterNot(trivial).distinct
  val inner: (_Expression, _Expression) => _Expression = if conjunctive then Or.apply else And.apply
  val outer: (_Expression, _Expression) => _Expression = if conjunctive then And.apply else Or.apply
  if kept.isEmpty then _Bool(conjunctive)
  else if kept.exists(_.isEmpty) then _Bool(!conjunctive)  // an emptied clause is the absorbing element
  else kept.map(_.reduceLeft(inner)).reduceLeft(outer)
