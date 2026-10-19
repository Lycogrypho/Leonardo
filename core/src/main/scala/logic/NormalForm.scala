package it.grypho.scala.leonardo
package logic

import core.*


/** Hard cap on the number of clauses a normal-form distribution may produce.
 *
 *  Distribution is exponential in the worst case (an n-term xor chain yields 2^(n-1)
 *  clauses); past this cap [[toCNF]]/[[toDNF]] return the input unchanged rather than
 *  exhausting the heap -- the same give-up-stay-symbolic convention as `Expand`'s
 *  integer-power cap.  The **default** of [[core.Limit.MaxNormalFormClauses]] (F_0124); the
 *  `Limits` overloads of [[toCNF]]/[[toDNF]] take another.
 */
private val MaxNormalFormClauses = Limit.MaxNormalFormClauses.default.toInt


/** Rewrites `e` into conjunctive normal form: a conjunction of disjunctions of literals.
 *
 *  Pipeline: [[simplifyLogic]] fixpoint (folds connective-level constants away), desugar
 *  `implies`/`xor`, push `not` to the leaves via [[https://en.wikipedia.org/wiki/De_Morgan%27s_laws De Morgan]], then distribute `or` over
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
def toCNF(e: _Expression, symmetric: Boolean = false): _Expression =
  toNormalForm(e, conjunctive = true, symmetric, MaxNormalFormClauses)


/** [[toCNF]] under the caller's clause budget, `limits(Limit.MaxNormalFormClauses)` (F_0124).
 *
 *  @param e         the expression to normalise
 *  @param symmetric whether the symmetric ternary digits `{-1, 0, 1}` are in scope
 *  @param limits    the work budgets; only the clause cap is read
 *  @return the CNF of `e`, or `e` unchanged when it is not crisp or the distribution passes the cap
 */
def toCNF(e: _Expression, symmetric: Boolean, limits: Limits): _Expression =
  toNormalForm(e, conjunctive = true, symmetric, limits(Limit.MaxNormalFormClauses))


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
def toDNF(e: _Expression, symmetric: Boolean = false): _Expression =
  toNormalForm(e, conjunctive = false, symmetric, MaxNormalFormClauses)


/** [[toDNF]] under the caller's clause budget, `limits(Limit.MaxNormalFormClauses)` (F_0124).
 *
 *  @param e         the expression to normalise
 *  @param symmetric whether the symmetric ternary digits `{-1, 0, 1}` are in scope
 *  @param limits    the work budgets; only the clause cap is read
 *  @return the DNF of `e`, or `e` unchanged when it is not crisp or the distribution passes the cap
 */
def toDNF(e: _Expression, symmetric: Boolean, limits: Limits): _Expression =
  toNormalForm(e, conjunctive = false, symmetric, limits(Limit.MaxNormalFormClauses))


/** Shared CNF/DNF pipeline; `conjunctive` selects which connective is the outer level.
 *
 *  Returns `e` untouched when it carries a `_Truth` degree: clause-level cleanup rests on
 *  the complement law (`a or not a` is a trivially-true clause), which fails at `unknown`.
 */
private def toNormalForm(e: _Expression, conjunctive: Boolean, symmetric: Boolean,
                         maxClauses: Long): _Expression =
  normalForm(e, conjunctive, symmetric, maxClauses).getOrElse(e)


/** The normal form of `e`, or `None` when there is none to give: `e` carries a graded truth
 *  value, or the distribution passes `maxClauses`.
 *
 *  [[toCNF]] and [[toDNF]] answer `e` itself in both cases, which cannot be told apart from an
 *  input already in normal form; [[_NormalForm]] needs the difference, so it stays unevaluated
 *  on a refusal rather than presenting the input as its own normal form (F_0133).
 */
private def normalForm(e: _Expression, conjunctive: Boolean, symmetric: Boolean,
                       maxClauses: Long): Option[_Expression] =
  Option.when(isCrisp(e, symmetric))(simplifyLogicFully(e, identity, symmetric)).flatMap { simplified =>
    clauseSet(nnf(simplified, negated = false), conjunctive, maxClauses).map(rebuildNormal(_, conjunctive))
  }


/** Which normal form a [[_NormalForm]] asks for (F_0133), with its grammar keyword. */
enum NormalKind(val word: String) extends Keyworded:
  /** The grammar keyword (F_0148). */
  def keyword: String = word
  /** `cnf(e)` -- a conjunction of disjunctions of literals. */
  case CNF extends NormalKind("cnf")
  /** `dnf(e)` -- a disjunction of conjunctions of literals. */
  case DNF extends NormalKind("dnf")


/** `cnf(e)` / `dnf(e)` -- the conjunctive or disjunctive normal form of a logical expression
 *  at the prompt (F_0133).
 *
 *  The library has had [[toCNF]] and [[toDNF]] since 4.E; this node is their prompt form,
 *  chosen over a REPL command (Decision 1, 2026-10-17) so that it composes inside an expression
 *  and round-trips through `:save`.  The operand is evaluated first, so a bound name takes its
 *  value; a concrete truth value is its own normal form.  On a refusal -- a graded truth value
 *  anywhere in the operand, or more than the environment's `Limit.MaxNormalFormClauses`
 *  clauses -- the node stays unevaluated rather than answering the operand as though it were
 *  already in normal form.
 *
 *  @param kind which normal form
 *  @param e    the logical expression
 */
case class _NormalForm(kind: NormalKind, e: _Expression) extends _Expression:
  override def toString: String = s"${kind.word}($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _NormalForm(kind, c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(v) => Right(v)
      case Left(x)  =>
        normalForm(x, kind == NormalKind.CNF, env.symmetricLogic, env.limits(Limit.MaxNormalFormClauses))
          .fold(Left(_NormalForm(kind, x)))(Left(_))


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
 *  `None` when the clause count exceeds `maxClauses`.
 */
private def clauseSet(e: _Expression, conjunctive: Boolean,
                      maxClauses: Long): Option[Vector[Vector[_Expression]]] =
  def merge(a: _Expression, b: _Expression): Option[Vector[Vector[_Expression]]] =
    for x <- clauseSet(a, conjunctive, maxClauses); y <- clauseSet(b, conjunctive, maxClauses)
        r <- Option.when(x.size.toLong + y.size <= maxClauses)(x ++ y)
    yield r
  // the product size is checked BEFORE materialising it, so an over-cap distribution
  // costs nothing but the sub-results
  def cross(a: _Expression, b: _Expression): Option[Vector[Vector[_Expression]]] =
    for x <- clauseSet(a, conjunctive, maxClauses); y <- clauseSet(b, conjunctive, maxClauses)
        r <- Option.when(x.size.toLong * y.size <= maxClauses)(
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
