package it.grypho.scala.leonardo
package scalar

import core.*


/** A pattern-variable unifier and rule engine, so a rule can be **data** — issue 6.21.
 *
 *  **The hybrid, not the migration.**  The ~389 existing `case` arms across `scalar`,
 *  `transform` and `probability` stay compiled Scala matches: porting them would buy
 *  ergonomics and no coverage, while trading away exhaustiveness checking and static types
 *  on exactly the tables that are hardest to get right.  This engine exists for rules that
 *  are *expected to keep growing* — principally the table of integrals — where being data
 *  genuinely pays.
 *
 *  **It lives in `scalar`, not in a `rewrite/` package of its own, and that is forced.**  A
 *  separate package would be a cycle: rules must be expressible over `scalar` nodes (`Sin`,
 *  `Exp`, `Power`), so it would import `scalar`; and [[integrate]] must consult the table, so
 *  `scalar` would import it back.  An algorithm module here is also the package's own
 *  convention — `Derive`, `Integrate`, `Simplify`, `Normalize`, `Sign`, `Domain` and
 *  `Polynomial` are all exactly that.
 *
 *  **The data table is consulted last**, after every compiled arm has declined.  That is the
 *  property that makes the hybrid cheap: nothing which works today can change behaviour,
 *  because only expressions that currently give up can newly match.
 */

/** A pattern variable: matches any sub-expression and binds it under `name`.
 *
 *  Never appears in a user expression — it exists only inside a rule's left-hand side — so
 *  nothing outside this file has to recognise it, and `core` needs no change to accommodate
 *  it.  `eval` stays symbolic, which is the correct answer for a thing that is not a value.
 */
case class _Pattern(name: String) extends _Expression:
  override def toString: String = s"?$name"
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this
  override def eval(env: Environment): Either[_Expression, _Value] = Left(this)


/** A rewrite rule: match `lhs`, check `condition`, produce `rhs`.
 *
 *  @param lhs       the pattern, built from ordinary nodes plus [[_Pattern]] holes
 *  @param rhs       the template, whose `_Pattern` holes are filled from the match
 *  @param condition a guard over the bindings *and the context variable* — `∫ dv/(a²+v²)`
 *                   needs the hole `a` to be free of the integration variable, which is only
 *                   readable once that variable is threaded in (issue 3.8).  The combinators
 *                   below ([[freeOf]], [[nonZero]], …) are the intended way to build one.
 *  @param name      a label, for diagnostics and test failure messages
 */
case class RewriteRule(lhs: _Expression, rhs: _Expression,
                       condition: (Map[String, _Expression], _Variable) => Boolean = (_, _) => true,
                       name: String = "")


/** Matches `pattern` against `e`, returning the pattern-variable bindings.
 *
 *  **Binding consistency is the part that is easy to omit**: a name occurring twice must
 *  bind the *same* sub-expression, or `f(?a, ?a)` would happily match `f(1, 2)` and the rule
 *  would fire on an expression it does not describe.
 *
 *  @return the bindings, or `None` when the shapes do not match
 */
private[leonardo] def unify(pattern: _Expression,
                            e: _Expression): Option[Map[String, _Expression]] =
  unifyInto(pattern, e, Map.empty)

private def unifyInto(p: _Expression, e: _Expression,
                      acc: Map[String, _Expression]): Option[Map[String, _Expression]] =
  p match
    case _Pattern(n) =>
      acc.get(n) match
        case Some(bound) => Option.when(bound == e)(acc)   // must bind consistently
        case None        => Some(acc + (n -> e))
    case _ =>
      val pc = p.children
      val ec = e.children
      // `p.rebuild(ec) == e` is the whole structural test in one line: it checks the node
      // types agree AND that every field which is *not* a child agrees too -- the binder
      // variable of a `_Derivative`, the operator of a `_Comparison`, the base of a
      // `_Based`. Comparing `getClass` alone would let `derive(f, x)` match `derive(f, y)`.
      if pc.sizeCompare(ec) != 0 || p.rebuild(ec) != e then None
      else pc.zip(ec).foldLeft(Option(acc)) { (a, pair) =>
        a.flatMap(m => unifyInto(pair._1, pair._2, m))
      }

/** Fills a template's [[_Pattern]] holes from `bindings`.
 *
 *  @return the instantiated expression, or `None` if the template names a hole the match
 *          did not bind (a malformed rule, caught rather than producing a `?x` in output)
 */
private[leonardo] def instantiate(template: _Expression,
                                  bindings: Map[String, _Expression]): Option[_Expression] =
  template match
    case _Pattern(n) => bindings.get(n)
    case other =>
      val kids = other.children.map(instantiate(_, bindings))
      Option.when(kids.forall(_.isDefined))(other.rebuild(kids.flatten))

/** The result of the first rule whose pattern matches `e` and whose condition holds.
 *
 *  Order is significant: rules are tried as given, so a more specific rule must be listed
 *  before a more general one — the same discipline the compiled `case` tables already use.
 *
 *  @param v the context variable passed to each rule's condition (the integration variable
 *           for the integral table); rules whose condition ignores it are unaffected
 */
private[leonardo] def applyRules(rules: List[RewriteRule],
                                 e: _Expression, v: _Variable): Option[_Expression] =
  rules.iterator
    .flatMap(r => unify(r.lhs, e).filter(r.condition(_, v)).flatMap(b => instantiate(r.rhs, b)))
    .nextOption()

/** Rewrites `e` to a fixpoint, innermost-first, capped at [[MaxRewriteSteps]].
 *
 *  The cap follows the convention of `MaxTaylorOrder` and `MaxNormalFormClauses`: a rule set
 *  can be non-terminating (`a + b -> b + a` is enough), and giving up is preferable to
 *  hanging.  Reaching the cap returns the expression as it stands.
 */
private[leonardo] def rewriteFully(rules: List[RewriteRule], e: _Expression,
                                   v: _Variable): _Expression =
  var current = e
  var steps   = 0
  var changed = true
  while changed && steps < MaxRewriteSteps do
    val next = rewriteOnce(rules, current, v)
    changed = next != current
    current = next
    steps  += 1
  current

/** One innermost-first pass: children are rewritten before the node itself is tried. */
private def rewriteOnce(rules: List[RewriteRule], e: _Expression, v: _Variable): _Expression =
  val withKids = e.rebuild(e.children.map(rewriteOnce(rules, _, v)))
  applyRules(rules, withKids, v).getOrElse(withKids)

// ── condition combinators (issue 3.8) ────────────────────────────────────────
// One place to build a rule's guard.  Each returns a `(bindings, contextVariable)`
// predicate, so it drops straight into `RewriteRule.condition`.  They compose with `&&`.

/** A rule condition — the shape [[RewriteRule.condition]] expects. */
private[leonardo] type Condition = (Map[String, _Expression], _Variable) => Boolean

/** The closed numeric value of a binding, or `None` when it is symbolic or non-finite.
 *
 *  `_Number` is the widening extractor, so a `_Rational` reads through it as a `Double` too.
 */
private def numericValue(e: _Expression): Option[Double] =
  if e.freeVars.nonEmpty then None
  else e.eval(new Environment()) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
    case _                                              => None

/** The binding for `name` exists and does not mention the context variable. */
private[leonardo] def freeOf(name: String): Condition =
  (b, v) => b.get(name).exists(!_.freeVars.contains(v.variable))

/** The binding for `name` is a concrete real number. */
private[leonardo] def isNumeric(name: String): Condition =
  (b, _) => b.get(name).flatMap(numericValue).isDefined

/** The binding for `name` is a positive integer. */
private[leonardo] def isPositiveInteger(name: String): Condition =
  (b, _) => b.get(name).flatMap(numericValue).exists(d => d > 0.0 && d == math.floor(d))

/** The binding for `name` is *not provably zero* — a symbolic binding passes; only one that
 *  evaluates to `0` fails.  This is the denominator-safety reading: it admits a symbolic
 *  parameter while still refusing a literal zero.
 */
private[leonardo] def nonZero(name: String): Condition =
  (b, _) => b.get(name).exists(e => !numericValue(e).contains(0.0))

/** The bindings for `a` and `b` both exist and are structurally distinct. */
private[leonardo] def distinct(a: String, b: String): Condition =
  (bnd, _) => bnd.get(a).zip(bnd.get(b)).exists((x, y) => x != y)

/** Conjunction of two conditions, so a rule can require several at once. */
extension (c: Condition)
  private[leonardo] def &&(d: Condition): Condition = (b, v) => c(b, v) && d(b, v)


/** Step cap for [[rewriteFully]] — a non-terminating rule set must give up, not hang. */
private[leonardo] val MaxRewriteSteps: Int = 100
