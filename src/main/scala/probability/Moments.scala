package it.grypho.scala.leonardo
package probability

import core.*
import scalar.{Sum, Product, Ratio, Power, dependsOn}


/** The linearity rule table behind `expect` and `variance`.
 *
 *  This is the part that belongs in a computer algebra system rather than in a statistics
 *  library: `E[aX + b] = a·E[X] + b` is a *rewrite*, applied to the expression's structure
 *  before any number is computed, so it works while `a` and `b` are still free variables.
 *
 *  Structured exactly like `scalar.Derive`: a rule table, most specific first, with a
 *  give-up fallback.  The give-up is `None`, which the calling node turns into "stay
 *  symbolic" — the termination guard every `_Functional` in this codebase shares.
 *
 *  **What is deliberately not here.**  `E[XY] = E[X]·E[Y]` requires the factors to be
 *  *independent*, and nothing in the language expresses independence between two random
 *  variables.  Asserting it would produce confidently wrong answers for correlated ones, so
 *  a product of two distinct random variables stays symbolic.  A product where only one
 *  factor is random is fine, and is the `a·X` rule below.
 *
 *  @see [[https://en.wikipedia.org/wiki/Expected_value#Properties Linearity of expectation]]
 */
object Moments:

  /** `E[e]`, where `x` names the random variable (or `None` for a bare distribution).
   *
   *  @param e   the expression to average
   *  @param x   the random variable, or `None` for the one-argument form
   *  @param env the evaluation environment
   *  @return the reduced result, or `None` to leave the caller symbolic
   */
  def expectation(e: _Expression, x: Option[_Variable],
                  env: Environment): Option[Either[_Expression, _Value]] =
    x match
      case None    => distributionOf(e, env).flatMap(d => d.mean.map(m => Right(_Number(m))))
      case Some(v) => rewriteE(e, v, env).map(_.eval(env))

  /** `Var(e)`, where `x` names the random variable (or `None` for a bare distribution).
   *
   *  @param e   the expression whose variance is wanted
   *  @param x   the random variable, or `None` for the one-argument form
   *  @param env the evaluation environment
   *  @return the reduced result, or `None` to leave the caller symbolic
   */
  def variance(e: _Expression, x: Option[_Variable],
               env: Environment): Option[Either[_Expression, _Value]] =
    x match
      case None    => distributionOf(e, env).flatMap(d => d.variance.map(v => Right(_Number(v))))
      case Some(v) => rewriteVar(e, v, env).map(_.eval(env))

  /** Rewrites `E[e]` by linearity into an expression free of any expectation.
   *
   *  Returns `None` when no rule applies, which is the honest answer for a shape whose
   *  expectation is not determined by linearity alone (`E[X²]`, `E[sin X]`, `E[XY]`).
   */
  private def rewriteE(e: _Expression, v: _Variable, env: Environment): Option[_Expression] =
    e match
      // E[c] = c for anything free of the random variable.
      case _ if isConstant(e, v, env) => Some(e)
      // E[X] = the distribution's mean.
      case w: _Variable if w.variable == v.variable =>
        env.get(v.variable) match
          case Some(d: _Distribution) => d.mean.map(m => _Number(m))
          case _                      => None
      // E[a + b] = E[a] + E[b]  -- holds unconditionally, no independence needed.
      case Sum(a, b)                 =>
        for ea <- rewriteE(a, v, env); eb <- rewriteE(b, v, env) yield Sum(ea, eb)
      // E[c*a] = c*E[a] for a constant factor on either side.
      case Product(a, b) if isConstant(a, v, env) => rewriteE(b, v, env).map(Product(a, _))
      case Product(a, b) if isConstant(b, v, env) => rewriteE(a, v, env).map(Product(_, b))
      // E[a/c] = E[a]/c.
      case Ratio(a, b) if isConstant(b, v, env)   => rewriteE(a, v, env).map(Ratio(_, b))
      // Everything else -- E[X^2], E[sin X], E[X*Y] -- is not linear, so give up.
      case _                                 => None

  /** Rewrites `Var(e)` into an expression free of any variance.
   *
   *  `Var(aX + b) = a²·Var(X)`: the shift drops out entirely, which is the rule worth
   *  having, and the scale comes out squared.
   */
  private def rewriteVar(e: _Expression, v: _Variable, env: Environment): Option[_Expression] =
    e match
      // Var(c) = 0.  The zero is built in the tier of the expression, per the numeric-tier
      // rule -- a hard-coded _Number would demote an exact computation around it.
      case _ if isConstant(e, v, env) => Some(_Rational.literalLike(0, e))
      case w: _Variable if w.variable == v.variable =>
        env.get(v.variable) match
          case Some(d: _Distribution) => d.variance.map(x => _Number(x))
          case _                      => None
      // Var(a + c) = Var(a) for a shift free of the random variable.
      case Sum(a, b) if isConstant(a, v, env) => rewriteVar(b, v, env)
      case Sum(a, b) if isConstant(b, v, env) => rewriteVar(a, v, env)
      // Var(c*a) = c^2 * Var(a).
      case Product(a, b) if isConstant(a, v, env) =>
        rewriteVar(b, v, env).map(r => Product(Power(a, _Rational.literalLike(2, e)), r))
      case Product(a, b) if isConstant(b, v, env) =>
        rewriteVar(a, v, env).map(r => Product(Power(b, _Rational.literalLike(2, e)), r))
      // Var(a/c) = Var(a)/c^2.
      case Ratio(a, b) if isConstant(b, v, env)   =>
        rewriteVar(a, v, env).map(r => Ratio(r, Power(b, _Rational.literalLike(2, e))))
      // Var(a + b) with BOTH random needs Cov(a, b), which is not expressible here.
      case _                                 => None

  /** Whether `e` may be treated as a CONSTANT when averaging over `v`.
   *
   *  Not simply "free of `v`", which is the trap this replaced: in `E[X·Y]` the factor `Y`
   *  is free of `X`, so a `!dependsOn` guard would pull it out of the expectation as though
   *  it were a constant and return `E[X]·Y`.  It is not a constant — it is another random
   *  variable, and `E[XY]` needs independence, which this language cannot express.
   *
   *  A subexpression is constant here when none of its free variables is bound to a
   *  distribution, so an unbound name still counts as constant (it is an ordinary symbol)
   *  while a second random variable does not.
   *
   *  @param e   the subexpression
   *  @param v   the random variable being averaged over
   *  @param env supplies the bindings that say which names are random
   */
  private def isConstant(e: _Expression, v: _Variable, env: Environment): Boolean =
    !dependsOn(e, v) && e.freeVars.forall(n => !env.get(n).exists(_.isInstanceOf[_Distribution]))
  /** The distribution an expression denotes, if it denotes one. */
  private def distributionOf(e: _Expression, env: Environment): Option[_Distribution] =
    e.eval(env) match
      case Right(d: _Distribution) => Some(d)
      case _                       => None
