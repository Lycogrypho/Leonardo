package it.grypho.scala.leonardo
package scalar

import core.*


/** Sign determination for an expression — issue 3.2's central primitive.
 *
 *  Dividing an inequality by a coefficient **flips its direction** when that coefficient is
 *  negative, so every tier of the inequality solver has to ask the same question first: what
 *  is the sign of this divisor?  The naive test — "is it a numeric literal?" — is wrong in
 *  both directions:
 *
 *  - it refuses `exp(a) * x > b`, which is perfectly solvable, because `exp` is *strictly
 *    positive for every real argument* and the direction therefore cannot flip;
 *  - it says nothing useful about `a^2`, which fails not because its sign is unknown but
 *    because it **may be zero**, and dividing by it is invalid whatever its sign.
 *
 *  So the question is answered by a rule table in the shape of [[derive]] / [[integrate]]:
 *  specific rules first, `None` to give up and leave the caller symbolic.
 *
 *  `Some(0)` means *provably exactly zero*, which is a stronger claim than "might be zero" —
 *  `a^2` yields `None`, not `Some(0)`, because it is zero only at one point.  A caller that
 *  wants to divide must therefore see `Some(1)` or `Some(-1)`; both `Some(0)` and `None`
 *  refuse, for different reasons.
 *
 *  This table is shared with issue 6.13, whose `Positive` / `NonNegative` domain
 *  requirements are the same machinery viewed from the other side.  It lives in `scalar`
 *  precisely so both `equation` (inequalities) and a future domain analysis can reach it.
 */

/** The sign of `e` under `env`, when it can be determined.
 *
 *  @param e   the expression whose sign is wanted
 *  @param env supplies bindings, so a bound variable participates
 *  @return `Some(1)` strictly positive, `Some(-1)` strictly negative, `Some(0)` provably
 *          zero, or `None` when it cannot be determined
 */
private[leonardo] def sign(e: _Expression, env: Environment): Option[Int] =
  concreteSign(e, env).orElse(structuralSign(e, env))

/** Whether `e` is provably non-zero, which is what division actually requires. */
private[leonardo] def isNonZero(e: _Expression, env: Environment): Boolean =
  sign(e, env).exists(_ != 0)

/** The sign of a value that folds to a concrete real number.
 *
 *  `_Number` is a widening extractor, so an exact `_Rational` is read here too — reading it
 *  as a `Double` is safe for a *sign*, which no rounding can change. A `_Complex` has no
 *  order and falls through to `None`.
 */
private def concreteSign(e: _Expression, env: Environment): Option[Int] = e.eval(env) match
  case Right(_Number(d)) => if d.isNaN then None else Some(math.signum(d).toInt)
  case _                 => None

/** The sign of an expression that stays symbolic, from the shape of its operator. */
private def structuralSign(e: _Expression, env: Environment): Option[Int] = e match
  // exp is strictly positive on the whole real line -- the rule that makes this table
  // worth having, since it decides a case no literal test could.
  case Exp(_) => Some(1)

  // An even power is non-negative, and strictly positive exactly when its base is non-zero.
  // A base of unknown sign gives `None` rather than "non-negative", because the caller's
  // question is whether it may be zero.
  case Power(b, _Number(n)) if n > 0 && n == n.toInt && n.toInt % 2 == 0 =>
    sign(b, env) match
      case Some(0) => Some(0)
      case Some(_) => Some(1)
      case None    => None

  case Product(a, b) => for sa <- sign(a, env); sb <- sign(b, env) yield sa * sb

  // A zero denominator is not a sign question but an undefined one, so it gives up.
  case Ratio(a, b) =>
    for sa <- sign(a, env); sb <- sign(b, env); if sb != 0 yield sa * sb

  // Like signs add to that sign; opposite signs cancel unpredictably and give up.
  //
  // The last two arms are what makes `a^2 + 1` come out strictly positive.  `sign(a^2)` is
  // `None` — correctly, since it may be zero and so cannot be divided by — but a *sum* asks
  // a weaker question: adding something that is merely non-negative to something strictly
  // positive is still strictly positive.  Without this the table would answer `None` for
  // every `x^2 + c` discriminant-style expression, which is a common divisor.
  case Sum(a, b) =>
    (sign(a, env), sign(b, env)) match
      case (Some(x), Some(y)) if x == y            => Some(x)
      case (Some(0), Some(y))                      => Some(y)
      case (Some(x), Some(0))                      => Some(x)
      case (Some(1), _) if isNonNegative(b, env)   => Some(1)
      case (_, Some(1)) if isNonNegative(a, env)   => Some(1)
      case (Some(-1), _) if isNonPositive(b, env)  => Some(-1)
      case (_, Some(-1)) if isNonPositive(a, env)  => Some(-1)
      case _                                       => None

  case _ => None

/** Whether `e` is `>= 0` — weaker than a strict sign, and deliberately so.
 *
 *  An even power is the motivating case: it cannot be divided by, because it may be zero,
 *  yet it can safely be *added* to a strictly positive quantity.
 *
 *  Widened from `private` to `private[leonardo]` by issue 3.3, whose `NonNegative` domain
 *  requirement asks exactly this question from the other side.
 */
private[leonardo] def isNonNegative(e: _Expression, env: Environment): Boolean =
  sign(e, env).exists(_ >= 0) || (e match
    case Power(_, _Number(n)) if n > 0 && n == n.toInt && n.toInt % 2 == 0 => true
    case Sum(a, b)                                                        => isNonNegative(a, env) && isNonNegative(b, env)
    case _                                                                => false)

/** Whether `e` is `<= 0`, the mirror of [[isNonNegative]]. */
private[leonardo] def isNonPositive(e: _Expression, env: Environment): Boolean =
  sign(e, env).exists(_ <= 0) || (e match
    case Product(k, r) if sign(k, env).contains(-1) => isNonNegative(r, env)
    case Product(r, k) if sign(k, env).contains(-1) => isNonNegative(r, env)
    case Sum(a, b)                                  => isNonPositive(a, env) && isNonPositive(b, env)
    case _                                          => false)
