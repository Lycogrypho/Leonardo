package it.grypho.scala.leonardo
package scalar

import core.*


/** Truncated power-series arithmetic (F_0085): the Taylor coefficients of an expression about a
 *  centre, computed bottom-up by recurrences at `O(n²)` per node, where repeated differentiation
 *  grew exponentially with the order.
 *
 *  **A coefficient is an expression** (Decision A): each is combined through `Sum`/`Product`/
 *  `Ratio` and folded by `eval`, so one engine serves numeric, exact and symbolic input alike —
 *  a rational stays rational, a free parameter (`exp(a*x)`) or a symbolic centre stays symbolic.
 *  Every constant the recurrences invent (`k`, `1/k`, `k!`, an exponent) is built in the input's
 *  tier, the numeric tier rule.
 *
 *  **Coverage** (Decision B): `+ − × ÷`, constant powers (`(1+u)^α`, integer or real `α`),
 *  `exp`/`ln`/`sin`/`cos`/`sinh`/`cosh` by their ODE recurrences, the reciprocal and quotient
 *  functions (`tan`, `sec`, `tanh`, …) as quotients of those, and the six inverse functions by
 *  **series integration** of their algebraic derivatives. Anything else — and a quotient or
 *  logarithm whose operand vanishes at the centre — gets **its own** series from the derivative
 *  path ([[seriesDerivatives]], budget included), so coverage never shrinks and a composite pays
 *  the exponential cost only on the part that needs it.
 */
private final class SeriesEngine(v: _Variable, centre: _Expression, order: Int, exact: Boolean,
                                 maxNodes: Int):

  private type Series = Vector[_Expression]
  private val env = new Environment()

  /** The integer `n` in the input's tier. */
  private def lit(n: Int): _Expression = if exact then _Rational(n) else _Number(n)

  /** `k!` in the input's tier — the divisor the derivative path invents. */
  def factorial(k: Int): _Expression =
    if exact then _Rational((1 to k).foldLeft(BigInt(1))(_ * _))
    else _Number(factorialOf(k.toDouble).getOrElse(Double.NaN))

  /** A coefficient folded as far as it goes: a value when it is one, else simplified. */
  private def norm(e: _Expression): _Expression = e.eval(env) match
    case Right(value)   => value
    case Left(symbolic) => simplifyFully(symbolic)

  private def plus(a: _Expression, b: _Expression)  = norm(Sum(a, b))
  private def times(a: _Expression, b: _Expression) = norm(Product(a, b))
  private def over(a: _Expression, b: _Expression)  = norm(Ratio(a, b))

  private val zero: _Expression = lit(0)
  private def constant(c: _Expression): Series = norm(c) +: Vector.fill(order)(zero)

  /** `Σ_{j=lo..hi} f(j)`, folded; `zero` when the range is empty. */
  private def sum(lo: Int, hi: Int)(f: Int => _Expression): _Expression =
    (lo to hi).foldLeft(zero)((acc, j) => plus(acc, f(j)))

  private def add(a: Series, b: Series): Series = a.lazyZip(b).map(plus)

  /** The Cauchy product, truncated at the order. */
  private def multiply(a: Series, b: Series): Series =
    Vector.tabulate(order + 1)(k => sum(0, k)(j => times(a(j), b(k - j))))

  /** Builds `c` term by term, each term from the ones before it. */
  private def recur(first: _Expression)(next: (Int, Vector[_Expression]) => _Expression): Series =
    (1 to order).foldLeft(Vector(first))((built, k) => built :+ next(k, built))

  /** The series of `e`, or why it has none. */
  def of(e: _Expression): Either[String, Series] =
    if !dependsOn(e, v) then Right(constant(e))
    else e match
      case _: _Variable          => Right(Vector.tabulate(order + 1)(k => if k == 0 then norm(centre) else if k == 1 then lit(1) else zero))
      case Sum(a, b)             => for sa <- of(a); sb <- of(b) yield add(sa, sb)
      case Product(a, b)         => for sa <- of(a); sb <- of(b) yield multiply(sa, sb)
      case Ratio(a, b)           => quotient(e, a, b)
      case Power(b, k) if !dependsOn(k, v) => power(e, b, k)
      case Power(b, k)           => of(Exp(Product(k, Ln(b))))
      case Exp(u)                => of(u).map(expOf)
      case Ln(u)                 => of(u).flatMap(su => if isZeroLiteral(su.head) then fallback(e) else Right(lnOf(su)))
      case LogBase(u, b)         => of(Ratio(Ln(u), Ln(b)))
      case Sin(u)                => of(u).map(su => trig(su, hyperbolic = false)._1)
      case Cos(u)                => of(u).map(su => trig(su, hyperbolic = false)._2)
      case Sinh(u)               => of(u).map(su => trig(su, hyperbolic = true)._1)
      case Cosh(u)               => of(u).map(su => trig(su, hyperbolic = true)._2)
      case Tg(u)                 => of(Ratio(Sin(u), Cos(u)))
      case Cot(u)                => of(Ratio(Cos(u), Sin(u)))
      case Sec(u)                => of(Ratio(lit(1), Cos(u)))
      case Csc(u)                => of(Ratio(lit(1), Sin(u)))
      case Tanh(u)               => of(Ratio(Sinh(u), Cosh(u)))
      case Coth(u)               => of(Ratio(Cosh(u), Sinh(u)))
      case Sech(u)               => of(Ratio(lit(1), Cosh(u)))
      case Csch(u)               => of(Ratio(lit(1), Sinh(u)))
      case Atan(u)               => integrated(e, u, w => Ratio(lit(1), Sum(lit(1), Power(w, lit(2)))))
      case Asin(u)               => integrated(e, u, w => Power(Sum(lit(1), Product(lit(-1), Power(w, lit(2)))), minusHalf))
      case Acos(u)               => integrated(e, u, w => Product(lit(-1), Power(Sum(lit(1), Product(lit(-1), Power(w, lit(2)))), minusHalf)))
      case Asinh(u)              => integrated(e, u, w => Power(Sum(Power(w, lit(2)), lit(1)), minusHalf))
      case Acosh(u)              => integrated(e, u, w => Power(Sum(Power(w, lit(2)), lit(-1)), minusHalf))
      case Atanh(u)              => integrated(e, u, w => Ratio(lit(1), Sum(lit(1), Product(lit(-1), Power(w, lit(2))))))
      case _                     => fallback(e)

  private def minusHalf: _Expression = norm(Ratio(lit(-1), lit(2)))

  /** `a/b` by series division; a denominator vanishing at the centre is the derivative path's. */
  private def quotient(e: _Expression, a: _Expression, b: _Expression): Either[String, Series] =
    for
      sa <- of(a)
      sb <- of(b)
      q  <- if isZeroLiteral(sb.head) then fallback(e)
            else Right(recur(over(sa.head, sb.head))((k, q) =>
              over(plus(sa(k), times(lit(-1), sum(1, k)(j => times(sb(j), q(k - j))))), sb.head)))
    yield q

  /** `b^α` for a constant `α`: a natural power by repeated products (exact, and defined when
   *  `b` vanishes at the centre), otherwise the recurrence `b·p′ = α·b′·p`, which needs `b₀ ≠ 0`. */
  private def power(e: _Expression, b: _Expression, k: _Expression): Either[String, Series] =
    of(b).flatMap { sb =>
      norm(k) match
        case _Number(d) if d >= 0 && d == math.floor(d) && d <= MaxNaturalPower =>
          Right((1 to d.toInt).foldLeft(constant(lit(1)))((acc, _) => multiply(acc, sb)))
        case alpha if !isZeroLiteral(sb.head) =>
          val alpha1 = plus(alpha, lit(1))
          Right(recur(norm(Power(sb.head, alpha)))((n, p) =>
            over(sum(1, n)(j => times(plus(times(alpha1, lit(j)), lit(-n)), times(sb(j), p(n - j)))),
                 times(lit(n), sb.head))))
        case _ => fallback(e)
    }

  /** `e^u`: `e₀ = exp(u₀)`, `eₖ = (1/k) Σ j·uⱼ·eₖ₋ⱼ`. */
  private def expOf(su: Series): Series =
    recur(norm(Exp(su.head)))((k, s) => over(sum(1, k)(j => times(times(lit(j), su(j)), s(k - j))), lit(k)))

  /** `ln u`: `lₖ = (uₖ − (1/k) Σ_{j<k} j·lⱼ·uₖ₋ⱼ) / u₀`. */
  private def lnOf(su: Series): Series =
    recur(norm(Ln(su.head)))((k, l) =>
      over(plus(su(k), times(lit(-1), over(sum(1, k - 1)(j => times(times(lit(j), l(j)), su(k - j))), lit(k)))), su.head))

  /** `(sin u, cos u)` or `(sinh u, cosh u)`, built together since each recurrence reads the other. */
  private def trig(su: Series, hyperbolic: Boolean): (Series, Series) =
    val (s0, c0) = if hyperbolic then (Sinh(su.head), Cosh(su.head)) else (Sin(su.head), Cos(su.head))
    val sign     = lit(if hyperbolic then 1 else -1)
    (1 to order).foldLeft((Vector(norm(s0)), Vector(norm(c0)))) { case ((s, c), k) =>
      val sk = over(sum(1, k)(j => times(times(lit(j), su(j)), c(k - j))), lit(k))
      val ck = times(sign, over(sum(1, k)(j => times(times(lit(j), su(j)), s(k - j))), lit(k)))
      (s :+ sk, c :+ ck)
    }

  /** `f(u)` as `f(u₀) + ∫ f′(u)·u′` (Decision B): one term-wise integration turns the series of the
   *  algebraic derivative into the function's own. `deriv` is written in the input's tier here
   *  rather than taken from `derive`, which would pick the `Double` tier for a bare `f(w)`. */
  private def integrated(e: _Expression, u: _Expression, deriv: _Expression => _Expression): Either[String, Series] =
    for
      su <- of(u)
      sg <- of(deriv(u))
    yield
      val du    = Vector.tabulate(order + 1)(k => if k < order then times(lit(k + 1), su(k + 1)) else zero)
      val prod  = multiply(sg, du)
      norm(e.rebuild(List(su.head))) +: Vector.tabulate(order)(k => over(prod(k), lit(k + 1)))

  /** The derivative path for one subtree: `f⁽ᵏ⁾(a)/k!`, each derivative held to `maxNodes`
   *  (the caller's `Limit.MaxDerivativeNodes`), so an uncovered node keeps exactly the coverage
   *  it had. */
  private def fallback(e: _Expression): Either[String, Series] =
    seriesDerivatives(e, v, order, maxNodes).map(_.zipWithIndex.map((d, k) =>
      norm(Ratio(substitute(d, Map(v.variable -> centre)), factorial(k)))))

  /** The largest natural power multiplied out term by term; past it the general recurrence runs.
   *  Fixed, not a `core.Limit` (F_0124 phase C): it chooses between two algorithms that give the
   *  same series, so it bounds no work a caller could trade for an answer. */
  private val MaxNaturalPower = 64


/** The Taylor coefficients `c₀ … cₙ` of `e` in `v` about `centre` (F_0085), or why there are none.
 *
 *  @param e      the expression
 *  @param v      the expansion variable
 *  @param centre the centre of the expansion
 *  @param order    the highest power
 *  @param maxNodes the derivative path's node budget for an uncovered subtree
 *  @return the coefficients of `(v − centre)ᵏ`, or the reason they cannot all be had
 */
private[scalar] def powerSeries(e: _Expression, v: _Variable, centre: _Expression, order: Int,
                                maxNodes: Int = MaxDerivativeNodes): Either[String, Vector[_Expression]] =
  SeriesEngine(v, centre, order, _Rational.containsExact(e) || _Rational.containsExact(centre),
               maxNodes).of(e)
