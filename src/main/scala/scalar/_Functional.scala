package it.grypho.scala.leonardo
package scalar

import core.*


/** Direction of approach for a limit expression. */
enum LimitDir:
  /** Two-sided limit (default). */
  case Both
  /** One-sided limit approaching from the right (`x -> p+`). */
  case FromRight
  /** One-sided limit approaching from the left (`x -> p-`). */
  case FromLeft


/** Base class for higher-order operators that take an expression and a variable and
 *  produce a new expression: differentiation, integration, and limits.
 *
 *  The algorithms live in their own files (`Derive.scala`, `Integrate.scala`,
 *  `Limit.scala`); subclasses here are the AST nodes that carry the unevaluated form.
 *
 *  `children` / `rebuild` convention: the binder variable (`v` in `derive(e, v)`,
 *  `integral(e, v)`, `limit(e, v, point)`) is excluded from `children` -- it names the
 *  variable of differentiation/integration, not a use-site occurrence -- so traversals
 *  ([[substitute]], [[dependsOn]]) never recurse into it.
 *  `_DefIntegral`'s `children` include `low_limit` and `up_limit` because they are
 *  regular expression positions subject to substitution and `dependsOn` checks.
 */
abstract class _Functional extends _Expression


/** AST node for symbolic differentiation: `derive(e, v)` in the grammar.
 *
 *  `eval` delegates to [[derive]], and guards against the fixpoint case where [[derive]]
 *  returns `this` (would loop forever): if the algorithm cannot reduce, stays symbolic.
 *
 *  @param e the expression to differentiate
 *  @param v the differentiation variable (binder -- excluded from `children`)
 */
case class _Derivative(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"derive($e, $v)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Derivative(c.head, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val derivative = it.grypho.scala.leonardo.scalar.derive(e, v)
    // derive returns this same _Derivative node when it cannot reduce further (e.g.
    // differentiating an integral w.r.t. an unrelated variable). Re-evaluating that
    // would call derive on the identical node forever, so stay symbolic instead.
    if derivative == this then Left(this)
    else derivative.eval(env)


/** AST node for symbolic indefinite integration: `integral(e, v)` in the grammar.
 *
 *  `eval` delegates to [[integrate]], guarded against the fixpoint where [[integrate]]
 *  returns `this` (mirrors `_Derivative.eval`'s termination guard).
 *
 *  @param e the integrand
 *  @param v the integration variable (binder -- excluded from `children`)
 */
case class _Integral(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"integral($e, $v)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Integral(c.head, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val antiderivative = it.grypho.scala.leonardo.scalar.integrate(e, v)
    // integrate returns this same _Integral node when no rule applies. Re-evaluating
    // it would call integrate on the identical node forever, so stay symbolic instead
    // (mirrors _Derivative.eval's termination guard).
    if antiderivative == this then Left(this)
    else antiderivative.eval(env)


/** AST node for a definite integral: `integral(e, v, lo, hi)` in the grammar.
 *
 *  `eval` numerically integrates with composite Simpson's rule when `low_limit` and
 *  `up_limit` fold to concrete numbers.  Step count is scaled by the interval length
 *  and the session precision.  The fast path [[compile]]s the integrand to a
 *  `Double => Double` closure; the fallback evaluates the tree per step.
 *
 *  @param e         the integrand
 *  @param v         the integration variable (binder -- excluded from `children`)
 *  @param low_limit lower bound of integration
 *  @param up_limit  upper bound of integration
 */
case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional:
  override def toString: String = s"integral($e, $v, $low_limit, $up_limit)"
  override def children: List[_Expression] = List(e, low_limit, up_limit)
  override def rebuild(c: List[_Expression]): _Expression = _DefIntegral(c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (low_limit.eval(env), up_limit.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) =>
        val rawN = (env.precision.toLong * 200L).max(100L).min(Int.MaxValue.toLong).toInt
        val n    = if rawN % 2 == 0 then rawN else rawN + 1
        val h    = (b - a) / n

        compile(e, v, env) match
          case Some(f) =>
            // Fast path: compiled closure -- no per-step tree traversal or env allocation.
            @annotation.tailrec
            def fastLoop(i: Int, acc: Double): Double =
              if i > n then acc
              else
                val coeff = if i == 0 || i == n then 1.0
                            else if i % 2 == 1   then 4.0
                            else                       2.0
                fastLoop(i + 1, acc + coeff * f(a + i * h))
            val s = h / 3.0 * fastLoop(0, 0.0)
            if s.isNaN || s.isInfinite then Left(this) else Right(_Number(s))

          case None =>
            // Fallback: tree-evaluation per step (handles symbolic sub-expressions).
            @annotation.tailrec
            def loop(i: Int, acc: Double): Option[Double] =
              if i > n then Some(acc)
              else
                e.eval(env.withBinding(v.variable, _Number(a + i * h))) match
                  case Right(_Number(y)) =>
                    val coeff = if i == 0 || i == n then 1.0
                                else if i % 2 == 1   then 4.0
                                else                       2.0
                    loop(i + 1, acc + coeff * y)
                  case _ => None

            loop(0, 0.0) match
              case Some(s) =>
                val result = h / 3.0 * s
                if result.isNaN || result.isInfinite then Left(this) else Right(_Number(result))
              case None    => Left(this)

      case _ => Left(this)


/** AST node for a limit expression: `limit(e, v, point)` or `limit(e, v, point, +/-)`.
 *
 *  `eval` delegates to [[evalLimit]], guarded against the fixpoint where it returns
 *  `this` (stays symbolic).  The binder variable `v` is excluded from `children`;
 *  `point` is included because it may contain other free variables.
 *
 *  @param e     the expression whose limit to compute
 *  @param v     the approach variable (binder -- excluded from `children`)
 *  @param point the limit point
 *  @param dir   the direction of approach (default: [[LimitDir.Both]])
 */
case class _Limit(e: _Expression, v: _Variable, point: _Expression, dir: LimitDir = LimitDir.Both) extends _Functional:
  override def toString: String = dir match
    case LimitDir.Both      => s"limit($e, $v, $point)"
    case LimitDir.FromRight => s"limit($e, $v, $point, +)"
    case LimitDir.FromLeft  => s"limit($e, $v, $point, -)"
  override def children: List[_Expression] = List(e, point)
  override def rebuild(c: List[_Expression]): _Expression = _Limit(c.head, v, c(1), dir)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = evalLimit(e, v, point, dir, env)
    if result == this then Left(this)
    else result.eval(env)


/** Taylor expansion of `e` in `v` about `point`, truncated after order `n`:
 *  `taylor(e, v, point, n)`.
 *
 *  `v` is the *expansion* variable, not a binder: unlike `_Derivative`'s or `_Integral`'s
 *  variable it appears **free in the result**, which is a polynomial in `(v - point)` —
 *  the same role the output variable plays in `_Laplace`.  It is nevertheless kept out of
 *  `children` and carried through `rebuild`, so `substitute` cannot rewrite the variable
 *  the expansion is taken in.
 *
 *  `eval` builds the series with [[taylorSeries]] and evaluates the result, so a bound `v`
 *  folds the polynomial to a number while a free one leaves it symbolic.  It stays
 *  symbolic when `n` does not reduce to an integer in `0 .. MaxTaylorOrder`, or when some
 *  coefficient cannot be differentiated (`Gamma`, `fact`, ... — see `hasDerivative`).
 *
 *  `maclaurin(e, v, n)` in the grammar is sugar for `taylor(e, v, 0, n)` and prints in
 *  that form, exactly as `log(x)` prints as `log(x, 10)`.
 *
 *  @param e     the expression to expand
 *  @param v     the expansion variable
 *  @param point the centre of the expansion
 *  @param n     the truncation order
 */
case class _Taylor(e: _Expression, v: _Variable, point: _Expression, n: _Expression)
    extends _Functional:
  override def toString: String = s"taylor($e, $v, $point, $n)"
  override def children: List[_Expression] = List(e, point, n)
  override def rebuild(c: List[_Expression]): _Expression = _Taylor(c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    n.eval(env) match
      case Right(_Number(d)) if d.isWhole && d >= 0 && d <= MaxTaylorOrder =>
        // point is NOT required to reduce: expanding about a symbolic centre is
        // meaningful, so it is passed through as it stands.
        taylorSeries(e, v, point, d.toInt) match
          case Some(series) => series.eval(env)
          case None         => Left(this)
      case _ => Left(this)

/** Truncated Fourier series of `e` in `v` over one period centred on zero:
 *  `fourierSeries(e, v, period, n)`.
 *
 *  Like [[_Taylor]], `v` is the *expansion* variable rather than a binder: it appears
 *  free in the result.  It is kept out of `children` and carried through `rebuild` so
 *  `substitute` cannot rewrite the variable the expansion is taken in.
 *
 *  Unlike `_Taylor`, the coefficients are **numeric**: each is a definite integral
 *  evaluated by Simpson's rule, so `period` must reduce to a concrete positive number and
 *  the integrand must be evaluable over `[-period/2, period/2]`.  Anything else leaves
 *  the node symbolic, the fixpoint convention shared with the transforms.
 *
 *  Not to be confused with `transform._Fourier` (`fourier(e, t, w)`), which is the
 *  Fourier *transform* -- a different operation with a different result type.
 *
 *  @param e      the expression to expand
 *  @param v      the expansion variable
 *  @param period the period, `T`
 *  @param n      the highest harmonic retained
 */
case class _FourierSeries(e: _Expression, v: _Variable, period: _Expression, n: _Expression)
    extends _Functional:
  override def toString: String = s"fourierSeries($e, $v, $period, $n)"
  override def children: List[_Expression] = List(e, period, n)
  override def rebuild(c: List[_Expression]): _Expression = _FourierSeries(c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (period.eval(env), n.eval(env)) match
      case (Right(_Number(t)), Right(_Number(d))) if d.isWhole && d >= 0 && d <= MaxFourierOrder =>
        fourierSeries(e, v, t, d.toInt, env) match
          case Some(series) => series.eval(env)
          case None         => Left(this)
      case _ => Left(this)

/** Padé approximant `[m/n]` of `e` about zero: `pade(e, v, m, n)`.
 *
 *  The rational function `P/Q` with `deg P <= m`, `deg Q <= n` and `Q(0) = 1` whose
 *  Maclaurin series agrees with `e`'s through order `m + n`.  Frequently a much better
 *  approximation than the Taylor polynomial of the same total degree, because a rational
 *  function can model a nearby pole that no polynomial can.
 *
 *  Follows [[_Taylor]]'s convention that `v` is the expansion variable rather than a
 *  binder -- free in the result, but excluded from `children` so `substitute` cannot
 *  rewrite it.  Like `_FourierSeries` the coefficients are numeric, so `m` and `n` must
 *  fold to non-negative integers with `m + n <= MaxTaylorOrder` and every Maclaurin
 *  coefficient must reduce; anything else leaves the node symbolic, including the case
 *  where the linear system is singular and no `[m/n]` approximant exists.
 *
 *  @param e the expression to approximate
 *  @param v the expansion variable
 *  @param m the numerator degree
 *  @param n the denominator degree
 */
case class _Pade(e: _Expression, v: _Variable, m: _Expression, n: _Expression)
    extends _Functional:
  override def toString: String = s"pade($e, $v, $m, $n)"
  override def children: List[_Expression] = List(e, m, n)
  override def rebuild(c: List[_Expression]): _Expression = _Pade(c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (m.eval(env), n.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b)))
          if a.isWhole && b.isWhole && a >= 0 && b >= 0 && a + b <= MaxTaylorOrder =>
        padeApproximant(e, v, a.toInt, b.toInt, env) match
          case Some(r) => r.eval(env)
          case None    => Left(this)
      case _ => Left(this)

/** AST node for `laurent(e, v, point, m, n)` — issue 3.4.
 *
 *  Expands `e` about an isolated singularity as `Σ(k = −m to n) c_k·(v − point)ᵏ`.
 *
 *  Follows [[_Taylor]]'s convention that `v` is the expansion variable rather than a binder
 *  — free in the result, but excluded from `children` so `substitute` cannot rewrite the
 *  variable the expansion is taken in.
 *
 *  **`m` is optional.**  When absent the pole order is detected with `singularitiesOf`
 *  (3.3 slice D), whose `Pole(order)` *is* the length of the principal part.  A `Removable`
 *  singularity gives `m = 0`, which is an ordinary Taylor series — returned rather than
 *  refused, since the caller asked a well-formed question that simply has no principal part.
 *
 *  Stays symbolic when the order cannot be determined, when a coefficient does not fold
 *  finite, or when the point is an **essential** singularity: there the principal part is
 *  infinite, and a truncation would carry a different contract from the polynomial tiers —
 *  the discarded terms blow up near the point instead of becoming small.
 *
 *  @param e     the expression to expand
 *  @param v     the expansion variable
 *  @param point the singularity
 *  @param m     the pole order, or `None` to detect it
 *  @param n     the highest non-negative power retained
 */
case class _Laurent(e: _Expression, v: _Variable, point: _Expression,
                    m: Option[_Expression], n: _Expression) extends _Functional:
  override def toString: String =
    m.fold(s"laurent($e, $v, $point, $n)")(mm => s"laurent($e, $v, $point, $mm, $n)")
  override def children: List[_Expression] = List(e, point) ++ m.toList :+ n
  override def rebuild(c: List[_Expression]): _Expression =
    if m.isDefined then _Laurent(c.head, v, c(1), Some(c(2)), c(3))
    else _Laurent(c.head, v, c(1), None, c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val order = m match
      case Some(me) => me.eval(env) match
        case Right(_Number(d)) if d.isWhole && d >= 0 => Some(d.toInt)
        case _                                        => None
      case None => detectedOrder(env)
    (order, n.eval(env)) match
      case (Some(mm), Right(_Number(nn))) if nn.isWhole && nn >= 0 =>
        laurentSeries(e, v, point, mm, nn.toInt, env) match
          case Some(r) => r.eval(env)
          case None    => Left(this)
      case _ => Left(this)

  /** The pole order at `point`, from the 3.3 singularity classifier. */
  private def detectedOrder(env: Environment): Option[Int] =
    point.eval(env) match
      case Right(_Number(a)) if !a.isNaN && !a.isInfinite =>
        singularitiesOf(e, v, env).flatMap { ss =>
          ss.find(s => math.abs(s.at - a) < 1e-6).map(_.kind) match
            case Some(SingularityKind.Pole(o)) => Some(o)
            case Some(SingularityKind.Removable) => Some(0)
            // Not a singularity at all: an ordinary Taylor expansion is the right answer.
            case None => Some(0)
        }
      case _ => None
