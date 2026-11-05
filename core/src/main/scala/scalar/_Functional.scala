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
    // The tier comes from the body AND the bound values of its free names (F_0171, V3): `a/t`
    // holds no number, but with `a := 1/3` its derivative must be exact.
    // The answer with like terms collected and common factors cancelled (F_0292, the owner's
    // decision): `derive(sin(x)^2 + cos(x)^2, x)` is 0.  Only this answer, not `derive` itself, which
    // every other pass calls.
    val derivative = simplifyAlgebraic(deriveInTier(e, v, _Rational.containsExactIn(e, env)))
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
    // The tier by V3 (F_0171), as `_Derivative` takes it: the integrand and the bound values of
    // its free names.
    val antiderivative = integrateInTier(e, v, _Rational.containsExactIn(e, env))
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
      // An infinite limit (F_0288): the antiderivative's limits, or nothing — no numeric half-line.
      case (Right(lo @ _Number(a)), Right(hi @ _Number(b))) if !a.isNaN && !b.isNaN && (a.isInfinite || b.isInfinite) =>
        if a == b then Left(this)
        else QuadratureWork.within(byAntiderivative(lo, hi, a, b, env)).toRight(this)
      case (Right(lo @ _Number(a)), Right(hi @ _Number(b))) if a.isFinite && b.isFinite =>
        if a == b then Right(_Rational.literalLike(0, e, env))
        else QuadratureWork.within {
          // One evaluation's setup — the antiderivative attempt and the domain analysis — is charged
          // to the outermost integral's budget too: inside a nested integral it runs per outer sample.
          Option.when(QuadratureWork.spend(QuadratureWork.SetupCost, env.limits(Limit.MaxQuadratureEvaluations)))(
            byAntiderivative(lo, hi, a, b, env)
              .orElse(Option.when(insideDomain(a, b, env))(
                if a < b then adaptiveSimpson(a, b, env) else adaptiveSimpson(b, a, env).map(-_)).flatten.map(_Number(_)))
              .orElse(complexAntiderivative(a, b, env))
          ).flatten
        }.toRight(this)
      case (l, h) =>
        val (lo, hi) = (l.toExpression, h.toExpression)
        symbolicAntiderivative(lo, hi, env) match
          // A limit that is a closed expression of named constants (`pi` in exact mode, F_0289):
          // the exact `F(b) − F(a)` first, then the limits read at the working precision.
          case Left(_) if lo.freeVars.isEmpty && hi.freeVars.isEmpty && (containsConstant(lo) || containsConstant(hi)) =>
            _DefIntegral(e, v, foldConstants(lo, env), foldConstants(hi, env)).eval(env) match
              case Right(r) => Right(r)
              case Left(_)  => Left(this)
          case other => other
  /** `F(b) − F(a)` for an integrand with complex values on the interval (F_0297, the owner's decision:
   *  `x^(1/3)` at a negative `x` is the principal complex value).  The real paths read the real domain
   *  and decline; here the antiderivative is certified on the interval itself: the integrand finite at
   *  201 points, `F` free of jumps (each increment within twice `sup|f|·Δx`), and `F′ = f` at five
   *  interior points by a central difference.  Only when the integrand is complex somewhere, so no
   *  real answer changes. */
  private def complexAntiderivative(a: Double, b: Double, env: Environment): Option[_Value] =
    val f = integrand(env)
    val antiderivative = integrateInTier(f, v, _Rational.containsExactIn(e, env))
    def unresolved(x: _Expression): Boolean = x match
      case _: _Integral | _: _Derivative => true
      case other                         => other.children.exists(unresolved)
    def at(x: _Expression, t: Double): Option[(Double, Double)] =
      x.eval(env.withBinding(v.variable, _Number(t))) match
        case Right(c: _Complex)              => _Complex.parts(c).filter((p, q) => p.isFinite && q.isFinite)
        case Right(_Number(d)) if d.isFinite => Some((d, 0.0))
        case _                               => None
    def dist(p: (Double, Double), q: (Double, Double)): Double = math.hypot(p._1 - q._1, p._2 - q._2)
    val n  = 200
    val xs = (0 to n).map(i => a + (b - a) * i / n).toList
    if unresolved(antiderivative) then None
    else for
      fs <- sequence(xs.map(at(f, _)))
      if fs.exists((_, im) => im != 0.0)
      gs <- sequence(xs.map(at(antiderivative, _)))
      step = fs.map((p, q) => math.hypot(p, q)).max * math.abs(b - a) / n
      if gs.sliding(2).forall { case List(p, q) => dist(p, q) <= 2 * step + 1e-12; case _ => true }
      h  = math.abs(b - a) * 1e-6
      if (1 to 9 by 2).forall { i =>
        val x = xs(i * n / 10)
        (at(antiderivative, x + h), at(antiderivative, x - h)) match
          case (Some(p), Some(q)) =>
            val slope = ((p._1 - q._1) / (2 * h), (p._2 - q._2) / (2 * h))
            val fx    = fs(i * n / 10)
            dist(slope, fx) <= 1e-5 * math.max(1.0, math.hypot(fx._1, fx._2))
          case _ => false
      }
    yield _Complex.of(gs.last._1 - gs.head._1, gs.last._2 - gs.head._2)


  /** `F(hi) − F(lo)` for limits that do not both fold to numbers (F_0273): valid when `F` is
   *  differentiable on the **whole** real line, so no value the limits can take crosses a pole.
   *  That is what makes an iterated integral exact — `integral(integral(1, y, 0, x), x, 0, 1)`: the
   *  inner one is `x`, not an unevaluated node the outer can only sample.  Otherwise the node waits. */
  private def symbolicAntiderivative(lo: _Expression, hi: _Expression, env: Environment): Either[_Expression, _Value] =
    val exact = _Rational.containsExactIn(e, env)
    val f     = integrateInTier(integrand(env), v, exact)
    def unresolved(x: _Expression): Boolean = x match
      case _: _Integral | _: _Derivative => true
      case other                         => other.children.exists(unresolved)
    val wholeLine = differentiableDomainOf(f, v, DomainKind.Real, env).intervals.contains(Vector(Interval.All))
    if unresolved(f) || !wholeLine then Left(this)
    else
      val at = (x: _Expression) => substitute(f, Map(v.variable -> x))
      Sum(at(hi), Product(_Rational.literalLike(-1, e, env), at(lo))).eval(env)

  /** `F(b) − F(a)` when `integrate` closes and the fundamental theorem holds on the interval (F_0281):
   *  `F` differentiable on the open interval — `differentiableDomainOf(F)` covers it, so `F′ = f`
   *  throughout — and continuous at each endpoint, read off the same domain or, at a boundary, as the
   *  one-sided limit of `F` agreeing with `F`'s value.  Exact on exact input; `None` otherwise. */
  private def byAntiderivative(lo: _Value, hi: _Value, a: Double, b: Double, env: Environment): Option[_Value] =
    val exact = _Rational.containsExactIn(e, env) || _Rational.containsExact(lo) || _Rational.containsExact(hi)
    val f     = integrateInTier(integrand(env), v, exact)
    def unresolved(x: _Expression): Boolean = x match
      case _: _Integral | _: _Derivative => true
      case other                         => other.children.exists(unresolved)
    val (l, r) = (math.min(a, b), math.max(a, b))
    if unresolved(f) then None
    else differentiableDomainOf(f, v, DomainKind.Real, env).intervals
      .flatMap(_.find(i => i.lo <= l && i.hi >= r))
      .flatMap { i =>
        def at(x: _Value): Option[_Value] = f.eval(env.withBinding(v.variable, x)).toOption.filter(isReal)
        // F at an endpoint: its value where F is continuous there, inside the domain; else its one-sided
        // limit from inside the interval — an infinite endpoint, or one F is undefined at (F_0288: the
        // improper integral is lim F(b) − lim F(a); ∫₀¹ ln x is [x ln x − x] with x ln x → 0 at 0).
        def end(x: Double, xv: _Value, side: LimitDir): Option[_Value] =
          val inside = x.isFinite && ((x > i.lo && x < i.hi) || i.contains(x))
          Option.when(inside)(at(xv)).flatten.orElse(
            evalLimit(f, v, xv, side, env).eval(env).toOption.collect { case w @ _Number(d) if !d.isNaN => w })
        for
          fa <- end(a, lo, if a < b then LimitDir.FromRight else LimitDir.FromLeft)
          fb <- end(b, hi, if a < b then LimitDir.FromLeft else LimitDir.FromRight)
          d  <- difference(fb, fa, env)
        yield d
      }

  /** `fb − fa` over the extended reals: an infinity stands (the integral diverges to it, F_0288), and
   *  `∞ − ∞` has no value. */
  private def difference(fb: _Value, fa: _Value, env: Environment): Option[_Value] = (fb, fa) match
    case (_Number(x), _Number(y)) if x.isInfinite && y.isInfinite => Option.when(x != y)(_Number(x))
    case (_Number(x), _) if x.isInfinite                         => Some(_Number(x))
    case (_, _Number(y)) if y.isInfinite                         => Some(_Number(-y))
    case _ => Sum(fb, Product(_Rational.literalLike(-1, fb), fa)).eval(env).toOption.filter(isReal)

  /** The integrand as it evaluates with `v` free (F_0273): an inner integral with a symbolic limit
   *  becomes its antiderivative's difference, which `integrate` can read where the unevaluated node
   *  was opaque.  Left as written when `v` is bound, since evaluating would substitute it. */
  private def integrand(env: Environment): _Expression =
    if env.isBound(v.variable) then e else e.eval(env).toExpression

  /** A finite real value. */
  private def isReal(x: _Value): Boolean = x match
    case _Number(d) => d.isFinite
    case _          => false

  /** Whether `[a, b]` lies inside one interval of the integrand's real domain, when that resolves
   *  (F_0281): a pole or a gap inside declines.  An unresolved domain (`tan`) passes, and the
   *  certificate of [[adaptiveSimpson]] is what refuses a pole there. */
  private def insideDomain(a: Double, b: Double, env: Environment): Boolean =
    val (l, r) = (math.min(a, b), math.max(a, b))
    domainOf(e, v, DomainKind.Real, env).intervals.forall(_.exists(i => i.contains(l) && i.contains(r)))

  /** Adaptive Simpson (F_0281), certified: each panel splits until its two halves agree with it within
   *  its share of the tolerance, `0.05·10⁻ᵖ` of the integral of `|f|` estimated on 32 panels — relative
   *  to the integrand's own size, so no unit enters.  A non-finite sample, a panel narrower than the
   *  rounding of its midpoint, or more than `Limit.MaxQuadratureEvaluations` samples declines.  It
   *  replaced Simpson's rule at a fixed `precision·200` steps, which answered `∫₁¹⁰⁰⁰⁰ dx/x²` as
   *  `3.49848` and `∫ sec²` across a pole as `2.2·10³⁰`. */
  private def adaptiveSimpson(a: Double, b: Double, env: Environment): Option[Double] =
    val budget = env.limits(Limit.MaxQuadratureEvaluations)
    val compiled = compile(e, v, env)
    val f: Double => Option[Double] = compiled match
      case Some(g) => x => Some(g(x)).filter(_.isFinite)
      case None    => x => e.eval(env.withBinding(v.variable, _Number(x))) match
        case Right(_Number(y)) if y.isFinite => Some(y)
        case _                                => None
    // A tree-evaluated sample costs what its nodes cost, a `sum` included (F_0281, measured).
    val cost = if compiled.isDefined then 1L else QuadratureWork.TreeSampleCost
    def sample(x: Double): Option[Double] =
      if QuadratureWork.spend(cost, budget) then f(x) else None
    def simpson(fa: Double, fm: Double, fb: Double, h: Double): Double = h / 6 * (fa + 4 * fm + fb)
    // One panel `[x0, x1]`, refined until certified; `None` declines the whole integral.
    def panel(x0: Double, x1: Double, f0: Double, fm: Double, f1: Double, whole: Double, tol: Double): Option[Double] =
      val m = (x0 + x1) / 2
      if !(m > x0 && m < x1) then None
      else
        for
          fl <- sample((x0 + m) / 2)
          fr <- sample((m + x1) / 2)
          left  = simpson(f0, fl, fm, m - x0)
          right = simpson(fm, fr, f1, x1 - m)
          s <- if math.abs(left + right - whole) <= 15 * tol then Some(left + right + (left + right - whole) / 15)
               else for l <- panel(x0, m, f0, fl, fm, left, tol / 2); r <- panel(m, x1, fm, fr, f1, right, tol / 2) yield l + r
        yield s
    // The accuracy asked of the panels, relative to ∫|f|: five digits beyond the display precision,
    // since a coefficient built on an integral (`fourierSeries`) needs more than it shows; floored
    // where a `Double` stops resolving a panel's agreement.
    val relTol = math.max(math.pow(10, -(env.precision + 5)), 1e-13)
    // One adaptive run from `panels` equal panels: the integral and the integral of |f| it was
    // certified against.
    def run(panels: Int): Option[(Double, Double)] =
      val h  = (b - a) / panels
      val xs = Vector.tabulate(2 * panels + 1)(k => if k == 2 * panels then b else a + k * h / 2)
      xs.foldLeft(Option(Vector.empty[Double]))((acc, x) => acc.flatMap(ys => sample(x).map(ys :+ _))).flatMap { ys =>
        val coarse = (0 until panels).map(p => simpson(ys(2 * p), ys(2 * p + 1), ys(2 * p + 2), h))
        val scale  = (0 until panels).map(p => simpson(ys(2 * p).abs, ys(2 * p + 1).abs, ys(2 * p + 2).abs, h)).sum
        val tol    = relTol * scale / panels
        (0 until panels).foldLeft(Option(0.0)) { (acc, p) =>
          acc.flatMap(s => panel(xs(2 * p), xs(2 * p + 2), ys(2 * p), ys(2 * p + 1), ys(2 * p + 2), coarse(p), tol).map(s + _))
        }.map((_, scale))
      }
    // Two runs from incommensurate grids must agree: a local estimate is blind to an oscillation every
    // panel aliases the same way, and `abs(sin(1000x))` over [0, 100] came back 59.46 for 63.66,
    // certified.  Aliasing tied to one grid does not survive the other.
    for
      (i1, s1) <- run(32)
      (i2, s2) <- run(43)
      if math.abs(i1 - i2) <= 10 * relTol * math.max(s1, s2)
    yield i2


/** The work a definite integral spends, shared by every integral nested inside it (F_0281).
 *
 *  Each integral's own budget would multiply through a nest — an outer adaptive Simpson sampling an
 *  inner one, `2·10⁵ × 2·10⁵` evaluations, a frozen tab — so the count is one per outermost
 *  evaluation, kept per thread (the `Parser` depth guard's mechanism: Scala.js has one thread, the JVM
 *  one per caller) and reset when the outermost integral starts.  `(spent, depth)`. */
private object QuadratureWork:
  private val state = new ThreadLocal[Array[Long]]:
    override def initialValue(): Array[Long] = Array(0L, 0L)

  /** What one integral's setup is charged, in evaluations: its `integrate` attempt and domain
   *  analysis cost about this many compiled samples (measured below, F_0281). */
  val SetupCost: Long = 200L

  /** What one tree-evaluated sample is charged, in compiled samples: the integrand's nodes are
   *  walked per sample, and one holding a `sum` costs ~110 µs on Node against ~0.2 µs compiled.
   *  At 1 such an integrand declined in 22 s on Node (2.1 s JVM); at 40 it gets 5·10³ samples, five
   *  times the 1000 of the fixed grid it replaced (measured in `Limit.MaxQuadratureEvaluations`). */
  val TreeSampleCost: Long = 40L

  /** Runs `body` as one integral, nested in any that is running. */
  def within[A](body: => A): A =
    val s = state.get
    if s(1) == 0 then s(0) = 0
    s(1) += 1
    try body finally s(1) -= 1

  /** Spends `n` evaluations; `false` once the outermost integral's `budget` is exceeded. */
  def spend(n: Long, budget: Long): Boolean =
    val s = state.get
    s(0) += n
    s(0) <= budget


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
    // A sum over the limit variable in closed form first (F_0291): sampled, `sum(f, k, 1, n)` at a
    // large or fractional `n` is refused, while its closed form is a rational function of `n`.
    val body   = closeSums(e, v)
    val result = evalLimit(body, v, point, dir, env)
    if result == this || (body != e && result == _Limit(body, v, point, dir)) then Left(this)
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
    detailed(env).fold(_ => Left(this), _.eval(env))

  /** The series, or why there is none — what the REPL's note reads (F_0077, F_0052 Decision A).
   *  @param env the evaluation environment
   *  @return the unevaluated series, or the reason it was declined
   */
  private[leonardo] def detailed(env: Environment): Either[String, _Expression] =
    n.eval(env) match
      case Right(_Number(d)) if d.isWhole && d >= 0 && d <= env.limits(Limit.MaxTaylorOrder) =>
        // point is NOT required to reduce: expanding about a symbolic centre is
        // meaningful, so it is passed through as it stands.
        taylorDetailedIn(e, v, point, d.toInt, env.limits,
                         _Rational.containsExactIn(e, env) || _Rational.containsExactIn(point, env))
      case _ => Left(orderRangeTo(env.limits(Limit.MaxTaylorOrder)))

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
      case (Right(_Number(t)), Right(_Number(d))) if d.isWhole && d >= 0 && d <= env.limits(Limit.MaxFourierOrder) =>
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
    detailed(env).fold(_ => Left(this), _.eval(env))

  /** The approximant, or why there is none — what the REPL's note reads (F_0077).
   *  @param env the evaluation environment
   *  @return the unevaluated approximant, or the reason it was declined
   */
  private[leonardo] def detailed(env: Environment): Either[String, _Expression] =
    (m.eval(env), n.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) if a.isWhole && b.isWhole =>
        padeDetailed(e, v, a.toInt, b.toInt, env)
      case _ => Left(s"the degrees must be whole numbers summing to at most ${env.limits(Limit.MaxTaylorOrder)}")

/** AST node for `laurent(e, v, point, m, n)`.
 *
 *  Expands `e` about an isolated singularity as `Σ(k = −m to n) c_k·(v − point)ᵏ`.
 *
 *  Follows [[_Taylor]]'s convention that `v` is the expansion variable rather than a binder
 *  — free in the result, but excluded from `children` so `substitute` cannot rewrite the
 *  variable the expansion is taken in.
 *
 *  **`m` is optional.**  When absent the pole order is detected with `singularitiesOf`
 *  whose `Pole(order)` *is* the length of the principal part.  A `Removable`
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
    detailed(env).fold(_ => Left(this), _.eval(env))

  /** The series, or why there is none — what the REPL's note reads (F_0077).
   *  @param env the evaluation environment
   *  @return the unevaluated series, or the reason it was declined
   */
  private[leonardo] def detailed(env: Environment): Either[String, _Expression] =
    val order = m match
      case Some(me) => me.eval(env) match
        case Right(_Number(d)) if d.isWhole && d >= 0 => Right(d.toInt)
        case _                                        => Left("the pole order must be a whole number")
      case None => detectedOrder(env).toRight(s"the pole order at $point could not be determined")
    order.flatMap { mm =>
      n.eval(env) match
        case Right(_Number(nn)) if nn.isWhole && nn >= 0 => laurentDetailed(e, v, point, mm, nn.toInt, env)
        case _                                           => Left(orderRangeTo(env.limits(Limit.MaxTaylorOrder)))
    }

  /** The pole order at `point`, from the singularity classifier. */
  private def detectedOrder(env: Environment): Option[Int] =
    point.eval(env) match
      case Right(_Number(a)) if a.isFinite =>
        singularitiesOf(e, v, env).flatMap { ss =>
          // The nearest singularity, within 1e-6 of the singularities' own scale (F_0125): an absolute
          // 1e-6 matched the regular point 5e-7 to the pole at 1e-7, read the order as 1, and the
          // expansion of 1/(x - 1e-7) about 5e-7 declined.
          val scale = (ss.map(s => math.abs(s.at)) :+ math.abs(a)).max
          ss.minByOption(s => math.abs(s.at - a)).filter(s => math.abs(s.at - a) <= 1e-6 * scale).map(_.kind) match
            case Some(SingularityKind.Pole(o)) => Some(o)
            case Some(SingularityKind.Removable) => Some(0)
            // Not a singularity at all: an ordinary Taylor expansion is the right answer.
            case None => Some(0)
        }
      case _ => None
