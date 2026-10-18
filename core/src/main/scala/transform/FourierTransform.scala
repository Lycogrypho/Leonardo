package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** The [[https://en.wikipedia.org/wiki/Fourier_transform Fourier transform]], two-sided and
 *  causal, by way of the Laplace table (F_0141).
 *
 *  `F{x}(ω) = ∫ x(t)·e^(−iωt) dt` over the whole line splits at `t = 0` into the transform of the
 *  causal part, `L{x}(iω)`, and that of the anti-causal part reflected, `L{x(−t)}(−iω)`.
 *  [[fourierOf]] adds the two; [[causalFourierOf]] keeps the first, the transform of `x(t)·u(t)`.
 *
 *  **The substitution `s → ±iω` is valid only where the Laplace integral converges on the
 *  imaginary axis**, so a part is answered only when every pole of its Laplace transform lies
 *  strictly left of the axis, and otherwise the call stays unevaluated.  Until F_0141 that was
 *  neither checked nor stated: `fourier(1, t, w)` answered `1/(iω)` and `fourier(cos(t), t, w)` a
 *  rational where the true transforms carry Dirac deltas, and `fourier(exp(-t), t, w)` answered
 *  for `e^(−t)·u(t)`, a different signal.  No delta node exists, so those now decline.
 *
 *  Three steps make the two-sided transform reachable from a one-sided table:
 *
 *  - **the half-line reading** ([[onHalfLine]]): on `t > 0`, `step(c·t + d)` is `1`, `0` or a right
 *    shift and `abs(c·t)` is `|c|·t`, which is what lets `e^(−|t|)` and the centred pulse
 *    `step(t + 1) − step(t − 1)` reach the table;
 *  - **the region test** ([[regionDecline]]): every pole on or right of the axis must be removable
 *    in the whole transform, which is what lets the pulse's `(1 − e^(−s))/s` through while `1/s`
 *    declines;
 *  - **the answer's shape**: a transform with no exponential factor and numeric coefficients is
 *    returned as one rational in `ω`, real when the signal is even — `e^(−|t|)` answers
 *    `2/(1 + ω²)` — and anything else as the substituted Laplace transforms.
 */

/** The two-sided Fourier transform of `e`, or the unevaluated [[_Fourier]] node when it declines.
 *
 *  @param e the time-domain expression to transform
 *  @param t the time variable
 *  @param w the angular frequency variable
 *  @return the transform, or the call unevaluated
 */
def fourierOf(e: _Expression, t: _Variable, w: _Variable): _Expression =
  transformed(e, t, w, causal = false).getOrElse(_Fourier(e, t, w))

/** The Fourier transform of `e(t)·u(t)`, `L{e}(iω)`, or the unevaluated [[_FourierCausal]] node
 *  when the Laplace transform has a pole on or right of the imaginary axis.
 *
 *  @param e the time-domain expression to transform
 *  @param t the time variable
 *  @param w the angular frequency variable
 *  @return the transform, or the call unevaluated
 */
def causalFourierOf(e: _Expression, t: _Variable, w: _Variable): _Expression =
  transformed(e, t, w, causal = true).getOrElse(_FourierCausal(e, t, w))

/** Why the transform of `e` declines, in prose, or `None` when it answers.
 *
 *  @param e      the time-domain expression
 *  @param t      the time variable
 *  @param w      the angular frequency variable
 *  @param causal whether the causal form was asked for
 *  @return the reason, or `None`
 */
def fourierDecline(e: _Expression, t: _Variable, w: _Variable, causal: Boolean): Option[String] =
  transformed(e, t, w, causal).left.toOption

/** Both forms: each part through the Laplace table and the region test, then combined. */
private def transformed(e: _Expression, t: _Variable, w: _Variable,
                        causal: Boolean): Either[String, _Expression] =
  // One internal Laplace variable, safe against every name in e and both binders (the shared
  // capture-safe generator, scalar.freshVar, does the pigeonhole search).
  val s = freshVar(e.freeVars + t.variable + w.variable, "__lts")

  def half(x: _Expression, part: String): Either[String, (_Expression, List[LaplaceTerm])] =
    val l = laplaceOf(x, t, s)
    if containsLaplace(l) then Left(s"no Laplace transform of its $part part is known")
    else
      laplaceTerms(l, s)
        .toRight(s"the Laplace transform of its $part part is not a rational function of s with numeric poles")
        .flatMap(ts => regionDecline(live(ts), part).toLeft((l, live(ts))))

  val forward = onHalfLine(e, t) match
    case same if same == e => e
    case rewritten         => simplifyFully(rewritten)
  val reflected =
    simplifyFully(onHalfLine(simplifyFully(substitute(e, Map(t.variable -> Product(_Number(-1), t)))), t))

  for
    c <- half(forward, "causal")
    a <- if causal then Right((_Number(0), Nil)) else half(reflected, "anti-causal")
  yield
    val (iw, minusIw) = (Product(_Complex.of(0, 1), w), Product(_Complex.of(0, -1), w))
    rationalInW(c._2, a._2, w).getOrElse {
      val forwardPart = substitute(c._1, Map(s.variable -> iw))
      if a._2.isEmpty then forwardPart else Sum(forwardPart, substitute(a._1, Map(s.variable -> minusIw)))
    }

/** Whether a Laplace node survives anywhere in `e`: the table returns the call for a part it
 *  cannot transform, nested inside a sum or a product. */
private def containsLaplace(e: _Expression): Boolean = e match
  case _: _Laplace => true
  case other       => other.children.exists(containsLaplace)


// ── The half-line reading ─────────────────────────────────────────────────────────────────

/** `e` as it reads on `t > 0`, which is all a Laplace transform sees: `step(c·t + d)` becomes `1`,
 *  `0`, the right shift `step(t − a)` the table knows, or `1 − step(t − a)`; `abs(c·t)` becomes
 *  `|c|·t`.  Anything else is left as written, so the table declines what it declined before. */
private def onHalfLine(e: _Expression, t: _Variable): _Expression =
  val r = if e.children.isEmpty then e else e.rebuild(e.children.map(onHalfLine(_, t)))
  r match
    case _Heaviside(arg) => linearIn(arg, t) match
      case Some((c, d)) if c > 0 => if d >= 0 then _Number(1) else _Heaviside(Sum(t, _Number(d / c)))
      case Some((c, d))          =>
        if d <= 0 then _Number(0)
        else Sum(_Number(1), Product(_Number(-1), _Heaviside(Sum(t, _Number(d / c)))))
      case None                  => r
    case Abs(arg) => linearIn(arg, t) match
      case Some((c, d)) if d == 0.0 => Product(_Number(math.abs(c)), t)
      case _                        => r
    case _ => r

/** `(c, d)` when `e = c·t + d` with numeric `c ≠ 0` and `d`. */
private def linearIn(e: _Expression, t: _Variable): Option[(Double, Double)] =
  collect(e, t).filter(_.size == 2).flatMap { cs =>
    for c <- numericValue(cs(1)) if c != 0.0; d <- numericValue(cs(0)) yield (c, d)
  }

private def numericValue(e: _Expression): Option[Double] = e.eval(new Environment()) match
  case Right(_Number(d)) if d.isFinite => Some(d)
  case _                               => None


// ── A Laplace transform read as terms ─────────────────────────────────────────────────────

/** One term `k·e^(b·s)·N(s)/D(s)` of a Laplace transform: `k` free of `s` (it may hold a free
 *  parameter), `b` numeric (the second-shift theorem's `e^(−as)`), `N` and `D` numeric polynomials. */
private final case class LaplaceTerm(k: _Expression, b: Double, num: Vector[Double], den: Vector[Double])

private val UnitPoly = Vector(1.0)

/** `a·b`, without multiplying by a literal one. */
private def times(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(1.0), _) => b
  case (_, _Number(1.0)) => a
  case _                 => Product(a, b)

private def capped(p: Vector[Double]): Option[Vector[Double]] = Option.when(p.size <= MaxRationalDegree + 1)(p)

/** `e` as a sum of [[LaplaceTerm]]s in `s`, or `None` when it is not of that shape — a pole that
 *  depends on a free parameter, a non-rational factor, an exponential not linear in `s`. */
private def laplaceTerms(e: _Expression, s: _Variable): Option[List[LaplaceTerm]] =
  if !dependsOn(e, s) then Some(List(LaplaceTerm(e, 0.0, UnitPoly, UnitPoly)))
  else e match
    case _: _Variable => Some(List(LaplaceTerm(_Number(1), 0.0, Vector(0.0, 1.0), UnitPoly)))
    case Sum(a, b)    => for x <- laplaceTerms(a, s); y <- laplaceTerms(b, s) yield x ++ y
    case Product(a, b) =>
      for
        x   <- laplaceTerms(a, s)
        y   <- laplaceTerms(b, s)
        out <- sequence(for p <- x; q <- y yield
                 for n <- capped(polyMul(p.num, q.num)); d <- capped(polyMul(p.den, q.den))
                 yield LaplaceTerm(times(p.k, q.k), p.b + q.b, n, d))
      yield out
    case Ratio(a, d) if !dependsOn(d, s) => laplaceTerms(a, s).map(_.map(p => p.copy(k = Ratio(p.k, d))))
    case Ratio(a, d) =>
      for
        (dn, dd) <- rationalCoeffs(d, s)
        if degreeOf(dn) >= 0
        x        <- laplaceTerms(a, s)
        out      <- sequence(x.map(p =>
                      for n <- capped(polyMul(p.num, dd)); m <- capped(polyMul(p.den, dn)) yield p.copy(num = n, den = m)))
      yield out
    case Exp(arg) =>
      collect(arg, s).filter(_.size == 2).flatMap { cs =>
        numericValue(cs(1)).map { b =>
          val k = if numericValue(cs(0)).contains(0.0) then _Number(1) else Exp(cs(0))
          List(LaplaceTerm(k, b, UnitPoly, UnitPoly))
        }
      }
    case _ => rationalCoeffs(e, s).map((n, d) => List(LaplaceTerm(_Number(1), 0.0, n, d)))

/** The terms that are not identically zero. */
private def live(ts: List[LaplaceTerm]): List[LaplaceTerm] =
  ts.filter(p => degreeOf(p.num) >= 0 && !numericValue(p.k).contains(0.0))


// ── The region test ──────────────────────────────────────────────────────────────────────

/** A complex number for the region test's arithmetic, kept private to it. */
private final case class Cx(re: Double, im: Double):
  def +(o: Cx): Cx = Cx(re + o.re, im + o.im)
  def -(o: Cx): Cx = Cx(re - o.re, im - o.im)
  def *(o: Cx): Cx = Cx(re * o.re - im * o.im, re * o.im + im * o.re)
  def /(o: Cx): Cx =
    val d = o.re * o.re + o.im * o.im
    Cx((re * o.re + im * o.im) / d, (im * o.re - re * o.im) / d)
  def abs: Double = math.hypot(re, im)

private val CxZero = Cx(0.0, 0.0)
private val CxOne  = Cx(1.0, 0.0)

/** A pole of one term: where, its multiplicity there, and its denominator's root scale. */
private final case class Pole(at: Cx, multiplicity: Int, scale: Double)

/** A pole counts as on or right of the axis when `Re ≥ −AxisTolerance·ρ`, `ρ` its polynomial's
 *  root scale: the computed real part of a root on the axis is rounding of that size, and a scale
 *  relative to the roots is what lets a pole at `−10⁻¹²` count as left of the axis. */
private val AxisTolerance = 1e-9

/** Two roots of different terms are one pole within this fraction of their scale. */
private val ClusterTolerance = 1e-6

/** A Taylor coefficient of the whole transform vanishes when it is this small against the
 *  terms that formed it: the cancellation of `(1 − e^(−s))/s` at `0` is exact, while a genuine
 *  pole leaves a coefficient of the terms' own size. */
private val RemovableTolerance = 1e-8

/** The poles of a denominator, multiplicities from the square-free factorisation (the QR
 *  iteration can fail outright on a repeated root, the 2.5 rule). */
private def polesOf(den: Vector[Double]): Option[Vector[Pole]] =
  val rho = rootScale(den)
  sequence(squareFreeFactors(den).map { (f, k) =>
    rootsAtScale(f).map(_.flatMap(_Complex.parts).map((re, im) => Pole(Cx(re, im), k, rho)))
  }).map(_.flatten)

private def near(p: Pole, q: Pole): Boolean =
  (p.at - q.at).abs <= ClusterTolerance * math.max(p.scale, q.scale)

/** Why a part's transform does not converge on the imaginary axis, or `None` when it does: every
 *  pole on or right of the axis must be removable in the sum of the terms. */
private def regionDecline(terms: List[LaplaceTerm], part: String): Option[String] =
  sequence(terms.map(p => polesOf(p.den))) match
    case None => Some(s"the poles of its $part part's Laplace transform could not be located")
    case Some(poles) =>
      val suspect = poles.flatten.filter(p => p.at.re >= -AxisTolerance * p.scale)
      val distinct = suspect.foldLeft(Vector.empty[Pole])((acc, p) => if acc.exists(near(_, p)) then acc else acc :+ p)
      distinct.iterator.flatMap { r =>
        val where = _Complex.of(r.at.re, r.at.im)
        analyticAt(terms, poles, r) match
          case Some(true)  => None
          case Some(false) =>
            Some(s"its $part part's Laplace transform has a pole at $where, on or right of the imaginary axis, so the integral does not converge")
          case None =>
            Some(s"its $part part's Laplace transform has a pole at $where with a coefficient that depends on a free parameter")
      }.nextOption()

/** Whether the sum of `terms` is analytic at the candidate `r`: with `m` the largest multiplicity
 *  of `r` among the terms, the Taylor coefficients of `(s − r)^m·F(s)` below order `m` vanish.
 *  `None` when a coefficient `k` is not a number, so the cancellation cannot be decided. */
private def analyticAt(terms: List[LaplaceTerm], poles: List[Vector[Pole]], r: Pole): Option[Boolean] =
  val mus = poles.map(ps => ps.filter(near(_, r)).map(_.multiplicity).maxOption.getOrElse(0))
  val m   = mus.max
  sequence(terms.map(p => numericValue(p.k))).flatMap { ks =>
    sequence(terms.lazyZip(ks).lazyZip(mus).map((p, k, mu) => termSeries(p, k, r.at, m, mu))).map { series =>
      (0 until m).forall { j =>
        val parts = series.map(_(j))
        parts.foldLeft(CxZero)(_ + _).abs <= RemovableTolerance * parts.map(_.abs).sum
      }
    }
  }

/** The first `m` Taylor coefficients at `r` of `(s − r)^m·k·e^(b·s)·N(s)/D(s)`, `D` holding the
 *  factor `(s − r)` `mu` times; `None` when the deflated denominator still vanishes at `r`. */
private def termSeries(p: LaplaceTerm, k: Double, r: Cx, m: Int, mu: Int): Option[Vector[Cx]] =
  val q  = (1 to mu).foldLeft(p.den.map(Cx(_, 0.0)))((acc, _) => deflate(acc, r))
  val nq = shifted(p.num.map(Cx(_, 0.0)), r, m)
  val qq = shifted(q, r, m)
  Option.when(qq(0).abs > 0.0 && !qq(0).abs.isNaN) {
    val ratio = seriesDiv(nq, qq, m)
    val prod  = seriesMul(ratio, expSeries(p.b, r, m), m)
    val lift  = m - mu
    Vector.tabulate(m)(j => if j < lift then CxZero else prod(j - lift) * Cx(k, 0.0))
  }

/** The quotient of `p` by `(x − r)`, by synthetic division; the remainder is dropped. */
private def deflate(p: Vector[Cx], r: Cx): Vector[Cx] =
  if p.sizeIs <= 1 then Vector(CxZero) else p.tail.scanRight(CxZero)((c, acc) => c + r * acc).init

private def horner(p: Vector[Cx], r: Cx): Cx = p.foldRight(CxZero)((c, acc) => acc * r + c)

/** The first `m` coefficients of `p(r + h)` in `h`. */
private def shifted(p: Vector[Cx], r: Cx, m: Int): Vector[Cx] =
  Iterator.iterate(p)(deflate(_, r)).take(m).map(horner(_, r)).toVector

private def seriesMul(a: Vector[Cx], b: Vector[Cx], m: Int): Vector[Cx] =
  Vector.tabulate(m)(j => (0 to j).foldLeft(CxZero)((acc, i) => acc + a(i) * b(j - i)))

private def seriesDiv(a: Vector[Cx], b: Vector[Cx], m: Int): Vector[Cx] =
  (0 until m).foldLeft(Vector.empty[Cx]) { (c, j) =>
    c :+ (1 to j).foldLeft(a(j))((acc, i) => acc - b(i) * c(j - i)) / b(0)
  }

/** The first `m` Taylor coefficients of `e^(b·s)` at `r`: `e^(b·r)·bʲ/j!`. */
private def expSeries(b: Double, r: Cx, m: Int): Vector[Cx] =
  val base = Cx(math.exp(b * r.re) * math.cos(b * r.im), math.exp(b * r.re) * math.sin(b * r.im))
  Vector.iterate(base, m)(c => c * Cx(b, 0.0)).zipWithIndex.map((c, j) => c / Cx((1 to j).foldLeft(1.0)(_ * _), 0.0))


// ── The answer's shape ───────────────────────────────────────────────────────────────────

/** The two halves as one rational in `ω` — `P(iω)` has coefficients `cⱼ·iʲ`, `P(−iω)` the
 *  conjugate pattern — when no term carries an exponential factor and every coefficient is a
 *  number; `None` otherwise.  Imaginary parts that are rounding of their terms are dropped, so an
 *  even signal's transform is real. */
private def rationalInW(forward: List[LaplaceTerm], backward: List[LaplaceTerm], w: _Variable): Option[_Expression] =
  def folded(ts: List[LaplaceTerm]): Option[(Vector[Double], Vector[Double])] =
    if ts.exists(_.b != 0.0) then None
    else sequence(ts.map(p => numericValue(p.k).map(k => (p.num.map(_ * k), p.den)))).flatMap {
      _.foldLeft(Option((Vector(0.0), UnitPoly))) {
        case (Some((an, ad)), (n, d)) =>
          for x <- capped(polyMul(an, d)); y <- capped(polyMul(n, ad)); z <- capped(polyMul(ad, d))
          // Added exactly, never through polyAdd, whose absolute trim would drop a genuine
          // coefficient below 1e-9 (a transform in small units).
          yield (trimExact(Vector.tabulate(math.max(x.size, y.size))(i => x.lift(i).getOrElse(0.0) + y.lift(i).getOrElse(0.0))), z)
        case (None, _) => None
      }
    }
  // Each coefficient travels with the size of the terms that formed it, so a rounding residue is
  // told from a genuine small coefficient by its own terms (F_0119's rule): measured against the
  // largest coefficient instead, the 1e-12 of e^(-1e-6|t|)'s denominator was dropped.
  def onAxis(p: Vector[Double], sign: Double): Vector[(Cx, Double)] =
    p.zipWithIndex.map((c, j) => (Cx(c, 0.0) * Vector(CxOne, Cx(0.0, sign), Cx(-1.0, 0.0), Cx(0.0, -sign))(j % 4), math.abs(c)))
  def mul(a: Vector[(Cx, Double)], b: Vector[(Cx, Double)]): Vector[(Cx, Double)] =
    Vector.tabulate(a.size + b.size - 1)(j =>
      (math.max(0, j - b.size + 1) to math.min(j, a.size - 1)).foldLeft((CxZero, 0.0)) { case ((c, m), i) =>
        (c + a(i)._1 * b(j - i)._1, m + a(i)._2 * b(j - i)._2)
      })
  def add(a: Vector[(Cx, Double)], b: Vector[(Cx, Double)]): Vector[(Cx, Double)] =
    Vector.tabulate(math.max(a.size, b.size)) { j =>
      val ((ca, ma), (cb, mb)) = (a.lift(j).getOrElse((CxZero, 0.0)), b.lift(j).getOrElse((CxZero, 0.0)))
      (ca + cb, ma + mb)
    }

  for
    (fn, fd) <- folded(forward)
    back     <- if backward.isEmpty then Some(None) else folded(backward).map(Some(_))
  yield
    val (cn, cd) = (onAxis(fn, 1.0), onAxis(fd, 1.0))
    val (num, den) = back match
      case None           => (cn, cd)
      case Some((bn, bd)) =>
        val (an, ad) = (onAxis(bn, -1.0), onAxis(bd, -1.0))
        (add(mul(cn, ad), mul(an, cd)), mul(cd, ad))
    val (n, d) = (cleaned(num), cleaned(den))
    if d.sizeIs == 1 && d(0) == CxOne then polyIn(n, w) else Ratio(polyIn(n, w), polyIn(d, w))

/** `cs` with each real or imaginary part at most `RoundingLevel` of the terms that formed it set to
 *  zero, and trailing zero coefficients dropped. */
private def cleaned(cs: Vector[(Cx, Double)]): Vector[Cx] =
  val out = cs.map { (c, m) =>
    def clip(x: Double) = if math.abs(x) <= RoundingLevel * m then 0.0 else x
    Cx(clip(c.re), clip(c.im))
  }
  val top = out.lastIndexWhere(_ != CxZero)
  if top < 0 then Vector(CxZero) else out.take(top + 1)

/** `Σ cⱼ·wʲ` as an expression, a complex coefficient as a `_Complex`. */
private def polyIn(cs: Vector[Cx], w: _Variable): _Expression =
  cs.zipWithIndex.collect {
    case (c, j) if c != CxZero =>
      val coef: _Expression = _Complex.of(c.re, c.im)
      j match
        case 0 => coef
        case _ =>
          val power = if j == 1 then w else Power(w, _Number(j))
          if c == CxOne then power else Product(coef, power)
  }.reduceLeftOption(Sum(_, _)).getOrElse(_Number(0))
