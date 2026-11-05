package it.grypho.scala.leonardo
package control

import core.*
import ArxFailure.*


// ARX model-order selection (F_0301, APAC L_0005).

/** The information criterion an ARX order search ranks by (F_0301), **always named** (the `c2d`
 *  method rule): with `N` scored equations, `d` coefficients and `V` the determinant of the residual
 *  covariance `(1/N)·Σ e·eᵀ` (the mean squared residual for one output),
 *  `AIC = N·ln V + 2d`, `BIC = N·ln V + d·ln N` and `FPE = V·(1 + d/N)/(1 − d/N)` (Ljung, *System
 *  Identification*, §16.4).  Lower is better. */
enum ArxCriterion:
  case AIC, BIC, FPE

/** One candidate's score (F_0301).
 *  @param na    the autoregressive order
 *  @param nb    the coefficients per input
 *  @param nk    the input delay
 *  @param score the criterion; `-∞` for a residual that vanishes exactly */
final case class ArxOrderScore(na: Int, nb: Int, nk: Int, score: Double)

/** The outcome of an ARX order search (F_0301).
 *  @param na      the chosen autoregressive order
 *  @param nb      the chosen coefficients per input
 *  @param nk      the chosen delay
 *  @param model   the model fitted at the chosen orders
 *  @param scores  every scored candidate, in grid order
 *  @param skipped every candidate that was not scored, with the reason its fit declined */
final case class ArxOrderSelection(na: Int, nb: Int, nk: Int, model: ArxMimoModel,
                                   scores: Vector[ArxOrderScore], skipped: Vector[(Int, Int, Int, ArxFailure)])

/** Selects ARX orders over the grid `nas × nbs × nks` by an information criterion (F_0301).
 *
 *  **Every candidate is fitted and scored on the same equations**, `k = k₀ … N − 1` with `k₀` the
 *  largest `max(na, nk + nb − 1)` on the grid — a criterion compares losses, and losses over
 *  different samples do not compare.  Each fit is [[arxMimo]] with its rank guard, so a candidate
 *  the data does not excite is skipped and reported, never scored.  With `validation`, `V` is the
 *  one-step prediction loss on the validation data over the same `k₀` (the estimate's loss favours
 *  the largest model); the penalty is the criterion's either way.  The lowest score wins, a tie going
 *  to fewer coefficients and then to the earlier candidate, so on noise-free data, where every
 *  model containing the true one fits exactly (`V = 0`, score `−∞`), the true orders are chosen.
 *
 *  The grid is held to [[core.Limit.MaxArxOrderWork]], the sum of the candidates' `r·c²` for their
 *  `r×c` regressor matrices, checked before any fit.
 *
 *  @param u          the inputs, `m×N` or `N×m`
 *  @param y          the outputs, `p×N` or `N×p`
 *  @param nas        the autoregressive orders to try
 *  @param nbs        the coefficient counts per input to try
 *  @param nks        the delays to try
 *  @param criterion  the criterion, named
 *  @param validation validation inputs and outputs in the same layout, or `None`
 *  @param env        the precision of the rank guard, and the budgets
 *  @return the selection, or the reason none was made
 */
def arxOrderSelection(u: _MatrixValue, y: _MatrixValue, nas: Seq[Int], nbs: Seq[Int], nks: Seq[Int],
                      criterion: ArxCriterion, validation: Option[(_MatrixValue, _MatrixValue)],
                      env: Environment): Either[ArxFailure, ArxOrderSelection] =
  val grid = (for na <- nas.distinct; nb <- nbs.distinct; nk <- nks.distinct yield (na, nb, nk)).toVector
  for
    _      <- Either.cond(grid.nonEmpty, (), InvalidOrders("the order grid is empty"))
    _      <- grid.map(checkGridOrders).collectFirst { case Left(e) => e }.toLeft(())
    est    <- seriesOf(u, y)
    valid  <- validation.fold[Either[ArxFailure, Option[(Vector[Vector[Double]], Vector[Vector[Double]])]]](Right(None))(
                (vu, vy) => seriesOf(vu, vy).flatMap(s =>
                  Either.cond(s._1.size == est._1.size && s._2.size == est._2.size, Some(s),
                              ShapeMismatch("the validation data must have as many inputs and outputs as the estimation data"))))
    k0      = grid.map((na, nb, nk) => math.max(na, nk + nb - 1)).max
    (us, ys) = est
    n       = ys.head.size
    rows    = n - k0
    _      <- Either.cond(rows >= 1, (), TooShort(n, 0, 0))
    _      <- valid.fold[Either[ArxFailure, Unit]](Right(()))(v => Either.cond(v._2.head.size - k0 >= 1, (), TooShort(v._2.head.size, 0, 0)))
    work    = grid.map { (na, nb, _) =>
                val cols = ys.size.toLong * na + us.size.toLong * nb
                _MatrixValue.svdWork(rows, cols.toInt)
              }.sum
    _      <- Either.cond(work <= env.limits(Limit.MaxArxOrderWork), (), TooLarge(rows, grid.size))
    fits    = grid.map { (na, nb, nk) =>
                val s = k0 - math.max(na, nk + nb - 1)
                (na, nb, nk, arxMimo(rowsOf(us.map(_.drop(s))), rowsOf(ys.map(_.drop(s))), na, nb, nk, env))
              }
    scored  = fits.collect { case (na, nb, nk, Right(model)) =>
                val (vu, vy) = valid.getOrElse(est)
                val errs = predictionErrors(model, nk, vu, vy, k0)
                val d = ys.size * (ys.size * na + us.size * nb)
                val sizes = vy.map(s => math.sqrt(s.drop(k0).map(x => x * x).sum / (s.size - k0)))
                (ArxOrderScore(na, nb, nk, criterionOf(criterion, errs, sizes, d)), model, d)
              }
    best   <- scored.zipWithIndex.minByOption { case ((s, _, d), i) => (s.score, d, i) }(
                using Ordering.Tuple3(using Ordering.Double.TotalOrdering, Ordering.Int, Ordering.Int))
                .toRight(fits.collectFirst { case (_, _, _, Left(e)) => e }.getOrElse(InvalidOrders("no candidate")))
  yield
    val ((choice, model, _), _) = best
    ArxOrderSelection(choice.na, choice.nb, choice.nk, model, scored.map(_._1),
                      fits.collect { case (na, nb, nk, Left(e)) => (na, nb, nk, e) })

/** An order triple in range, with the reason otherwise. */
private def checkGridOrders(o: (Int, Int, Int)): Either[ArxFailure, Unit] =
  val (na, nb, nk) = o
  if na < 0 || nb < 1 || nk < 0 then Left(InvalidOrders(s"($na, $nb, $nk): na >= 0, nb >= 1 and nk >= 0 are needed"))
  else Right(())

/** The inputs and outputs as one series each, in [[arxRegressors]]' layout. */
private def seriesOf(u: _MatrixValue, y: _MatrixValue): Either[ArxFailure, (Vector[Vector[Double]], Vector[Vector[Double]])] =
  val ys = if y.cols >= y.rows then Vector.tabulate(y.rows)(i => Vector.tabulate(y.cols)(y(i, _)))
           else Vector.tabulate(y.cols)(j => Vector.tabulate(y.rows)(y(_, j)))
  val n = ys.head.size
  val us =
    if u.cols == n then Some(Vector.tabulate(u.rows)(i => Vector.tabulate(n)(u(i, _))))
    else if u.rows == n then Some(Vector.tabulate(u.cols)(j => Vector.tabulate(n)(u(_, j))))
    else None
  us.toRight(ShapeMismatch(s"u has no side as long as y's $n samples: it is ${u.rows}x${u.cols}"))
    .flatMap(us => Either.cond((us ++ ys).forall(_.forall(_.isFinite)), (us, ys), NotNumeric("a sample is not a finite number")))

/** Series laid out a row each. */
private def rowsOf(series: Vector[Vector[Double]]): _MatrixValue =
  val n = series.head.size
  _MatrixValue(series.size, n, Array.tabulate(series.size * n)(idx => series(idx / n)(idx % n)))

/** The one-step prediction errors of `model` on the series, `k = from … N − 1`, one vector per `k`. */
private def predictionErrors(model: ArxMimoModel, nk: Int, us: Vector[Vector[Double]], ys: Vector[Vector[Double]],
                             from: Int): Vector[Vector[Double]] =
  val p = ys.size
  Vector.tabulate(ys.head.size - from) { idx =>
    val k = from + idx
    Vector.tabulate(p) { r =>
      val ar = model.a.indices.map(i => (0 until p).map(c => model.a(i)(r, c) * ys(c)(k - 1 - i)).sum).sum
      val br = model.b.indices.map(l => us.indices.map(j => model.b(l)(r, j) * us(j)(k - nk - l)).sum).sum
      ys(r)(k) - ar - br
    }
  }

/** A residual below this fraction of its output's root mean square is rounding, an exact fit. */
private val ExactFitLevel = 1e-10

/** The criterion on prediction errors `errs` with `d` coefficients; `sizes` are the outputs' root
 *  mean squares, against which an exact fit is recognised (the units rule: rounding leaves a
 *  residual near `10⁻¹⁶` of the output, and read as a loss it would rank exact fits by noise). */
private def criterionOf(criterion: ArxCriterion, errs: Vector[Vector[Double]], sizes: Vector[Double], d: Int): Double =
  val n = errs.size
  val p = errs.head.size
  val exact = (0 until p).forall(r => math.sqrt(errs.map(e => e(r) * e(r)).sum / n) <= ExactFitLevel * sizes(r))
  val cov = _MatrixValue(p, p, Array.tabulate(p * p)(idx => errs.map(e => e(idx / p) * e(idx % p)).sum / n))
  val v = if exact then 0.0 else if p == 1 then cov(0, 0) else cov.determinant.getOrElse(0.0)
  criterion match
    case ArxCriterion.AIC => if v <= 0 then Double.NegativeInfinity else n * math.log(v) + 2.0 * d
    case ArxCriterion.BIC => if v <= 0 then Double.NegativeInfinity else n * math.log(v) + d * math.log(n.toDouble)
    case ArxCriterion.FPE => if d >= n then Double.PositiveInfinity else math.max(v, 0.0) * (1.0 + d.toDouble / n) / (1.0 - d.toDouble / n)
