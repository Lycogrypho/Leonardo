package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix


/** Renders a parameter column `[[a], [b]]`, `minimize`'s variable form. */
private def nameColumn(vars: Vector[_Variable]): String = vars.map(v => s"[${v.variable}]").mkString("[", ", ", "]")

/** The optional tail shared by both `lsqfit` forms: bounds, then `cov`. */
private def lsqTail(bounds: Option[(_Expression, _Expression)], cov: Boolean): String =
  bounds.fold("")((lo, hi) => s", $lo, $hi") + (if cov then ", cov" else "")

/** Reads `p0` and the bounds, `-inf`/`inf` for none and a number broadcasting (the `qp` rule). */
private def startAndBounds(p0: _Expression, bounds: Option[(_Expression, _Expression)], n: Int,
                           env: Environment): Either[LsqFailure, (Vector[Double], Option[(Vector[Double], Vector[Double])])] =
  val read = for
    start <- columnOf(p0, n, env)
    box   <- bounds match
               case None           => Some(None)
               case Some((lo, hi)) => for l <- boundsOf(lo, n, env); u <- boundsOf(hi, n, env) yield Some((l, u))
  yield (start, box)
  read.toRight(LsqFailure.InvalidInput(s"the starting point must be a numeric column of $n entries, " +
                                       s"and each bound a number or such a column"))

/** The answer at the prompt (Decision C): the parameter column, or with `cov` the row
 *  `[[p, C]]` — `false` in place of `C` when there is none, the "provably none" convention. */
private def fitAnswer(fit: Either[LsqFailure, LsqResult], cov: Boolean,
                      self: _Expression): Either[_Expression, _Value] =
  fit match
    case Left(_)  => Left(self)
    case Right(r) =>
      val p = _MatrixValue(r.p.size, 1, r.p.toArray)
      if !cov then Right(p)
      else Left(_Matrix(1, 2, Vector(p, r.covariance.getOrElse(_Bool(false)))))


/** `lsqfit(r, [[a], [b]], p0[, lb, ub][, cov])` — nonlinear least squares over a residual column
 *  (F_0063).  Evaluates to the fitted parameters as an `n×1` column, or with the slot word `cov`
 *  (not reserved, the `svd(A, full)` precedent) to the row `[[p, C]]`; stays symbolic whenever
 *  [[lsqfit]] declines, and the REPL appends why.
 *
 *  The parameters are binders, excluded from `children`.  A residual over data is written cell
 *  by cell — `transpose(tabulate(at(Y, k, 1) - a*exp(b*at(T, k, 1)), k, 1, N))` — since
 *  `Y - a*exp(b*T)` with `T` a column stays an unevaluated product; the curve-fit form
 *  [[_CurveFit]] builds that residual itself.
 *
 *  @param r      the residual column
 *  @param params the parameters, in the order of `p0`
 *  @param p0     the starting point
 *  @param bounds the lower and upper bounds
 *  @param cov    whether to answer the covariance too
 */
case class _LsqFit(r: _Expression, params: Vector[_Variable], p0: _Expression,
                   bounds: Option[(_Expression, _Expression)], cov: Boolean) extends _Expression:
  override def toString: String = s"lsqfit($r, ${nameColumn(params)}, $p0${lsqTail(bounds, cov)})"
  override def children: List[_Expression] = r :: p0 :: bounds.toList.flatMap((lo, hi) => List(lo, hi))
  override def rebuild(c: List[_Expression]): _Expression =
    _LsqFit(c.head, params, c(1), bounds.map(_ => (c(2), c(3))), cov)

  override def eval(env: Environment): Either[_Expression, _Value] = fitAnswer(detailed(env), cov, this)

  /** The solver's full answer, or why there is none; what the REPL queries. */
  def detailed(env: Environment): Either[LsqFailure, LsqResult] =
    startAndBounds(p0, bounds, params.size, env).flatMap((s, b) => lsqfit(r, params, s, b, env))


/** `lsqfit(model, t, T, Y, [[a], [b]], p0[, lb, ub][, cov])` — the curve-fit form (F_0063
 *  Decision D): fits `model` in `t` to the data columns `T` and `Y` (scipy's `curve_fit` shape).
 *  Told apart from [[_LsqFit]] by its second argument, one bare name rather than a column.
 *
 *  `t` and the parameters are binders, excluded from `children`.
 *
 *  @param model  the model, an expression in `t` and the parameters
 *  @param t      the independent variable
 *  @param tdata  the values of `t`, a row or column
 *  @param ydata  the observations, a row or column of the same length
 *  @param params the parameters, in the order of `p0`
 *  @param p0     the starting point
 *  @param bounds the lower and upper bounds
 *  @param cov    whether to answer the covariance too
 */
case class _CurveFit(model: _Expression, t: _Variable, tdata: _Expression, ydata: _Expression,
                     params: Vector[_Variable], p0: _Expression,
                     bounds: Option[(_Expression, _Expression)], cov: Boolean) extends _Expression:
  override def toString: String =
    s"lsqfit($model, ${t.variable}, $tdata, $ydata, ${nameColumn(params)}, $p0${lsqTail(bounds, cov)})"
  override def children: List[_Expression] =
    model :: tdata :: ydata :: p0 :: bounds.toList.flatMap((lo, hi) => List(lo, hi))
  override def rebuild(c: List[_Expression]): _Expression =
    _CurveFit(c.head, t, c(1), c(2), params, c(3), bounds.map(_ => (c(4), c(5))), cov)

  override def eval(env: Environment): Either[_Expression, _Value] = fitAnswer(detailed(env), cov, this)

  /** The solver's full answer, or why there is none; what the REPL queries. */
  def detailed(env: Environment): Either[LsqFailure, LsqResult] =
    def data(e: _Expression) = numericMatrix(e, env).filter(d => d.rows == 1 || d.cols == 1).map(_.toVector)
    (data(tdata), data(ydata)) match
      case (Some(tv), Some(yv)) =>
        startAndBounds(p0, bounds, params.size, env).flatMap((s, b) => curveFit(model, t, tv, yv, params, s, b, env))
      case _ => Left(LsqFailure.InvalidInput("the data must be numeric rows or columns"))
