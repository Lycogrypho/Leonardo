package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix.{_Matrix, MaxMatrixCells, numericalRank, rankTolerance}
import statistics.leastSquares


/** Why an ARX call declined (F_0052 Decision A: the reason travels as data, and the REPL turns
 *  it into a note). */
enum ArxFailure:
  /** An order is not a whole number in range: `na ≥ 0`, `nb ≥ 1`, `nk ≥ 0`. */
  case InvalidOrders(detail: String)
  /** The operands do not have the shapes the model needs: an output that is not a vector, an
   *  input with no side as long as the output, a `b` that is not `nb×m`. */
  case ShapeMismatch(detail: String)
  /** An operand is not numeric — symbolic, or holding a non-finite sample. */
  case NotNumeric(what: String)
  /** The series gives fewer equations than there are coefficients to fit.
   *  @param samples      the number of output samples
   *  @param equations    the regressor rows they give, `N − max(na, nk + nb − 1)`
   *  @param coefficients the columns, `na + m·nb` */
  case TooShort(samples: Int, equations: Int, coefficients: Int)
  /** The data does not excite the model: the regressor matrix has numerical rank below its
   *  column count, so a least-squares fit would be an artefact of rounding, not of the plant.
   *  @param rank         the numerical rank at the session precision
   *  @param coefficients the column count */
  case NotExcited(rank: Int, coefficients: Int)
  /** The regressor matrix is past the size the library decomposes ([[matrix.MaxMatrixCells]],
   *  or the SVD's own work cap). */
  case TooLarge(rows: Int, cols: Int)
  /** A static gain (`na = 0`, `nk = 0`, `nb = 1`) has no state for [[arx2ss]] to realise. */
  case NoState

import ArxFailure.*


/** An identified ARX model in the **predictor form** (F_0060 Decision A):
 *  `y[k] = Σᵢ aᵢ·y[k−i] + Σⱼ Σₗ bₗⱼ·uⱼ[k−nk−l+1]`.
 *
 *  **The sign of `a` is stated because it cannot be inferred**: MATLAB's `arx`/`idpoly` write
 *  `A(q) = 1 + a₁q⁻¹ + …`, so their `a` is the negative of this one (`a_MATLAB = −a`). The
 *  predictor sign is the one every recurrence and regression in this library already uses.
 *
 *  @param a the `na` autoregressive coefficients, `a₁` first; empty for a finite impulse response
 *  @param b per input `j`, its `nb` coefficients `b₁ⱼ … b_nb,j`, applied from lag `nk` on
 */
final case class ArxModel(a: Vector[Double], b: Vector[Vector[Double]])


/** The regressor matrix and target of an ARX fit (F_0060).
 *
 *  For `k = k₀ … N−1` with `k₀ = max(na, nk + nb − 1)`, row `k` is
 *  `[y[k−1] … y[k−na], u₁[k−nk] … u₁[k−nk−nb+1], u₂[…] …]` and the target is `y[k]`. The
 *  output is one series, a `1×N` row or an `N×1` column; the inputs are `m×N` (a row per input)
 *  or `N×m`, read by which side matches `N` — `m×N` when both do. It needs only one row; a
 *  short experiment's rows can be stacked with another's before fitting, which is why the
 *  rows-per-coefficient requirement belongs to [[arx]], not here.
 *
 *  @param u  the inputs
 *  @param y  the output
 *  @param na the autoregressive order, `≥ 0`
 *  @param nb the number of coefficients per input, `≥ 1`
 *  @param nk the input delay, `≥ 0`; `nk = 0` is direct feedthrough
 *  @return `(X, Y)`, or the reason they cannot be built
 */
def arxRegressors(u: _MatrixValue, y: _MatrixValue, na: Int, nb: Int, nk: Int): Either[ArxFailure, (_MatrixValue, _MatrixValue)] =
  for
    _  <- checkOrders(na, nb, nk)
    ys <- outputOf(y)
    us <- inputsOf(u, ys.size)
    _  <- Either.cond((ys +: us).forall(_.forall(_.isFinite)), (), NotNumeric("a sample is not a finite number"))
    xy <- layout(us, ys, na, nb, nk)
  yield xy

/** Identifies an ARX model by least squares (F_0060), in the predictor form of [[ArxModel]].
 *
 *  **Declines data that does not excite the model** rather than fitting it: the regressor
 *  matrix must have full numerical rank at the session precision — `svd` and the relative
 *  tolerance `rank` uses (F_0061 Decision B) — before `leastSquares` is asked. QR alone would
 *  return numbers on an unexcited data set, meaningless ones. The fit is QR, never the normal
 *  equations. Several outputs are one call per output (MISO, Decision C).
 *
 *  @param u   the inputs, `m×N` or `N×m`
 *  @param y   the output, `1×N` or `N×1`
 *  @param na  the autoregressive order
 *  @param nb  the coefficients per input
 *  @param nk  the input delay
 *  @param env the environment, whose precision sets the rank tolerance
 *  @return the model, or the reason it was declined
 */
def arx(u: _MatrixValue, y: _MatrixValue, na: Int, nb: Int, nk: Int, env: Environment): Either[ArxFailure, ArxModel] =
  for
    xy   <- arxRegressors(u, y, na, nb, nk)
    // Building the regressors needs one row; fitting them needs one per coefficient.
    _    <- Either.cond(xy._1.rows >= xy._1.cols, (),
              TooShort(xy._1.rows + math.max(na, nk + nb - 1), xy._1.rows, xy._1.cols))
    _    <- excited(xy._1, env)
    beta <- leastSquares(xy._1, xy._2).toRight(NotExcited(xy._1.cols - 1, xy._1.cols))
  yield
    val v = beta.toVector
    val m = (xy._1.cols - na) / nb
    ArxModel(v.take(na), Vector.tabulate(m)(j => v.slice(na + j * nb, na + (j + 1) * nb)))

/** The discrete transfer functions of an ARX model, one per input (F_0060).
 *
 *  `Gⱼ(z) = Bⱼ(z)/A(z)` written in positive powers of `z`: with `n = max(na, nk + nb − 1)`,
 *  `A(z) = zⁿ − Σ aᵢ·zⁿ⁻ⁱ` and `Bⱼ(z) = Σ bₗⱼ·zⁿ⁻ⁿᵏ⁻ˡ⁺¹`, folded to lowest terms by the
 *  same `normaliseTf` [[feedback]] uses — so its poles are what [[poles]] reports. **The delay
 *  is an argument** (Decision B), visible where it acts, so `b` holds exactly what was estimated.
 *
 *  @param a  the autoregressive coefficients, predictor sign; empty for none
 *  @param b  per input, its `nb` coefficients
 *  @param nk the input delay
 *  @param z  the discrete frequency variable to write the result in
 *  @return one rational in `z` per input, or the reason the model is malformed
 */
def arx2tf(a: Vector[Double], b: Vector[Vector[Double]], nk: Int, z: _Variable): Either[ArxFailure, Vector[_Expression]] =
  checkModel(a, b, nk).map { nb =>
    val n   = math.max(a.size, nk + nb - 1)
    val den = Vector.tabulate(n + 1)(p => if p == n then 1.0 else coeff(a, n - p).fold(0.0)(-_))
    b.map { bj =>
      val num = Vector.tabulate(n + 1)(p => coeff(bj, n - nk - p + 1).getOrElse(0.0))
      normaliseTf(Ratio(polyToExpr(num, z), polyToExpr(den, z)), z)
    }
  }

/** A state-space realisation `[A, B, C, D]` of an ARX model (F_0060), discrete-time.
 *
 *  The **observer canonical form** of [[arx2tf]]'s common denominator: with
 *  `n = max(na, nk + nb − 1)` states, `A` carries `a₁ … aₙ` down its first column and ones on
 *  its superdiagonal, `C = [1, 0, …, 0]`, `D` the direct feedthrough (`b₁ⱼ` when `nk = 0`, else
 *  `0`), and `B` the numerator less that feedthrough times the denominator. From rest, its
 *  recursion `x[k+1] = A·x[k] + B·u[k]`, `y[k] = C·x[k] + D·u[k]` reproduces the ARX output
 *  sample for sample — which is the test, rather than a transcribed matrix.
 *
 *  @param a  the autoregressive coefficients, predictor sign; empty for none
 *  @param b  per input, its `nb` coefficients
 *  @param nk the input delay
 *  @return `(A, B, C, D)`, or the reason — [[ArxFailure.NoState]] for a static gain
 */
def arx2ss(a: Vector[Double], b: Vector[Vector[Double]], nk: Int): Either[ArxFailure, (_MatrixValue, _MatrixValue, _MatrixValue, _MatrixValue)] =
  checkModel(a, b, nk).flatMap { nb =>
    val n = math.max(a.size, nk + nb - 1)
    val m = b.size
    if n == 0 then Left(NoState)
    else
      // The numerator of input j as coefficients of z^(n - k), k = 0 .. n.
      def num(j: Int, k: Int): Double = coeff(b(j), k - nk + 1).getOrElse(0.0)
      def ai(i: Int): Double          = coeff(a, i).getOrElse(0.0)    // a_i, 0 past na
      val am = _MatrixValue(n, n, Array.tabulate(n * n) { idx =>
        val (r, c) = (idx / n, idx % n)
        if c == 0 then ai(r + 1) else if c == r + 1 then 1.0 else 0.0
      })
      // The denominator is z^n - sum a_i z^(n-i), so the strictly proper part's numerator is
      // num(k) + a_k * num(0): the feedthrough num(0) taken out.
      val bm = _MatrixValue(n, m, Array.tabulate(n * m)(idx => num(idx % m, idx / m + 1) + ai(idx / m + 1) * num(idx % m, 0)))
      val cm = _MatrixValue(1, n, Array.tabulate(n)(i => if i == 0 then 1.0 else 0.0))
      val dm = _MatrixValue(1, m, Array.tabulate(m)(num(_, 0)))
      Right((am, bm, cm, dm))
  }

/** The 1-based coefficient `xs(i)`, or `None` outside `1 .. xs.size`. */
private def coeff(xs: Vector[Double], i: Int): Option[Double] =
  Option.when(i >= 1 && i <= xs.size)(xs(i - 1))

private def checkOrders(na: Int, nb: Int, nk: Int): Either[ArxFailure, Unit] =
  if na < 0 then Left(InvalidOrders(s"na must be a whole number >= 0, got $na"))
  else if nb < 1 then Left(InvalidOrders(s"nb must be a whole number >= 1, got $nb"))
  else if nk < 0 then Left(InvalidOrders(s"nk must be a whole number >= 0, got $nk"))
  else Right(())

/** A model's shape: every input with the same `nb ≥ 1` coefficients, `nk ≥ 0`, all finite.
 *  @return `nb` */
private def checkModel(a: Vector[Double], b: Vector[Vector[Double]], nk: Int): Either[ArxFailure, Int] =
  val nb = b.headOption.fold(0)(_.size)
  for
    _ <- Either.cond(b.nonEmpty && nb >= 1 && b.forall(_.sizeIs == nb), (),
           ShapeMismatch("b must hold the same nb >= 1 coefficients for every input"))
    _ <- checkOrders(a.size, nb, nk)
    _ <- Either.cond((a +: b).forall(_.forall(_.isFinite)), (), NotNumeric("a coefficient is not a finite number"))
  yield nb

/** The output as one series, from a row or a column. */
private def outputOf(y: _MatrixValue): Either[ArxFailure, Vector[Double]] =
  if y.rows == 1 then Right(Vector.tabulate(y.cols)(y(0, _)))
  else if y.cols == 1 then Right(Vector.tabulate(y.rows)(y(_, 0)))
  else Left(ShapeMismatch(s"y must be one output, a 1xN row or an Nx1 column: it is ${y.rows}x${y.cols}"))

/** The inputs as one series each: rows when a row is as long as the output, else columns. */
private def inputsOf(u: _MatrixValue, n: Int): Either[ArxFailure, Vector[Vector[Double]]] =
  if u.cols == n then Right(Vector.tabulate(u.rows)(i => Vector.tabulate(n)(u(i, _))))
  else if u.rows == n then Right(Vector.tabulate(u.cols)(j => Vector.tabulate(n)(u(_, j))))
  else Left(ShapeMismatch(s"u has no side as long as y's $n samples: it is ${u.rows}x${u.cols}"))

/** The regressor rows, after the length and size checks. */
private def layout(us: Vector[Vector[Double]], ys: Vector[Double], na: Int, nb: Int, nk: Int)
    : Either[ArxFailure, (_MatrixValue, _MatrixValue)] =
  val k0   = math.max(na.toLong, nk.toLong + nb - 1)
  val cols = na.toLong + us.size.toLong * nb
  val rows = ys.size - k0
  if rows < 1 then Left(TooShort(ys.size, 0, math.min(cols, Int.MaxValue.toLong).toInt))
  else if rows * cols > MaxMatrixCells then Left(TooLarge(rows.toInt, cols.toInt))
  else
    val (r, c, start) = (rows.toInt, cols.toInt, k0.toInt)
    val x = _MatrixValue(r, c, Array.tabulate(r * c) { idx =>
      val (k, col) = (start + idx / c, idx % c)
      if col < na then ys(k - 1 - col)
      else
        val q = col - na
        us(q / nb)(k - nk - q % nb)
    })
    Right((x, _MatrixValue(r, 1, ys.drop(start).toArray)))

/** Full numerical column rank at the session precision, or the reason the data does not have it. */
private def excited(x: _MatrixValue, env: Environment): Either[ArxFailure, Unit] =
  x.svd match
    case Some((_, sigma, _)) =>
      val r = numericalRank(sigma, rankTolerance(env))
      Either.cond(r == x.cols, (), NotExcited(r, x.cols))
    case None if _MatrixValue.svdWork(x.rows, x.cols) > _MatrixValue.MaxSvdWork => Left(TooLarge(x.rows, x.cols))
    case None => Left(NotNumeric("the regressor matrix could not be decomposed"))


// --- the nodes ---

/** A matrix operand of an ARX node, evaluated; an exact matrix demotes (the `svd` precedent). */
private def arxMatrix(e: _Expression, what: String, env: Environment): Either[ArxFailure, _MatrixValue] =
  _MatrixValue.ofEvaluated(e.eval(env)).toRight(NotNumeric(s"$what is not a numeric matrix"))

/** A whole-number order of an ARX node, at least `min`. */
private def arxOrder(e: _Expression, name: String, min: Int, env: Environment): Either[ArxFailure, Int] =
  e.eval(env) match
    case Right(_Number(d)) if d == math.floor(d) && d >= min && d <= Int.MaxValue => Right(d.toInt)
    case Right(_Number(d)) => Left(InvalidOrders(s"$name must be a whole number >= $min, got ${_Number.render(d)}"))
    case _                 => Left(NotNumeric(s"$name is not a number"))

/** The `(u, y, na, nb, nk)` operands of `arx` and `arxRegressors`. */
private def arxOperands(u: _Expression, y: _Expression, na: _Expression, nb: _Expression, nk: _Expression,
                        env: Environment): Either[ArxFailure, (_MatrixValue, _MatrixValue, Int, Int, Int)] =
  for
    um <- arxMatrix(u, "u", env)
    ym <- arxMatrix(y, "y", env)
    a  <- arxOrder(na, "na", 0, env)
    b  <- arxOrder(nb, "nb", 1, env)
    k  <- arxOrder(nk, "nk", 0, env)
  yield (um, ym, a, b, k)

/** The `(a, b, nk)` operands of the converters: `a` a vector of coefficients or `0` for none (the
 *  absent-group convention of `qp`/`kkt`), `b` the `nb×m` matrix [[_Arx]] returns — read
 *  strictly, since a `1×2` `b` is one coefficient for two inputs, not two for one. */
private def modelOperands(a: _Expression, b: _Expression, nk: _Expression, env: Environment)
    : Either[ArxFailure, (Vector[Double], Vector[Vector[Double]], Int)] =
  val av = a.eval(env) match
    case Right(_Number(0.0)) => Right(Vector.empty[Double])
    case other => _MatrixValue.ofEvaluated(other) match
      case Some(m) if m.rows == 1 || m.cols == 1 => Right(m.toVector)
      case Some(m) => Left(ShapeMismatch(s"a must be a vector of coefficients, or 0 for none: it is ${m.rows}x${m.cols}"))
      case None    => Left(NotNumeric("a is not a vector of coefficients, or 0 for none"))
  for
    as <- av
    bm <- arxMatrix(b, "b (the nb x m matrix arx returns)", env)
    k  <- arxOrder(nk, "nk", 0, env)
  yield (as, Vector.tabulate(bm.cols)(j => Vector.tabulate(bm.rows)(bm(_, j))), k)


/** `arxRegressors(u, y, na, nb, nk)` — [[arxRegressors]] as a node, the row `[[X, Y]]`.
 *  @param u  the inputs
 *  @param y  the output
 *  @param na the autoregressive order
 *  @param nb the coefficients per input
 *  @param nk the input delay */
case class _ArxRegressors(u: _Expression, y: _Expression, na: _Expression, nb: _Expression, nk: _Expression) extends _Expression:
  override def children: List[_Expression] = List(u, y, na, nb, nk)
  override def rebuild(c: List[_Expression]): _Expression = _ArxRegressors(c(0), c(1), c(2), c(3), c(4))
  override def toString: String = s"arxRegressors($u, $y, $na, $nb, $nk)"

  /** The regressors, or why they were declined.
   *  @param env the evaluation environment
   *  @return `(X, Y)` or the reason */
  def detailed(env: Environment): Either[ArxFailure, (_MatrixValue, _MatrixValue)] =
    arxOperands(u, y, na, nb, nk, env).flatMap((um, ym, a, b, k) => arxRegressors(um, ym, a, b, k))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), (x, t) => Left(_Matrix(1, 2, Vector(x, t))))


/** `arx(u, y, na, nb, nk)` — [[arx]] as a node, the row `[[a, b]]` (F_0060).
 *
 *  `a` is the `na×1` column of autoregressive coefficients in the predictor sign, or `0` when
 *  `na = 0` — a matrix cannot have zero rows, and `0` for an absent group is the `qp`/`kkt`
 *  convention; `b` is `nb×m`, a column per input. Tuple assignment binds both:
 *  `a, b := arx(u, y, 2, 1, 1)`. A refusal leaves the call as written, and [[detailed]]
 *  carries the reason the REPL prints.
 *
 *  @param u  the inputs
 *  @param y  the output
 *  @param na the autoregressive order
 *  @param nb the coefficients per input
 *  @param nk the input delay */
case class _Arx(u: _Expression, y: _Expression, na: _Expression, nb: _Expression, nk: _Expression) extends _Expression:
  override def children: List[_Expression] = List(u, y, na, nb, nk)
  override def rebuild(c: List[_Expression]): _Expression = _Arx(c(0), c(1), c(2), c(3), c(4))
  override def toString: String = s"arx($u, $y, $na, $nb, $nk)"

  /** The identified model, or why it was declined.
   *  @param env the evaluation environment
   *  @return the model or the reason */
  def detailed(env: Environment): Either[ArxFailure, ArxModel] =
    arxOperands(u, y, na, nb, nk, env).flatMap((um, ym, a, b, k) => arx(um, ym, a, b, k, env))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), model => Left(_Matrix(1, 2, Vector(aOf(model), bOf(model)))))

  /** `a` as a column, or `0` when there is none. */
  private def aOf(m: ArxModel): _Expression =
    if m.a.isEmpty then _Number(0) else _MatrixValue(m.a.size, 1, m.a.toArray)

  /** `b` as the `nb×m` matrix, a column per input. */
  private def bOf(m: ArxModel): _Expression =
    val (rows, cols) = (m.b.head.size, m.b.size)
    _MatrixValue(rows, cols, Array.tabulate(rows * cols)(idx => m.b(idx % cols)(idx / cols)))


/** `arx2tf(a, b, nk, z)` — [[arx2tf]] as a node: one rational in `z` for one input, a `1×m` row
 *  of them for several. `z` is excluded from `children` and carried through `rebuild`, the
 *  `_Series` convention.
 *  @param a  the autoregressive coefficients, or `0` for none
 *  @param b  the `nb×m` input coefficients
 *  @param nk the input delay
 *  @param z  the discrete frequency variable */
case class _Arx2Tf(a: _Expression, b: _Expression, nk: _Expression, z: _Variable) extends _Expression:
  override def children: List[_Expression] = List(a, b, nk)
  override def rebuild(c: List[_Expression]): _Expression = _Arx2Tf(c(0), c(1), c(2), z)
  override def toString: String = s"arx2tf($a, $b, $nk, $z)"

  /** The transfer functions, or why they were declined.
   *  @param env the evaluation environment
   *  @return one per input, or the reason */
  def detailed(env: Environment): Either[ArxFailure, Vector[_Expression]] =
    modelOperands(a, b, nk, env).flatMap((as, bs, k) => arx2tf(as, bs, k, z))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env) match
      case Left(_)                  => Left(this)
      case Right(Vector(v: _Value)) => Right(v)
      case Right(Vector(g))         => Left(g)
      case Right(gs)                => Left(_Matrix(1, gs.size, gs))


/** `arx2ss(a, b, nk)` — [[arx2ss]] as a node, the state-space row `[[A, B, C, D]]`.
 *  @param a  the autoregressive coefficients, or `0` for none
 *  @param b  the `nb×m` input coefficients
 *  @param nk the input delay */
case class _Arx2Ss(a: _Expression, b: _Expression, nk: _Expression) extends _Expression:
  override def children: List[_Expression] = List(a, b, nk)
  override def rebuild(c: List[_Expression]): _Expression = _Arx2Ss(c(0), c(1), c(2))
  override def toString: String = s"arx2ss($a, $b, $nk)"

  /** The realisation, or why it was declined.
   *  @param env the evaluation environment
   *  @return `(A, B, C, D)` or the reason */
  def detailed(env: Environment): Either[ArxFailure, (_MatrixValue, _MatrixValue, _MatrixValue, _MatrixValue)] =
    modelOperands(a, b, nk, env).flatMap((as, bs, k) => arx2ss(as, bs, k))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), (am, bm, cm, dm) => Left(stateSpace(am, bm, cm, dm)))
