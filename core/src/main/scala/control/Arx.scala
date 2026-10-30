package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix.{_Matrix, numericalRank, rankTolerance}
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


/** An identified **MIMO** ARX model (F_0093), in the predictor form
 *  `y[k] = Σᵢ Aᵢ·y[k−i] + Σₗ Bₗ·u[k−nk−l+1]`, the `Aᵢ` coupling the outputs.
 *
 *  A record of its own rather than a widened [[ArxModel]], which is published API. At the
 *  prompt the blocks are **stacked** (F_0093 Decision A): `a` is `(na·p)×p` = `[A₁; A₂; …]` and
 *  `b` is `(nb·p)×m` = `[B₁; B₂; …]`, which at `p = 1` are exactly [[ArxModel]]'s `na×1` and
 *  `nb×m`. The sign of the `Aᵢ` is [[ArxModel]]'s predictor sign.
 *
 *  @param a the `na` autoregressive blocks `A₁ … A_na`, each `p×p`; empty for a finite impulse
 *           response
 *  @param b the `nb` input blocks `B₁ … B_nb`, each `p×m`, applied from lag `nk` on
 */
final case class ArxMimoModel(a: Vector[_MatrixValue], b: Vector[_MatrixValue])


/** The regressor matrix and target of an ARX fit (F_0060; several outputs since F_0093).
 *
 *  For `k = k₀ … N−1` with `k₀ = max(na, nk + nb − 1)`, row `k` is
 *  `[y[k−1]ᵀ … y[k−na]ᵀ, u₁[k−nk] … u₁[k−nk−nb+1], u₂[…] …]` — every output's past lag by lag,
 *  then each input's lags — and the target row is `y[k]ᵀ`. The outputs are `p×N` (a row per
 *  output) or `N×p`, the samples running along the longer side and rows-per-output on a tie, so
 *  a row or a column is one output exactly as before; the inputs are `m×N` (a row per input)
 *  or `N×m`, read by which side matches `N` — `m×N` when both do. It needs only one row; a
 *  short experiment's rows can be stacked with another's before fitting, which is why the
 *  rows-per-coefficient requirement belongs to [[arx]], not here.
 *
 *  @param u  the inputs
 *  @param y  the outputs
 *  @param na the autoregressive order, `≥ 0`
 *  @param nb the number of coefficients per input, `≥ 1`
 *  @param nk the input delay, `≥ 0`; `nk = 0` is direct feedthrough
 *  @return `(X, Y)`, `Y` with a column per output, or the reason they cannot be built
 */
def arxRegressors(u: _MatrixValue, y: _MatrixValue, na: Int, nb: Int, nk: Int): Either[ArxFailure, (_MatrixValue, _MatrixValue)] =
  arxRegressors(u, y, na, nb, nk, Limits.Default)

/** [[arxRegressors]] under `limits` (F_0124): the regressor matrix may have at most
 *  [[core.Limit.MaxMatrixCells]] cells. An overload rather than a defaulted parameter, which
 *  would be a binary break.
 *
 *  @param u      the inputs
 *  @param y      the output
 *  @param na     the autoregressive order, `≥ 0`
 *  @param nb     the number of coefficients per input, `≥ 1`
 *  @param nk     the input delay, `≥ 0`
 *  @param limits the budgets
 *  @return `(X, Y)`, or the reason they cannot be built
 */
def arxRegressors(u: _MatrixValue, y: _MatrixValue, na: Int, nb: Int, nk: Int,
                  limits: Limits): Either[ArxFailure, (_MatrixValue, _MatrixValue)] =
  for
    _  <- checkOrders(na, nb, nk)
    ys  = outputsOf(y)
    us <- inputsOf(u, ys.head.size)
    _  <- Either.cond((ys ++ us).forall(_.forall(_.isFinite)), (), NotNumeric("a sample is not a finite number"))
    xy <- layout(us, ys, na, nb, nk, limits(Limit.MaxMatrixCells))
  yield xy

/** Identifies an ARX model by least squares (F_0060), in the predictor form of [[ArxModel]].
 *
 *  **Declines data that does not excite the model** rather than fitting it: the regressor
 *  matrix must have full numerical rank at the session precision — `svd` and the relative
 *  tolerance `rank` uses (F_0061 Decision B) — before `leastSquares` is asked. QR alone would
 *  return numbers on an unexcited data set, meaningless ones. The fit is QR, never the normal
 *  equations. One output (MISO, Decision C); a model whose outputs are coupled is [[arxMimo]].
 *
 *  @param u   the inputs, `m×N` or `N×m`
 *  @param y   the output, `1×N` or `N×1`
 *  @param na  the autoregressive order
 *  @param nb  the coefficients per input
 *  @param nk  the input delay
 *  @param env the environment, whose precision sets the rank tolerance and whose limits the
 *             regressor size and the SVD's work
 *  @return the model, or the reason it was declined
 */
def arx(u: _MatrixValue, y: _MatrixValue, na: Int, nb: Int, nk: Int, env: Environment): Either[ArxFailure, ArxModel] =
  for
    _     <- Either.cond(y.rows == 1 || y.cols == 1, (), ShapeMismatch(
               s"y must be one output, a 1xN row or an Nx1 column: it is ${y.rows}x${y.cols} (several outputs are arxMimo)"))
    model <- arxMimo(u, y, na, nb, nk, env)
  yield ArxModel(model.a.map(_(0, 0)), Vector.tabulate(model.b.headOption.fold(0)(_.cols))(j => model.b.map(_(0, j))))

/** Identifies a **MIMO** ARX model by least squares (F_0093), the outputs coupled through
 *  `p×p` blocks `Aᵢ` (see [[ArxMimoModel]]).
 *
 *  Every output is regressed on the **same** regressor matrix — the stacked past of every output
 *  and the inputs ([[arxRegressors]]) — so the excitation test runs once and the fit is one QR
 *  solve per output column. Declines exactly as [[arx]] does; at `p = 1` it *is* [[arx]].
 *
 *  @param u   the inputs, `m×N` or `N×m`
 *  @param y   the outputs, `p×N` or `N×p`
 *  @param na  the autoregressive order
 *  @param nb  the coefficients per input
 *  @param nk  the input delay
 *  @param env the environment, whose precision sets the rank tolerance and whose limits the
 *             regressor size and the SVD's work
 *  @return the model, or the reason it was declined
 */
def arxMimo(u: _MatrixValue, y: _MatrixValue, na: Int, nb: Int, nk: Int, env: Environment): Either[ArxFailure, ArxMimoModel] =
  for
    xy     <- arxRegressors(u, y, na, nb, nk, env.limits)
    (x, t)  = xy
    // Building the regressors needs one row; fitting them needs one per coefficient.
    _      <- Either.cond(x.rows >= x.cols, (), TooShort(x.rows + math.max(na, nk + nb - 1), x.rows, x.cols))
    _      <- excited(x, env)
    thetas <- sequence(Vector.tabulate(t.cols)(r => leastSquares(x, _MatrixValue(t.rows, 1, Array.tabulate(t.rows)(t(_, r))))))
                .toRight(NotExcited(x.cols - 1, x.cols))
  yield
    val p = t.cols
    val m = (x.cols - p * na) / nb
    // theta(r)(c): output r's coefficient on regressor column c.
    val theta = thetas.map(_.toVector)
    ArxMimoModel(
      Vector.tabulate(na)(i => _MatrixValue(p, p, Array.tabulate(p * p)(idx => theta(idx / p)(i * p + idx % p)))),
      Vector.tabulate(nb)(l => _MatrixValue(p, m, Array.tabulate(p * m)(idx => theta(idx / m)(p * na + (idx % m) * nb + l)))))

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

/** The `p×m` discrete transfer matrix of a MIMO ARX model (F_0093), `G(z) = A(z)⁻¹B(z)`.
 *
 *  With `n = max(na, nk + nb − 1)`, `A(z) = zⁿI − Σ Aᵢzⁿ⁻ⁱ` and `B(z) = Σ Bₗzⁿ⁻ⁿᵏ⁻ˡ⁺¹`, so
 *  `G = adj(A)·B / det(A)`, the determinant and adjugate by cofactor expansion over coefficient
 *  vectors and every entry folded to lowest terms by `normaliseTf` — a common factor of
 *  `det(A)` that an entry does not carry cancels there. The cofactor expansion is held to
 *  [[core.Limit.MaxSymbolicDim]] outputs, the rule the symbolic determinant follows; past it
 *  the model is still realised by [[arx2ss]].
 *
 *  @param model the identified model
 *  @param nk    the input delay
 *  @param z     the discrete frequency variable to write the result in
 *  @return the transfer matrix as `p` rows of `m` rationals, or the reason it is malformed
 */
def arx2tf(model: ArxMimoModel, nk: Int, z: _Variable): Either[ArxFailure, Vector[Vector[_Expression]]] =
  arx2tf(model, nk, z, Limits.Default)

/** [[arx2tf]] under `limits` (F_0124): at most [[core.Limit.MaxSymbolicDim]] outputs. An overload
 *  rather than a defaulted parameter, which would be a binary break.
 *
 *  @param model  the identified model
 *  @param nk     the input delay
 *  @param z      the discrete frequency variable
 *  @param limits the budgets
 *  @return the transfer matrix as `p` rows of `m` rationals, or the reason it is malformed
 */
def arx2tf(model: ArxMimoModel, nk: Int, z: _Variable, limits: Limits): Either[ArxFailure, Vector[Vector[_Expression]]] =
  checkMimo(model, nk).flatMap { (p, m, nb) =>
    val maxP = limits(Limit.MaxSymbolicDim)
    if p > maxP then Left(ShapeMismatch(
      s"arx2tf writes the transfer matrix for at most $maxP outputs, by cofactor expansion: this model has $p (arx2ss realises it)"))
    else
      val n = math.max(model.a.size, nk + nb - 1)
      // A(z) and B(z) as matrices of coefficient vectors, ascending powers of z.
      val am = Vector.tabulate(p, p) { (r, s) =>
        Vector.tabulate(n + 1)(q => (if q == n && r == s then 1.0 else 0.0) - mimoA(model, n - q).fold(0.0)(_(r, s)))
      }
      val bm = Vector.tabulate(p, m)((r, j) => Vector.tabulate(n + 1)(q => mimoN(model, nk, n - q).fold(0.0)(_(r, j))))
      val det = polyDet(am)
      // adj(A)(r, k) = (-1)^(r + k) det(A without row k and column r).
      val adj = Vector.tabulate(p, p)((r, k) => polyScale(polyDet(minor(am, k, r)), if (r + k) % 2 == 0 then 1.0 else -1.0))
      Right(Vector.tabulate(p, m) { (r, j) =>
        val num = (0 until p).map(k => polyMul(adj(r)(k), bm(k)(j))).foldLeft(Vector(0.0))(polySum)
        normaliseTf(Ratio(polyToExpr(num, z), polyToExpr(det, z)), z)
      })
  }

/** A state-space realisation `[A, B, C, D]` of a MIMO ARX model (F_0093), discrete-time.
 *
 *  The **block observer form** (the owner's decision): with `n = max(na, nk + nb − 1)` block
 *  states of `p` each, `A` carries `A₁ … Aₙ` down its first block column and identities on its
 *  block superdiagonal, `C = [I, 0, …, 0]`, `D` the feedthrough `N₀` (`B₁` when `nk = 0`, else
 *  `0`), and block `k` of `B` is `Nₖ + Aₖ·N₀`, `Nₖ` the input block at lag `k`. **Not minimal**:
 *  `p·n` states where the McMillan degree may be lower — exact and simple, a correct non-minimal
 *  model simulates identically, and `controllable` / `observable` show what is redundant. At
 *  `p = 1` it is [[arx2ss]]'s observer canonical form.
 *
 *  @param model the identified model
 *  @param nk    the input delay
 *  @return `(A, B, C, D)`, or the reason — [[ArxFailure.NoState]] for a static gain
 */
def arx2ss(model: ArxMimoModel, nk: Int): Either[ArxFailure, (_MatrixValue, _MatrixValue, _MatrixValue, _MatrixValue)] =
  checkMimo(model, nk).flatMap { (p, m, nb) =>
    val n = math.max(model.a.size, nk + nb - 1)
    if n == 0 then Left(NoState)
    else
      val ns   = p * n
      val zero = _MatrixValue(p, m, Array.fill(p * m)(0.0))
      val n0   = mimoN(model, nk, 0).getOrElse(zero)
      val am = _MatrixValue(ns, ns, Array.tabulate(ns * ns) { idx =>
        val (r, c) = (idx / ns, idx % ns)
        if c < p then mimoA(model, r / p + 1).fold(0.0)(_(r % p, c))
        else if c / p == r / p + 1 && c % p == r % p then 1.0
        else 0.0
      })
      // Block k (0-based) of B is N_(k+1) + A_(k+1)·N_0: the feedthrough taken out of the numerator.
      val blocks = Vector.tabulate(n) { k =>
        val nk1 = mimoN(model, nk, k + 1).getOrElse(zero)
        mimoA(model, k + 1).fold(nk1)(ak => nk1.add(ak.multiply(n0)))
      }
      val bm = _MatrixValue(ns, m, Array.tabulate(ns * m)(idx => blocks((idx / m) / p)((idx / m) % p, idx % m)))
      val cm = _MatrixValue(p, ns, Array.tabulate(p * ns)(idx => if idx % ns == idx / ns then 1.0 else 0.0))
      Right((am, bm, cm, n0))
  }

/** `Aᵢ` for `i` in `1 .. na`, else `None` (a zero block). */
private def mimoA(model: ArxMimoModel, i: Int): Option[_MatrixValue] =
  Option.when(i >= 1 && i <= model.a.size)(model.a(i - 1))

/** The input block at lag `k`: `Bₗ` with `l = k − nk + 1` in `1 .. nb`, else `None`. */
private def mimoN(model: ArxMimoModel, nk: Int, k: Int): Option[_MatrixValue] =
  val l = k - nk + 1
  Option.when(l >= 1 && l <= model.b.size)(model.b(l - 1))

/** A MIMO model's shape: `nb ≥ 1` input blocks all `p×m`, autoregressive blocks all `p×p`,
 *  `nk ≥ 0`, every entry finite.
 *  @return `(p, m, nb)` */
private def checkMimo(model: ArxMimoModel, nk: Int): Either[ArxFailure, (Int, Int, Int)] =
  val (p, m) = model.b.headOption.fold((0, 0))(b => (b.rows, b.cols))
  for
    _ <- Either.cond(model.b.nonEmpty && model.b.forall(b => b.rows == p && b.cols == m), (),
           ShapeMismatch("b must hold nb >= 1 blocks of the same p x m shape"))
    _ <- Either.cond(model.a.forall(a => a.rows == p && a.cols == p), (),
           ShapeMismatch(s"every autoregressive block must be p x p, $p x $p for this b"))
    _ <- checkOrders(model.a.size, model.b.size, nk)
    _ <- Either.cond((model.a ++ model.b).forall(_.toVector.forall(_.isFinite)), (),
           NotNumeric("a coefficient is not a finite number"))
  yield (p, m, model.b.size)

/** The determinant of a square matrix of polynomials, by cofactor expansion along the first row.
 *  Untrimmed sums: `polyAdd`'s absolute trim would erase a genuinely small coefficient (F_0119),
 *  and `normaliseTf` trims by magnitude afterwards. */
private def polyDet(m: Vector[Vector[Vector[Double]]]): Vector[Double] =
  if m.isEmpty then Vector(1.0)     // the 0x0 minor of a 1x1 matrix: adj = [1]
  else if m.size == 1 then m(0)(0)
  else m.indices.foldLeft(Vector(0.0)) { (acc, c) =>
    val term = polyMul(m(0)(c), polyDet(minor(m, 0, c)))
    polySum(acc, if c % 2 == 0 then term else polyScale(term, -1.0))
  }

/** `m` without row `r` and column `c`. */
private def minor[A](m: Vector[Vector[A]], r: Int, c: Int): Vector[Vector[A]] =
  m.patch(r, Nil, 1).map(_.patch(c, Nil, 1))

/** The untrimmed sum of two coefficient vectors. */
private def polySum(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  Vector.tabulate(math.max(a.size, b.size))(i => a.lift(i).getOrElse(0.0) + b.lift(i).getOrElse(0.0))

private def polyScale(a: Vector[Double], k: Double): Vector[Double] = a.map(_ * k)

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

/** The outputs as one series each, the samples along the longer side, rows-per-output on a tie
 *  (F_0093): a row or a column is one output, as it always was. */
private def outputsOf(y: _MatrixValue): Vector[Vector[Double]] =
  if y.cols >= y.rows then Vector.tabulate(y.rows)(i => Vector.tabulate(y.cols)(y(i, _)))
  else Vector.tabulate(y.cols)(j => Vector.tabulate(y.rows)(y(_, j)))

/** The inputs as one series each: rows when a row is as long as the output, else columns. */
private def inputsOf(u: _MatrixValue, n: Int): Either[ArxFailure, Vector[Vector[Double]]] =
  if u.cols == n then Right(Vector.tabulate(u.rows)(i => Vector.tabulate(n)(u(i, _))))
  else if u.rows == n then Right(Vector.tabulate(u.cols)(j => Vector.tabulate(n)(u(_, j))))
  else Left(ShapeMismatch(s"u has no side as long as y's $n samples: it is ${u.rows}x${u.cols}"))

/** The regressor rows, after the length and size checks: every output's past lag by lag, then
 *  each input's lags, so at one output the columns are F_0060's. */
private def layout(us: Vector[Vector[Double]], ys: Vector[Vector[Double]], na: Int, nb: Int, nk: Int, maxCells: Long)
    : Either[ArxFailure, (_MatrixValue, _MatrixValue)] =
  val p    = ys.size
  val n    = ys.head.size
  val k0   = math.max(na.toLong, nk.toLong + nb - 1)
  val past = p.toLong * na
  val cols = past + us.size.toLong * nb
  val rows = n - k0
  if rows < 1 then Left(TooShort(n, 0, math.min(cols, Int.MaxValue.toLong).toInt))
  else if rows * cols > maxCells then Left(TooLarge(rows.toInt, cols.toInt))
  else
    val (r, c, start, pa) = (rows.toInt, cols.toInt, k0.toInt, past.toInt)
    val x = _MatrixValue(r, c, Array.tabulate(r * c) { idx =>
      val (k, col) = (start + idx / c, idx % c)
      if col < pa then ys(col % p)(k - 1 - col / p)
      else
        val q = col - pa
        us(q / nb)(k - nk - q % nb)
    })
    Right((x, _MatrixValue(r, p, Array.tabulate(r * p)(idx => ys(idx % p)(start + idx / p)))))

/** Full numerical column rank at the session precision, or the reason the data does not have it. */
private def excited(x: _MatrixValue, env: Environment): Either[ArxFailure, Unit] =
  x.svd(env.limits) match
    case Some((_, sigma, _)) =>
      val r = numericalRank(sigma, rankTolerance(env))
      Either.cond(r == x.cols, (), NotExcited(r, x.cols))
    case None if _MatrixValue.svdWork(x.rows, x.cols) > env.limits(Limit.MaxSvdWork) => Left(TooLarge(x.rows, x.cols))
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
      case Some(m) => Left(ShapeMismatch(
        s"a is ${m.rows}x${m.cols}, the stacked p x p blocks of a model with several outputs, not one output's coefficients"))
      case None    => Left(NotNumeric("a is not a vector of coefficients, or 0 for none"))
  for
    as <- av
    bm <- arxMatrix(b, "b (the nb x m matrix arx returns)", env)
    k  <- arxOrder(nk, "nk", 0, env)
  yield (as, Vector.tabulate(bm.cols)(j => Vector.tabulate(bm.rows)(bm(_, j))), k)

/** The `(model, nk)` operands of the converters when `a` holds **stacked `p×p` blocks** — both
 *  sides at least 2, which no single-output `a` has (F_0093): the converters tell the two
 *  model classes apart by `a`, since a stacked `b` and an `nb×m` one cannot be told apart by
 *  shape. `zeros(p, p)`, what [[_Arx]] writes for `na = 0`, reads as one zero block (Decision B).
 *  @return `None` when `a` is a single-output `a`, else the operands or the reason */
private def mimoOperands(a: _Expression, b: _Expression, nk: _Expression, env: Environment)
    : Option[Either[ArxFailure, (ArxMimoModel, Int)]] =
  a.eval(env) match
    case Right(_Number(_)) => None
    case other => _MatrixValue.ofEvaluated(other).filter(m => m.rows >= 2 && m.cols >= 2).map { am =>
      val p = am.cols
      for
        _  <- Either.cond(am.rows % p == 0, (), ShapeMismatch(s"a must stack p x p blocks: it is ${am.rows}x$p"))
        bm <- arxMatrix(b, "b (the stacked p x m blocks arx returns)", env)
        _  <- Either.cond(bm.rows >= p && bm.rows % p == 0, (),
                ShapeMismatch(s"b must stack $p x m blocks, one per input lag: it is ${bm.rows}x${bm.cols}"))
        k  <- arxOrder(nk, "nk", 0, env)
      yield (ArxMimoModel(blocksOf(am, p), blocksOf(bm, p)), k)
    }

/** A matrix of stacked `p`-row blocks, split. */
private def blocksOf(m: _MatrixValue, p: Int): Vector[_MatrixValue] =
  Vector.tabulate(m.rows / p)(i => _MatrixValue(p, m.cols, Array.tabulate(p * m.cols)(idx => m(i * p + idx / m.cols, idx % m.cols))))

/** Blocks stacked into one matrix, the inverse of [[blocksOf]]. */
private def stacked(blocks: Vector[_MatrixValue]): _MatrixValue =
  val (p, c) = (blocks.head.rows, blocks.head.cols)
  _MatrixValue(p * blocks.size, c, Array.tabulate(p * blocks.size * c)(idx => blocks((idx / c) / p)((idx / c) % p, idx % c)))


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
    arxOperands(u, y, na, nb, nk, env).flatMap((um, ym, a, b, k) => arxRegressors(um, ym, a, b, k, env.limits))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), (x, t) => Left(_Matrix(1, 2, Vector(x, t))))


/** `arx(u, y, na, nb, nk)` — [[arx]] as a node, the row `[[a, b]]` (F_0060; several outputs
 *  since F_0093, through [[arxMimo]]).
 *
 *  `a` is the `na×1` column of autoregressive coefficients in the predictor sign, or `0` when
 *  `na = 0` — a matrix cannot have zero rows, and `0` for an absent group is the `qp`/`kkt`
 *  convention; `b` is `nb×m`, a column per input. Tuple assignment binds both:
 *  `a, b := arx(u, y, 2, 1, 1)`. **With `p ≥ 2` outputs the blocks are stacked** (Decision A):
 *  `a` is `(na·p)×p` = `[A₁; A₂; …]`, or `zeros(p, p)` when `na = 0` (Decision B), and `b` is
 *  `(nb·p)×m` = `[B₁; B₂; …]` — the single-output shapes at `p = 1`. A refusal leaves the call as
 *  written, and [[declineReason]] carries the reason the REPL prints.
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

  /** Why the call stays as written, for one output or several (F_0093) — [[detailed]] keeps its
   *  single-output contract and so refuses a `p×N` output that this node answers.
   *  @param env the evaluation environment
   *  @return the reason, or `None` when it evaluates */
  def declineReason(env: Environment): Option[ArxFailure] = fitted(env).left.toOption

  override def eval(env: Environment): Either[_Expression, _Value] =
    fitted(env).fold(_ => Left(this), model => Left(_Matrix(1, 2, Vector(aOf(model), stacked(model.b)))))

  /** The model at any number of outputs: at one, [[arx]] itself is [[arxMimo]]. */
  private def fitted(env: Environment): Either[ArxFailure, ArxMimoModel] =
    arxOperands(u, y, na, nb, nk, env).flatMap((um, ym, a, b, k) => arxMimo(um, ym, a, b, k, env))

  /** `a` stacked, or `0` for none at one output and `zeros(p, p)` at several. */
  private def aOf(m: ArxMimoModel): _Expression =
    val p = m.b.head.rows
    if m.a.nonEmpty then stacked(m.a)
    else if p == 1 then _Number(0)
    else _MatrixValue(p, p, Array.fill(p * p)(0.0))


/** `arx2tf(a, b, nk, z)` — [[arx2tf]] as a node: one rational in `z` for one input, a `1×m` row
 *  of them for several; for a model with several outputs (`a` stacked `p×p` blocks, F_0093) the
 *  `p×m` transfer matrix. `z` is excluded from `children` and carried through `rebuild`, the
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

  /** Why the call stays as written, for one output or several (F_0093) — [[detailed]] keeps its
   *  single-output contract.
   *  @param env the evaluation environment
   *  @return the reason, or `None` when it evaluates */
  def declineReason(env: Environment): Option[ArxFailure] =
    mimo(env).fold(detailed(env).left.toOption)(_.left.toOption)

  override def eval(env: Environment): Either[_Expression, _Value] =
    mimo(env) match
      case Some(table) => table.fold(_ => Left(this), g => Left(_Matrix(g.size, g.head.size, g.flatten)))
      case None =>
        detailed(env) match
          case Left(_)                  => Left(this)
          case Right(Vector(v: _Value)) => Right(v)
          case Right(Vector(g))         => Left(g)
          case Right(gs)                => Left(_Matrix(1, gs.size, gs))

  /** The transfer matrix, when `a` holds stacked blocks; `None` for a single-output model. */
  private def mimo(env: Environment): Option[Either[ArxFailure, Vector[Vector[_Expression]]]] =
    mimoOperands(a, b, nk, env).map(_.flatMap((m, k) => arx2tf(m, k, z, env.limits)))


/** `arx2ss(a, b, nk)` — [[arx2ss]] as a node, the state-space row `[[A, B, C, D]]`; for a model
 *  with several outputs (`a` stacked `p×p` blocks, F_0093) the block observer form.
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
    mimoOperands(a, b, nk, env) match
      case Some(ops) => ops.flatMap((m, k) => arx2ss(m, k))
      case None      => modelOperands(a, b, nk, env).flatMap((as, bs, k) => arx2ss(as, bs, k))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), (am, bm, cm, dm) => Left(stateSpace(am, bm, cm, dm)))
