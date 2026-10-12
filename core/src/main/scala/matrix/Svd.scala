package it.grypho.scala.leonardo
package matrix

import core.*


/** The relative tolerance below which a singular value counts as zero (F_0061 Decision B):
 *  `σ ≤ 0.5·10⁻ᵖ·σ_max` at session precision `p` — the `=` tolerance applied to the spectrum.
 *
 *  **Relative to the largest singular value, never absolute**: an absolute threshold is a claim
 *  about the matrix's units, so `rank(k·A)` would move with `k` (the 2.11 rule). Shared by
 *  `rank` and `pinv`, so the two cannot disagree about which directions are numerically zero.
 *
 *  @param env the evaluation environment, whose precision sets the tolerance
 *  @return the relative tolerance
 */
private[leonardo] def rankTolerance(env: Environment): Double = 0.5 * math.pow(10, -env.precision)

/** The number of singular values (sorted descending) above `relTol·σ_max`; `0` for the zero matrix.
 *
 *  @param sigma  the singular values, largest first
 *  @param relTol the tolerance relative to the largest
 *  @return the numerical rank
 */
private[leonardo] def numericalRank(sigma: Vector[Double], relTol: Double): Int =
  sigma.headOption.filter(_ > 0).fold(0)(sMax => sigma.count(_ > relTol * sMax))

/** The Moore–Penrose pseudo-inverse `V·Σ⁺·Uᵀ`, inverting only the singular values `rank` counts
 *  at the same tolerance — the rest are treated as exactly zero, which is what keeps a
 *  rank-deficient matrix from returning a pseudo-inverse dominated by rounding.
 *
 *  @param a      the matrix
 *  @param relTol the tolerance relative to the largest singular value
 *  @return the `cols × rows` pseudo-inverse, or `None` when the SVD declines
 */
private[leonardo] def pseudoInverse(a: _MatrixValue, relTol: Double): Option[_MatrixValue] =
  a.svd.map { (u, sigma, v) =>
    val r = numericalRank(sigma, relTol)
    _MatrixValue(a.cols, a.rows, Array.tabulate(a.cols * a.rows) { idx =>
      val (i, j) = (idx / a.rows, idx % a.rows)
      (0 until r).foldLeft(0.0)((acc, l) => acc + v(i, l) * u(j, l) / sigma(l))
    })
  }

/** The 2-norm condition number `σ_max/σ_min` over the `min(m, n)` singular values, `inf` when the
 *  smallest is lost in rounding: `σ_min ≤ max(m, n)·ε·σ_max`, or the matrix is zero.
 *
 *  **Deliberately not Decision B's tolerance.** `rank` and `pinv` must decide which directions
 *  to *discard*, and the session precision is the right scale for that; a condition number
 *  *reports* how close to singular a matrix is, so it must stay finite and large right up to
 *  the point where the smallest singular value carries no digits at all — the cut-off LAPACK
 *  and numpy use for a numerically singular matrix. At precision 5, `cond` of a matrix with
 *  `σ_min/σ_max = 1e-8` is therefore `1e8`, while `rank` already counts that direction as zero.
 *
 *  @param sigma the singular values, largest first
 *  @param m     the row count
 *  @param n     the column count
 *  @return the condition number, possibly `+∞`
 */
private[leonardo] def conditionNumber(sigma: Vector[Double], m: Int, n: Int): Double =
  (sigma.headOption, sigma.lastOption) match
    case (Some(sMax), Some(sMin)) if sMax > 0 && sMin > math.max(m, n) * _MatrixValue.Epsilon * sMax =>
      sMax / sMin
    case _ => Double.PositiveInfinity

/** Why an SVD-based node with this evaluated operand stays unevaluated, when the operand is a
 *  real matrix and the reason is not merely that it is symbolic (F_0052 Decision A): the REPL
 *  appends it as a note, so a refusal never echoes the call back unexplained (the F_0079 rule).
 *
 *  @param operand the evaluated matrix operand
 *  @return the reason, or `None` when the decomposition succeeds or the operand is not numeric
 */
private[leonardo] def svdDecline(operand: Either[_Expression, _Value]): Option[String] =
  _MatrixValue.ofEvaluated(operand).flatMap { a =>
    if a.svd.isDefined then None
    else if !a.isFinite then Some("the matrix has a non-finite entry")
    else if _MatrixValue.svdWork(a.rows, a.cols) > _MatrixValue.MaxSvdWork then
      Some(s"the matrix is ${a.rows}x${a.cols}, past the decomposition's work cap " +
        s"(max(m, n)·min(m, n)² at most ${_MatrixValue.MaxSvdWork})")
    else Some("the decomposition did not converge to a certified answer")
  }

/** What a declined SVD-based node evaluates to: the call **as written** when its operand was a
 *  real matrix the decomposition refused (the `lu`/`expm` convention — printing the evaluated
 *  operand would spell out a matrix past the work cap cell by cell), otherwise the call rebuilt
 *  over its partially reduced operand.
 */
private def unevaluated(r: Either[_Expression, _Value], self: _Expression, rebuilt: => _Expression): _Expression =
  if _MatrixValue.ofEvaluated(r).isDefined then self else rebuilt

/** The columns of a dense matrix, as arrays. */
private def columnsOf(q: _MatrixValue): Vector[Array[Double]] =
  Vector.tabulate(q.cols)(j => Array.tabulate(q.rows)(i => q(i, j)))

/** A dense matrix built from columns of length `rows`. */
private def ofColumns(cols: Vector[Array[Double]], rows: Int): _MatrixValue =
  val k = cols.size
  _MatrixValue(rows, k, Array.tabulate(rows * k)(idx => cols(idx % k)(idx / k)))

/** The `rows × cols` matrix with `sigma` on its diagonal. */
private def diagonal(sigma: Vector[Double], rows: Int, cols: Int): _MatrixValue =
  _MatrixValue(rows, cols, Array.tabulate(rows * cols) { idx =>
    val (i, j) = (idx / cols, idx % cols)
    if i == j then sigma(i) else 0.0
  })

/** The singular value decomposition `A = U·S·Vᵀ` of a dense matrix, as the three factors.
 *
 *  **Economy** (the default, F_0061 Decision A): with `k = min(m, n)`, `U` is `m×k`, `S` the
 *  `k×k` diagonal of singular values largest first, `V` `n×k` — the factors every
 *  computation needs, with nothing in them that multiplies a zero. **Full**: `U` is `m×m`, `S`
 *  `m×n`, `V` `n×n`, the extra columns completing orthonormal bases of the left and right null
 *  spaces — the form a null-space or complementary-subspace question reads.
 *
 *  @param a    the matrix
 *  @param full whether to complete `U` and `V` to square orthogonal matrices
 *  @return `(U, S, V)`, or `None` when the kernel declines
 */
private[leonardo] def svdFactors(a: _MatrixValue, full: Boolean): Option[(_MatrixValue, _MatrixValue, _MatrixValue)] =
  a.svd.flatMap { (u, sigma, v) =>
    if !full then Some((u, diagonal(sigma, sigma.size, sigma.size), v))
    else
      for
        us <- _MatrixValue.completeBasis(columnsOf(u), a.rows, a.rows)
        vs <- _MatrixValue.completeBasis(columnsOf(v), a.cols, a.cols)
      yield (ofColumns(us, a.rows), diagonal(sigma, a.rows, a.cols), ofColumns(vs, a.cols))
  }


/** Singular value decomposition: `svd(A)` — `[[U, S, V]]` with `A = U·S·Vᵀ` (F_0061).
 *
 *  Economy by default; `svd(A, full)` gives the square `U` and `V` (Decision A — the slot word
 *  `full` is matched only there and is **not** reserved). The kernel is one-sided Jacobi
 *  ([[core._MatrixValue.svd]]), certified against its own reconstruction and orthonormality, so
 *  a matrix it cannot decompose stays unevaluated rather than being answered approximately.
 *  An **exact** matrix demotes, the `lu`/`qr` precedent: an SVD is iterative and irrational in
 *  general. A symbolic matrix stays symbolic.
 *
 *  Like `lu`, the result is a row of matrices, so it is a plain `_Expression` and tuple
 *  assignment binds the factors: `U, S, V := svd(A)`.
 *
 *  @param m    the matrix expression to decompose
 *  @param full whether to return the full rather than the economy factors
 */
case class _Svd(m: _Expression, full: Boolean = false) extends _Expression:
  override def toString: String = if full then s"svd($m, full)" else s"svd($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _Svd(c.head, full)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = m.eval(env)
    _MatrixValue.ofEvaluated(r).flatMap(svdFactors(_, full)) match
      case Some((u, s, v)) => Left(_Matrix(1, 3, Vector(u, s, v)))
      case None            => Left(unevaluated(r, this, _Svd(r.toExpression, full)))

  /** Why this call stays unevaluated, when a real matrix was refused; see [[svdDecline]].
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = svdDecline(m.eval(env))


/** Moore–Penrose pseudo-inverse: `pinv(A)`, the `n×m` matrix `V·Σ⁺·Uᵀ` (F_0061).
 *
 *  Singular values at or below `0.5·10⁻ᵖ·σ_max` are treated as zero (Decision B, shared with
 *  `rank`), so `pinv` of a rank-deficient matrix is the pseudo-inverse of its numerical-rank
 *  part rather than one amplified by rounding. For a square non-singular matrix it is the
 *  inverse; for a tall full-column-rank one it is the least-squares solver `(AᵀA)⁻¹Aᵀ`, without
 *  ever forming `AᵀA` (the conditioning rule). An exact matrix demotes; a symbolic one stays
 *  symbolic.
 *
 *  Extends `_MatrixOperation`, like `chol`, because its result is a single matrix.
 *
 *  @param m the matrix expression to pseudo-invert
 */
case class _PInv(m: _Expression) extends _MatrixOperation:
  override def toString: String = s"pinv($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _PInv(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = m.eval(env)
    _MatrixValue.ofEvaluated(r).flatMap(pseudoInverse(_, rankTolerance(env))) match
      case Some(p) => Right(p)
      case None    => Left(unevaluated(r, this, _PInv(r.toExpression)))

  /** Why this call stays unevaluated, when a real matrix was refused; see [[svdDecline]].
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = svdDecline(m.eval(env))


/** Numerical rank: `rank(A)` or `rank(A, tol)` — the number of singular values above
 *  `tol·σ_max` (F_0061 Decision B).
 *
 *  The default `tol` is `0.5·10⁻ᵖ` at session precision `p`; an explicit one is **relative to
 *  the largest singular value too**, so `rank(k·A, tol)` never depends on `k`. It must be a
 *  number in `[0, 1)`; anything else stays unevaluated. The zero matrix has rank `0`. A SCALAR
 *  result, so this does not extend `_MatrixOperation` (the `det` rule). The count is built in
 *  the operand's tier — an exact matrix answers an exact integer — although the decomposition
 *  behind it is `Double`.
 *
 *  @param m   the matrix expression
 *  @param tol the optional relative tolerance
 */
case class _Rank(m: _Expression, tol: Option[_Expression] = None) extends _Expression:
  override def toString: String = tol.fold(s"rank($m)")(t => s"rank($m, $t)")
  override def children: List[_Expression] = m :: tol.toList
  override def rebuild(c: List[_Expression]): _Expression = _Rank(c.head, c.tail.headOption)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r  = m.eval(env)
    val rt = tol.map(_.eval(env))
    val relTol = rt match
      case None                                              => Some(rankTolerance(env))
      case Some(Right(_Number(t))) if t >= 0 && t < 1        => Some(t)
      case _                                                 => None
    (for
      t     <- relTol
      a     <- _MatrixValue.ofEvaluated(r)
      sigma <- a.svd.map(_._2)
    yield _Rational.literalLike(numericalRank(sigma, t), r.toExpression)) match
      case Some(n) => Right(n)
      case None    => Left(unevaluated(r, this, _Rank(r.toExpression, rt.map(_.toExpression))))

  /** Why this call stays unevaluated: a tolerance that is a number outside `[0, 1)`, or a real
   *  matrix refused by the decomposition (see [[svdDecline]]).
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] =
    tol.map(_.eval(env)) match
      case Some(Right(_Number(t))) if !(t >= 0 && t < 1) =>
        Some("the tolerance must be a number in [0, 1), relative to the largest singular value")
      case _ => svdDecline(m.eval(env))


/** Which subspace a [[_NullSpace]] node asks for. */
enum Subspace:
  /** `nullspace(A)`: the null space, `{x : A·x = 0}`, spanned by the trailing columns of the full `V`. */
  case Null
  /** `orth(A)`: the range (column space), spanned by the leading columns of `U`. */
  case Range

object Subspace:
  /** The grammar keyword for a subspace.
   *  @param s the subspace
   *  @return `nullspace` or `orth`
   */
  def keyword(s: Subspace): String = s match
    case Null  => "nullspace"
    case Range => "orth"

/** An orthonormal basis of the null space or of the range of `a`, as columns (F_0087).
 *
 *  Both are read off **one** SVD at **one** rank, `numericalRank` at `relTol` — the definition
 *  `rank` and `pinv` already share, so `cols(nullspace) = n − rank` and `cols(orth) = rank` hold
 *  by construction rather than by agreement. The null space needs the **full** `V` (an `m < n`
 *  matrix's economy `V` has only `m` columns); the range needs only the economy `U`.
 *
 *  @param a      the matrix
 *  @param which  the subspace
 *  @param relTol the tolerance relative to the largest singular value
 *  @return `Some(Some(B))` for a basis `B`, `Some(None)` for the trivial subspace, `None` when the
 *          decomposition declines
 */
private[leonardo] def subspaceBasis(a: _MatrixValue, which: Subspace, relTol: Double): Option[Option[_MatrixValue]] =
  svdFactors(a, full = which == Subspace.Null).map { (u, s, v) =>
    val sigma = (0 until math.min(s.rows, s.cols)).map(i => s(i, i)).toVector
    val r     = numericalRank(sigma, relTol)
    which match
      case Subspace.Null  => Option.when(r < a.cols)(ofColumns(columnsOf(v).drop(r), a.cols))
      case Subspace.Range => Option.when(r > 0)(ofColumns(columnsOf(u).take(r), a.rows))
  }

/** An orthonormal basis of the null space, `nullspace(A)`, or of the range, `orth(A)`, each with
 *  `rank`'s optional relative tolerance (F_0087).
 *
 *  The answer is a matrix whose columns are the basis. **A trivial subspace answers `false`** —
 *  the null space of a full-column-rank matrix, the range of the zero matrix — since a matrix
 *  cannot have zero columns, and `false` is what `tfpoles` and `stationary` answer for provably
 *  none. The zero matrix's null space is the whole space. The basis is unique only up to
 *  rotation; its columns come from the SVD's `V` or `U`. An exact matrix demotes and a symbolic
 *  one stays symbolic, the `rank` rules; a tolerance that is not a number in `[0, 1)` leaves the
 *  call as written, with the reason in [[declineReason]].
 *
 *  It is the null-space method's `N` in `z = z₀ + N·w` for equality constraints, which is why it
 *  was filed (F_0061 Decision A, for F_0074 and F_0066).
 *
 *  @param which the subspace
 *  @param m     the matrix expression
 *  @param tol   the optional relative tolerance
 */
case class _NullSpace(which: Subspace, m: _Expression, tol: Option[_Expression] = None) extends _Expression:
  override def toString: String =
    tol.fold(s"${Subspace.keyword(which)}($m)")(t => s"${Subspace.keyword(which)}($m, $t)")
  override def children: List[_Expression] = m :: tol.toList
  override def rebuild(c: List[_Expression]): _Expression = _NullSpace(which, c.head, c.tail.headOption)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r  = m.eval(env)
    val rt = tol.map(_.eval(env))
    val relTol = rt match
      case None                                       => Some(rankTolerance(env))
      case Some(Right(_Number(t))) if t >= 0 && t < 1 => Some(t)
      case _                                          => None
    (for
      t <- relTol
      a <- _MatrixValue.ofEvaluated(r)
      b <- subspaceBasis(a, which, t)
    yield b) match
      case Some(Some(b)) => Right(b)
      case Some(None)    => Right(_Bool(false))
      case None          => Left(unevaluated(r, this, _NullSpace(which, r.toExpression, rt.map(_.toExpression))))

  /** Why this call stays unevaluated: a tolerance that is a number outside `[0, 1)`, or a real
   *  matrix refused by the decomposition (see [[svdDecline]]).
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] =
    tol.map(_.eval(env)) match
      case Some(Right(_Number(t))) if !(t >= 0 && t < 1) =>
        Some("the tolerance must be a number in [0, 1), relative to the largest singular value")
      case _ => svdDecline(m.eval(env))


/** 2-norm condition number: `cond(A)`, `σ_max/σ_min` over the `min(m, n)` singular values
 *  (F_0061), `inf` for a numerically singular matrix.
 *
 *  "Numerically singular" here is `σ_min ≤ max(m, n)·ε·σ_max`, **not** Decision B's
 *  precision-scaled tolerance — see [[conditionNumber]] for why the two questions take
 *  different cut-offs. A SCALAR result (the `det` rule); always a `Double`, since a ratio of
 *  singular values is irrational in general.
 *
 *  @param m the matrix expression
 */
case class _Cond(m: _Expression) extends _Expression:
  override def toString: String = s"cond($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _Cond(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = m.eval(env)
    _MatrixValue.ofEvaluated(r).flatMap(a => a.svd.map(s => conditionNumber(s._2, a.rows, a.cols))) match
      case Some(c) => Right(_Number(c))
      case None    => Left(unevaluated(r, this, _Cond(r.toExpression)))

  /** Why this call stays unevaluated, when a real matrix was refused; see [[svdDecline]].
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = svdDecline(m.eval(env))
