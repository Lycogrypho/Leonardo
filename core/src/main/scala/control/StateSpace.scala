package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix.*


/** [[https://en.wikipedia.org/wiki/State-space_representation State-space]] models and the discretisation that needs them.
 *
 *  **A model is a 1x4 `matrix._Matrix` of matrices**, exactly the shape `lu`/`qr`/`eig` return,
 *  so `at(model, 1, k)` indexes it and `:save` round-trips it with no new machinery
 *  — the shared indexing convention.
 */

/** Bundles `(A, B, C, D)` into the 1x4 row that represents a state-space model.
 *
 *  @param a state matrix
 *  @param b input matrix
 *  @param c output matrix
 *  @param d feedthrough matrix
 *  @return the model as a `_Matrix` node
 */
def stateSpace(a: _Expression, b: _Expression, c: _Expression, d: _Expression): _Expression =
  _Matrix(1, 4, Vector(a, b, c, d))

/** Reduces an expression to a dense matrix value, or `None`; an exact matrix demotes (F_0080 —
 *  it was declined, so `controllable` refused an exact literal the rest of the library accepts).
 *  The environment is empty because these functions take none: bind by substituting first. */
private def denseOf(e: _Expression): Option[_MatrixValue] =
  _MatrixValue.ofEvaluated(e.eval(new Environment()))

/** Controllability matrix `[B, A*B, A^2*B, …, A^(n-1)*B]`, joined by the shared block assembly
 *  (F_0055) — the private concatenation this file used to carry was a second definition of
 *  `hcat`.  `None` only past `matrix.MaxMatrixCells`, at its **default**: `controllable` and
 *  `observable` are published without an `Environment` (F_0124), and the cap binds only past
 *  `n²·m = 10⁶`, far beyond the plants the Riccati family accepts. */
private def ctrbOf(a: _MatrixValue, b: _MatrixValue, maxCells: Long = MaxMatrixCells): Option[_MatrixValue] =
  val blocks = (1 until a.rows).foldLeft(Vector(b))((acc, _) => acc :+ a.multiply(acc.last))
  assembleBlocks(Vector(blocks.map(_Matrix.fromValue)), maxCells).flatMap(matrix.denseOf)

/** Full row rank test, by QR of the transpose.
 *
 *  `qrDecompose` returns `None` exactly when its input is rank-deficient (a column
 *  orthogonalises to zero norm), so `rank(M) = rows` is precisely `Mᵀ` decomposing.  The
 *  transpose is what makes it applicable: a controllability matrix is `n x (n*m)` and so is
 *  wide for every multi-input plant, while QR requires rows >= cols.
 *
 *  **Deliberately not `det(M * Mᵀ)`, which is the trap this replaced**.  That
 *  form is wrong twice over.  Forming the Gram matrix **squares the condition number** — the
 *  very reason `statistics.leastSquares` solves by QR rather than by the normal equations, so
 *  using it here contradicted a rule the library had already settled.  And a determinant
 *  scales like `‖M‖^(2n)`, so any fixed threshold is really a statement about the *units* of
 *  the model: scaling `B` by `1e-3` — millivolts instead of volts — drove a perfectly
 *  controllable two-state plant to `1e-12` and reported it uncontrollable.  Controllability
 *  is invariant under that scaling, which is what the tests pin.
 */
private def fullRowRank(m: _MatrixValue): Boolean =
  m.transpose.qrDecompose.isDefined

/** Is the pair `(A, B)` [[https://en.wikipedia.org/wiki/Controllability controllable]] — can the input steer every state?
 *
 *  @param a the state matrix
 *  @param b the input matrix
 *  @return `Some(true)`/`Some(false)`, or `None` when either operand is not a dense matrix
 */
def controllable(a: _Expression, b: _Expression): Option[Boolean] =
  for
    am <- denseOf(a); bm <- denseOf(b)
    if am.rows == am.cols && bm.rows == am.rows
    ctrb <- ctrbOf(am, bm)
  yield fullRowRank(ctrb)

/** Is the pair `(A, C)` [[https://en.wikipedia.org/wiki/Observability observable]] — can the output distinguish every state?
 *
 *  Observability of `(A, C)` is controllability of `(Aᵀ, Cᵀ)` — the duality, used here rather
 *  than restated, so the two can never disagree.
 *
 *  @param a the state matrix
 *  @param c the output matrix
 *  @return `Some(true)`/`Some(false)`, or `None` when either operand is not a dense matrix
 */
def observable(a: _Expression, c: _Expression): Option[Boolean] =
  for
    am <- denseOf(a); cm <- denseOf(c)
    if am.rows == am.cols && cm.cols == am.rows
    obsv <- ctrbOf(am.transpose, cm.transpose)
  yield fullRowRank(obsv)

/** Which structural question a [[_Structure]] node asks (F_0134), with its prompt word. */
enum StructureKind(val word: String):
  /** `controllable(A, B)` -- the verdict of [[controllable]]. */
  case Controllable extends StructureKind("controllable")
  /** `observable(A, C)` -- the verdict of [[observable]]. */
  case Observable extends StructureKind("observable")
  /** `ctrb(A, B)` -- the controllability matrix `[B, A·B, …, Aⁿ⁻¹·B]`. */
  case Ctrb extends StructureKind("ctrb")
  /** `obsv(A, C)` -- the observability matrix `[C; C·A; …; C·Aⁿ⁻¹]`. */
  case Obsv extends StructureKind("obsv")

  /** Whether the question is about the output pair `(A, C)` rather than the input pair. */
  def dual: Boolean = this == Observable || this == Obsv


/** `controllable`, `observable`, `ctrb` and `obsv` at the prompt (F_0134), from `(A, B)` /
 *  `(A, C)` or from a `[[A, B, C, D]]` model row.
 *
 *  The verdicts are the library's own tests, run on operands evaluated in the session, so the
 *  prompt and Scala cannot disagree: full row rank by QR of the transpose, never `det(M·Mᵀ)`
 *  (2.11), and observability through the duality.  Operands are read as `dlsim` reads them
 *  (`matrixOperand`): a number is a `1×1` matrix, and `0` the zero block of the conforming
 *  shape.  `ctrb`/`obsv` are capped by the environment's `Limit.MaxMatrixCells`.
 *
 *  @param kind the question
 *  @param a    the state matrix, or the model row when `x` is absent
 *  @param x    the input matrix `B` or the output matrix `C`
 */
case class _Structure(kind: StructureKind, a: _Expression, x: Option[_Expression]) extends _Expression:
  override def toString: String = s"${kind.word}($a${x.fold("")(m => s", $m")})"
  override def children: List[_Expression] = a :: x.toList
  override def rebuild(c: List[_Expression]): _Expression = _Structure(kind, c.head, x.map(_ => c(1)))

  /** The evaluated pair `(A, B)` or `(A, C)`. */
  private def pair(env: Environment): Option[(Either[_Expression, _Value], Either[_Expression, _Value])] =
    x match
      case Some(m) => Some((a.eval(env), m.eval(env)))
      case None    => modelRow(a, env).map(cells => (cells(0), cells(if kind.dual then 2 else 1)))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val answer = for
      (ra, rx) <- pair(env)
      am       <- matrixOperand(ra, None) if am.rows == am.cols
      n         = am.rows
      xm       <- matrixOperand(rx, Some(if kind.dual then (1, n) else (n, 1)))
      if kind.dual && xm.cols == n || !kind.dual && xm.rows == n
      m        <- if kind.dual then ctrbOf(am.transpose, xm.transpose, env.limits(Limit.MaxMatrixCells))
                  else ctrbOf(am, xm, env.limits(Limit.MaxMatrixCells))
    yield kind match
      case StructureKind.Controllable | StructureKind.Observable => _Bool(fullRowRank(m))
      case StructureKind.Ctrb                                     => m
      case StructureKind.Obsv                                     => m.transpose
    answer.toRight(this)


/** The monic polynomial with the given roots, highest power first, as `(re, im)` pairs. */
private def polyFromRoots(roots: Vector[(Double, Double)]): Vector[(Double, Double)] =
  roots.foldLeft(Vector((1.0, 0.0))) { (p, r) =>
    // (s − r)·p: each new coefficient is p(i) − r·p(i − 1).
    Vector.tabulate(p.size + 1) { i =>
      val (a, b) = if i < p.size then p(i) else (0.0, 0.0)
      val (c, d) = if i > 0 then p(i - 1) else (0.0, 0.0)
      (a - (r._1 * c - r._2 * d), b - (r._1 * d + r._2 * c))
    }
  }

/** Single-input pole placement by Ackermann's formula (F_0138): the gain `K` that gives
 *  `A − B·K` the eigenvalues `poles`, in the `u = −K·x` convention `dlqr` uses.
 *
 *  `K = eₙᵀ · C⁻¹ · φ(A)`, with `C` the controllability matrix and `φ` the monic polynomial whose
 *  roots are the poles.  **Certified**: Ackermann's formula is badly conditioned as `n` grows, so
 *  the eigenvalues of `A − B·K` are compared with the requested poles, each within `1e-6` of the
 *  problem's own scale (the largest of `‖A‖` and the poles' moduli), and a gain that misses is
 *  declined rather than returned.  Floating point: an exact operand is demoted, the `expm`
 *  precedent.
 *
 *  @param a     the state matrix, `n×n`
 *  @param b     the input matrix, `n×1`
 *  @param poles the requested closed-loop poles, `n` of them, closed under conjugation
 *  @return `K` as a `1×n` row, or `None` for a multi-input or uncontrollable plant, poles not
 *          closed under conjugation or not `n` of them, or a gain that fails the certificate
 */
def acker(a: _MatrixValue, b: _MatrixValue, poles: Vector[(Double, Double)]): Option[_MatrixValue] =
  val n = a.rows
  val coeffs = polyFromRoots(poles)
  val top    = coeffs.map((re, im) => math.hypot(re, im)).max
  for
    _    <- Option.when(a.cols == n && b.rows == n && b.cols == 1 && poles.size == n && a.isFinite && b.isFinite)(())
    // Conjugate closure is exactly the coefficients coming out real.
    _    <- Option.when(coeffs.forall((_, im) => math.abs(im) <= 1e-9 * top))(())
    ctrb <- ctrbOf(a, b)
    if fullRowRank(ctrb)
    inv  <- ctrb.inverse
    phi   = coeffs.drop(1).foldLeft(_MatrixValue.identity(n))((m, c) => m.multiply(a).add(_MatrixValue.identity(n).scale(c._1)))
    k     = _MatrixValue(1, n, Array.tabulate(n)(j => inv(n - 1, j))).multiply(phi)
    eig  <- a.add(b.multiply(k).scale(-1.0)).eigenDecompose
    if polesPlaced(eig, poles, a)
  yield k

/** Whether `eig` matches `poles` one to one, each within `1e-6` of the problem's scale. */
private def polesPlaced(eig: Vector[_Value], poles: Vector[(Double, Double)], a: _MatrixValue): Boolean =
  val scale = math.max(math.sqrt(a.toVector.map(x => x * x).sum), poles.map((re, im) => math.hypot(re, im)).max)
  val found = eig.flatMap(v => _Complex.parts(v))
  found.size == poles.size && found.foldLeft(Option(poles)) { (left, e) =>
    left.flatMap { ps =>
      val (i, d) = ps.zipWithIndex.map((p, j) => (j, math.hypot(p._1 - e._1, p._2 - e._2))).minBy(_._2)
      Option.when(d <= 1e-6 * scale)(ps.patch(i, Nil, 1))
    }
  }.exists(_.isEmpty)


/** `acker(A, B, poles)` — [[acker]] at the prompt (F_0138): the poles a row (or column) of real
 *  and complex values; the answer the `1×n` gain.  `acker`, MATLAB's name for the single-input
 *  formula, rather than `place`: it says which method answers, and it is not a plausible
 *  variable name to reserve.
 *
 *  @param a     the state matrix
 *  @param b     the input column
 *  @param poles the requested closed-loop poles
 */
case class _Acker(a: _Expression, b: _Expression, poles: _Expression) extends _Expression:
  override def toString: String = s"acker($a, $b, $poles)"
  override def children: List[_Expression] = List(a, b, poles)
  override def rebuild(c: List[_Expression]): _Expression = _Acker(c.head, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ra, rb, rp) = (a.eval(env), b.eval(env), poles.eval(env))
    val roots: Option[Vector[(Double, Double)]] = rp match
      case Right(m: _MatrixValue) if m.rows == 1 || m.cols == 1 => Some(m.toVector.map(d => (d, 0.0)))
      case Left(m: _Matrix) if m.rows == 1 || m.cols == 1 =>
        val parts = m.elems.collect { case v: _Value => _Complex.parts(v) }.flatten
        Option.when(parts.size == m.elems.size)(parts)
      case _ => None
    val k = for
      am <- matrixOperand(ra, None)
      bm <- matrixOperand(rb, None)
      rs <- roots
      k  <- acker(am, bm, rs)
    yield k
    k.toRight(_Acker(ra.toExpression, rb.toExpression, rp.toExpression))


/** **Exact** state-space discretisation over one sample period: `A_d = e^(A*Ts)`.
 *
 *  **Uses the block-matrix form, not `B_d = A⁻¹(A_d − I)B`, and that is a correctness matter
 *  rather than tidiness.**  Exponentiating `[[A, B], [0, 0]]*Ts` yields `[[A_d, B_d], [0, I]]`
 *  in one step, and it is defined for a **singular** `A` — where the inverse form is not, even
 *  though `B_d` exists.  A singular `A` is entirely ordinary: any system with an integrator
 *  has one, so the naive form would fail on a common case rather than an exotic one.
 *
 *  This is what the matrix exponential exists to supply, and what a Kalman filter needs.
 *
 *  @param a  the continuous state matrix
 *  @param b  the continuous input matrix
 *  @param ts the sample period
 *  @return `(A_d, B_d)`, or `None` when the operands are not conforming dense matrices
 */
def c2dExact(a: _Expression, b: _Expression, ts: Double): Option[(_MatrixValue, _MatrixValue)] =
  // The block assembly lives in core (F_0006) so the ode system tier shares it: the two
  // cannot disagree about the augmented exponential, and ode need not import this leaf.
  for
    am     <- denseOf(a); bm <- denseOf(b)
    result <- _MatrixValue.augmentedExp(am, bm, ts)
  yield result
