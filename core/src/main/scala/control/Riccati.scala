package it.grypho.scala.leonardo
package control

import core.*
import matrix._Matrix


/** Why a Riccati-family call declined (F_0052 Decision A: the reason travels as data, the REPL
 *  prints it). */
enum RiccatiFailure:
  /** The operands do not conform; `detail` says how. */
  case ShapeMismatch(detail: String)
  /** An operand is not a numeric matrix; `what` names it. */
  case NotNumeric(what: String)
  /** A matrix that must be symmetric is not, relative to its largest entry. */
  case NotSymmetric(what: String)
  /** `R` (or `Rv`, or `C·P·Cᵀ + Rv`) is not positive definite, or `Q` (or `Qw`) not positive
   *  semidefinite. */
  case NotDefinite(what: String)
  /** No stabilising solution exists: `(A, B)` is not stabilisable, `(A, Q^½)` has an unobservable
   *  mode on the unit circle, or (for `dlyap`) `A` is not stable. Only the stabilising solution is
   *  answered, so a solution of the equation that leaves `A − B·K` unstable lands here too. */
  case NoStabilisingSolution
  /** The iteration ended, but its answer failed the residual certificate (X-02). */
  case Uncertified(residual: Double)
  /** The state dimension `n` is past [[MaxRiccatiOrder]]. */
  case TooLarge(n: Int)

import RiccatiFailure.*


/** The largest state dimension the Riccati family solves (F_0065).
 *
 *  **Measured** (2026-10-11), `dlqr` on a random stabilisable plant with three inputs, JVM / Node:
 *  `n = 50` 48 / 104 ms, `n = 100` 130 / 357 ms, `n = 200` 452 ms / 2.5 s — a few `n³` products per
 *  doubling step and about ten steps. Node is the slowest platform and sets the cap (the F_0077
 *  rule); 2.5 s there matches `lsqfit`'s and `arx`'s caps.
 */
val MaxRiccatiOrder: Int = 200

/** The most doubling steps. The iteration converges quadratically, so a well-posed problem needs
 *  far fewer; past this the certificate still decides. */
private val MaxDoublingSteps = 100

/** Symmetry and definiteness are judged relative to the matrix's largest entry, never absolutely
 *  (the 2.11 rule), at the tolerance `cholesky` uses. */
private val SymmetryTolerance = 1e-12

private def frobenius(a: _MatrixValue): Double = math.sqrt(a.toVector.map(x => x * x).sum)

private def maxAbs(a: _MatrixValue): Double = a.toVector.foldLeft(0.0)((m, x) => math.max(m, math.abs(x)))

/** `(A + Aᵀ)/2`, exactly symmetric in `Double` since `a + b == b + a` (the RLS rule). */
private def symmetrised(a: _MatrixValue): _MatrixValue = a.add(a.transpose).scale(0.5)

private def isSymmetric(a: _MatrixValue): Boolean =
  a.rows == a.cols && {
    val tol = SymmetryTolerance * maxAbs(a)
    (0 until a.rows).forall(i => (0 until i).forall(j => math.abs(a(i, j) - a(j, i)) <= tol))
  }

/** Whether every eigenvalue of `a` lies strictly inside the unit circle, **proved** by a power:
 *  `ρ(A) ≤ ‖Aᵏ‖^(1/k)` in any submultiplicative norm, so `‖A^(2^j)‖_F < 1` for some `j` proves
 *  `ρ(A) < 1`. Repeated squaring reaches `2⁶⁴` steps in 64 products, and it is a *proof*:
 *  reading `|λ| < 1` off computed eigenvalues needs a tolerance at the unit circle, which a
 *  norm below 1 does not. (It was chosen while `eigenDecompose` still declined random dense
 *  matrices, F_0100, since fixed; the reason to keep it is the proof.) A matrix it cannot prove stable — the powers grow past `1e150`, or 64
 *  squarings leave the norm at or above 1 — answers `false`, so a refusal follows, never a guess.
 */
private def schurStable(a: _MatrixValue): Boolean =
  @annotation.tailrec
  def go(m: _MatrixValue, j: Int): Boolean =
    val f = frobenius(m)
    if f < 1.0 then true
    else if j >= 64 || !m.isFinite || f > 1e150 then false
    else go(m.multiply(m), j + 1)
  go(a, 0)

/** A symmetric positive-semidefinite test: `Q + δ·I` has a Cholesky factor for `δ` relative to
 *  `Q`'s largest entry (the F_0054 test, shifted by the tolerance `cholesky` itself uses). A
 *  negative eigenvalue below `−δ` makes the factorisation fail; no eigenvalue iteration is run. */
private def semidefinite(a: _MatrixValue): Boolean =
  val scale = maxAbs(a)
  scale == 0.0 || a.add(_MatrixValue.identity(a.rows).scale(1e-10 * scale)).cholesky.isDefined

private def checkSquare(a: _MatrixValue, what: String, n: Int): Either[RiccatiFailure, Unit] =
  Either.cond(a.rows == n && a.cols == n, (), ShapeMismatch(s"$what must be ${n}x$n: it is ${a.rows}x${a.cols}"))

/** The weights of a Riccati problem, checked: `Q` symmetric positive semidefinite `n×n`, `R`
 *  symmetric positive definite `m×m` (its Cholesky factor existing is the F_0054 test). */
private def checkWeights(q: _MatrixValue, r: _MatrixValue, n: Int, m: Int, qName: String, rName: String)
    : Either[RiccatiFailure, Unit] =
  for
    _ <- checkSquare(q, qName, n)
    _ <- checkSquare(r, rName, m)
    _ <- Either.cond(isSymmetric(q), (), NotSymmetric(qName))
    _ <- Either.cond(isSymmetric(r), (), NotSymmetric(rName))
    _ <- Either.cond(r.cholesky.isDefined, (), NotDefinite(rName))
    _ <- Either.cond(semidefinite(q), (), NotDefinite(qName))
  yield ()

/** The structure-preserving doubling iteration (Chu, Fan & Lin 2005) for
 *  `X = AᵀX(I + G·X)⁻¹A + H`, the DARE when `G = B·R⁻¹·Bᵀ` and `H = Q`, and the Stein equation
 *  `X = AᵀXA + H` when `G = 0` (Smith's method). Only products and inverses: no generalised
 *  eigenproblem, which the library does not have. Converges quadratically when a stabilising
 *  solution exists; `None` when a step is singular or leaves the finite numbers.
 */
private def doubling(a0: _MatrixValue, g0: _MatrixValue, h0: _MatrixValue): Option[_MatrixValue] =
  val n  = a0.rows
  val id = _MatrixValue.identity(n)
  @annotation.tailrec
  def go(a: _MatrixValue, g: _MatrixValue, h: _MatrixValue, step: Int): Option[_MatrixValue] =
    id.add(g.multiply(h)).inverse match
      case None => None
      case Some(wInv) =>
        val aw = a.multiply(wInv)
        val a1 = aw.multiply(a)
        val g1 = g.add(aw.multiply(g).multiply(a.transpose))
        val h1 = h.add(a.transpose.multiply(h).multiply(wInv).multiply(a))
        if !(a1.isFinite && g1.isFinite && h1.isFinite) then None
        else
          val change = frobenius(h1.add(h.scale(-1)))
          if change <= 1e-14 * frobenius(h1) || step >= MaxDoublingSteps then Some(symmetrised(h1))
          else go(a1, g1, h1, step + 1)
  go(a0, g0, h0, 1)

/** The stabilising solution `P` of the discrete algebraic Riccati equation
 *  `AᵀPA − P − AᵀPB(R + BᵀPB)⁻¹BᵀPA + Q = 0` (F_0065, MPC T3-01).
 *
 *  Solved by structure-preserving doubling, then **certified** (X-02) twice:
 *  - the residual, relative to the larger of `‖Q‖`, `‖AᵀPA‖` and `‖P‖`, against `0.5·10⁻ᵖ`;
 *  - the closed loop `A − B·K` with every eigenvalue strictly inside the unit circle.
 *
 *  The second test is what declines a problem with **no stabilising solution** — `(A, B)` not
 *  stabilisable — and one whose `(A, Q^½)` is **not detectable**, as T3-01 asks. A solution of the
 *  equation that does not stabilise is never answered. For the second case the refusal comes from
 *  the iteration itself: it starts at `H₀ = Q`, and an unstable mode `Q` does not penalise stays
 *  at `0` there (`a = 2, b = 1, q = 0, r = 1` gives `p = 0`, not the stabilising `p = 3`), which
 *  the closed-loop test then refuses.
 *
 *  @param a   the state matrix, `n×n`
 *  @param b   the input matrix, `n×m`
 *  @param q   the state weight, symmetric positive semidefinite `n×n`
 *  @param r   the input weight, symmetric positive definite `m×m`
 *  @param env the evaluation environment, whose precision sets the certificate
 *  @return `P`, symmetric, or why there is none
 */
def dare(a: _MatrixValue, b: _MatrixValue, q: _MatrixValue, r: _MatrixValue, env: Environment)
    : Either[RiccatiFailure, _MatrixValue] =
  dlqr(a, b, q, r, env).map(_._2)

/** The infinite-horizon discrete LQR: the gain `K` of the law **`u = −K·x`** minimising
 *  `Σ xᵀQx + uᵀRu`, and the Riccati solution `P` (F_0065).
 *
 *  `K = (R + BᵀPB)⁻¹BᵀPA`, so the closed loop is `A − B·K`. The sign is stated because the literature
 *  also writes `u = K·x` (the `feedback` lesson: a convention that cannot be inferred is written
 *  down). See [[dare]] for the solver, the certificate and the refusals.
 *
 *  @param a   the state matrix, `n×n`
 *  @param b   the input matrix, `n×m`
 *  @param q   the state weight, symmetric positive semidefinite `n×n`
 *  @param r   the input weight, symmetric positive definite `m×m`
 *  @param env the evaluation environment, whose precision sets the certificate
 *  @return `(K, P)`, or why there is none
 */
def dlqr(a: _MatrixValue, b: _MatrixValue, q: _MatrixValue, r: _MatrixValue, env: Environment)
    : Either[RiccatiFailure, (_MatrixValue, _MatrixValue)] =
  val n = a.rows
  for
    _    <- checkSquare(a, "A", n)
    _    <- Either.cond(n <= MaxRiccatiOrder, (), TooLarge(n))
    _    <- Either.cond(b.rows == n, (), ShapeMismatch(s"B must have $n rows, as A does: it is ${b.rows}x${b.cols}"))
    _    <- Either.cond(a.isFinite && b.isFinite && q.isFinite && r.isFinite, (), NotNumeric("an entry is not finite"))
    _    <- checkWeights(q, r, n, b.cols, "Q", "R")
    rInv <- r.inverse.toRight(NotDefinite("R"))
    p    <- doubling(a, symmetrised(b.multiply(rInv).multiply(b.transpose)), q).toRight(NoStabilisingSolution)
    s    <- r.add(b.transpose.multiply(p).multiply(b)).inverse.toRight(NoStabilisingSolution)
    k     = s.multiply(b.transpose).multiply(p).multiply(a)
    _    <- Either.cond(schurStable(a.add(b.multiply(k).scale(-1))), (), NoStabilisingSolution)
    _    <- certified(a, b, q, r, p, s, env)
  yield (k, p)

/** The Riccati residual, relative to the equation's own scale, against the `=` tolerance. */
private def certified(a: _MatrixValue, b: _MatrixValue, q: _MatrixValue, r: _MatrixValue, p: _MatrixValue,
                      s: _MatrixValue, env: Environment): Either[RiccatiFailure, Unit] =
  val at   = a.transpose
  val apa  = at.multiply(p).multiply(a)
  val apb  = at.multiply(p).multiply(b)
  val res  = apa.add(q).add(p.scale(-1)).add(apb.multiply(s).multiply(apb.transpose).scale(-1))
  val size = math.max(math.max(frobenius(q), frobenius(apa)), math.max(frobenius(p), Double.MinPositiveValue))
  val rel  = frobenius(res) / size
  Either.cond(rel <= 0.5 * math.pow(10, -env.precision), (), Uncertified(rel))

/** The solution `X` of the Stein (discrete Lyapunov) equation `A·X·Aᵀ − X + Q = 0` (F_0065) —
 *  the requirement's `dlyap`, named sugar over what `solve`'s vectorized tier already answers.
 *
 *  Solved by the same doubling with `G = 0` (Smith's method), so its cost is `log` of the decay
 *  rather than `n²`-sized vectorisation. Requires `A` **stable** (every eigenvalue strictly inside
 *  the unit circle), the condition for the series `Σ AᵏQ(Aᵀ)ᵏ` to converge; declined otherwise.
 *  `Q` need only be square: the equation is linear in it.
 *
 *  @param a   the matrix, `n×n`
 *  @param q   the right-hand side, `n×n`
 *  @param env the evaluation environment, whose precision sets the certificate
 *  @return `X`, or why there is none
 */
def dlyap(a: _MatrixValue, q: _MatrixValue, env: Environment): Either[RiccatiFailure, _MatrixValue] =
  val n = a.rows
  for
    _ <- checkSquare(a, "A", n)
    _ <- checkSquare(q, "Q", n)
    _ <- Either.cond(n <= MaxRiccatiOrder, (), TooLarge(n))
    _ <- Either.cond(a.isFinite && q.isFinite, (), NotNumeric("an entry is not finite"))
    _ <- Either.cond(schurStable(a), (), NoStabilisingSolution)
    // doubling solves X = AᵀXA + H; the Stein equation in A is that equation in Aᵀ.
    x <- doubling(a.transpose, _MatrixValue(n, n, new Array[Double](n * n)), q).toRight(NoStabilisingSolution)
    res  = a.multiply(x).multiply(a.transpose).add(q).add(x.scale(-1))
    rel  = frobenius(res) / math.max(math.max(frobenius(x), frobenius(q)), Double.MinPositiveValue)
    _ <- Either.cond(rel <= 0.5 * math.pow(10, -env.precision), (), Uncertified(rel))
  yield if isSymmetric(q) then symmetrised(x) else x

/** The steady-state Kalman filter for `x[k+1] = A·x + B·u + w`, `y = C·x + D·u + v`,
 *  `w ~ N(0, Qw)`, `v ~ N(0, Rv)`: the measurement-update gain `M` and the a priori covariance
 *  `P` (F_0065).
 *
 *  `P` solves the **dual** Riccati equation, `dare(Aᵀ, Cᵀ, Qw, Rv)`, and
 *  `M = P·Cᵀ(C·P·Cᵀ + Rv)⁻¹` is the gain [[kalmanStep]]'s update applies. `P` is returned as well
 *  as `M`, as [[dlqr]] returns its `P`: it is what starts `kalmanStep` at steady state.
 *
 *  @param a   the state matrix, `n×n`
 *  @param c   the output matrix, `p×n`
 *  @param qw  the process noise covariance, symmetric positive semidefinite `n×n`
 *  @param rv  the measurement noise covariance, symmetric positive definite `p×p`
 *  @param env the evaluation environment, whose precision sets the certificate
 *  @return `(M, P)`, or why there is none
 */
def kalmanGain(a: _MatrixValue, c: _MatrixValue, qw: _MatrixValue, rv: _MatrixValue, env: Environment)
    : Either[RiccatiFailure, (_MatrixValue, _MatrixValue)] =
  for
    _ <- Either.cond(c.cols == a.rows, (), ShapeMismatch(s"C must have ${a.rows} columns, as A does: it is ${c.rows}x${c.cols}"))
    p <- dare(a.transpose, c.transpose, qw, rv, env).left.map {
           case NotDefinite("Q")  => NotDefinite("Qw")
           case NotDefinite("R")  => NotDefinite("Rv")
           case NotSymmetric("Q") => NotSymmetric("Qw")
           case NotSymmetric("R") => NotSymmetric("Rv")
           case other             => other
         }
    s <- c.multiply(p).multiply(c.transpose).add(rv).inverse.toRight(NotDefinite("C·P·Cᵀ + Rv"))
  yield (p.multiply(c.transpose).multiply(s), p)

/** One step of the discrete Kalman filter: **update, then predict** (F_0065, MPC T3-01).
 *
 *  From the prior `x̂`, `P` for step `k` and that step's `u`, `y`: the innovation
 *  `y − C·x̂ − D·u`, the gain `M = P·Cᵀ(C·P·Cᵀ + Rv)⁻¹`, the posterior `x̂⁺ = x̂ + M·(innovation)`
 *  with the **Joseph-form** covariance `(I − M·C)·P·(I − M·C)ᵀ + M·Rv·Mᵀ`, which stays symmetric
 *  positive semidefinite under rounding where `(I − M·C)·P` does not; then the prediction
 *  `x̂' = A·x̂⁺ + B·u`, `P' = A·P⁺·Aᵀ + Qw`. Both covariances are symmetrised, the RLS rule.
 *
 *  Returns the **prior for step `k + 1`**, so the result feeds back as the next call's input:
 *  state in, state out (X-04), nothing remembered.
 *
 *  @param xhat the prior state estimate, `n×1`
 *  @param p    the prior error covariance, `n×n`
 *  @param u    the input at this step, `m×1`
 *  @param y    the measurement at this step, `p×1`
 *  @param a    the state matrix, `n×n`
 *  @param b    the input matrix, `n×m`
 *  @param c    the output matrix, `p×n`
 *  @param d    the feedthrough matrix, `p×m`
 *  @param qw   the process noise covariance, `n×n`
 *  @param rv   the measurement noise covariance, `p×p`
 *  @return `(x̂', P')`, or why the step cannot be taken
 */
def kalmanStep(xhat: _MatrixValue, p: _MatrixValue, u: _MatrixValue, y: _MatrixValue,
               a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, d: _MatrixValue,
               qw: _MatrixValue, rv: _MatrixValue): Either[RiccatiFailure, (_MatrixValue, _MatrixValue)] =
  val (n, m, q) = (a.rows, b.cols, c.rows)
  val shapes = List(
    (a, n, n, "A"), (b, n, m, "B"), (c, q, n, "C"), (d, q, m, "D"), (xhat, n, 1, "xhat"),
    (p, n, n, "P"), (u, m, 1, "u"), (y, q, 1, "y"), (qw, n, n, "Qw"), (rv, q, q, "Rv"))
  shapes.collectFirst { case (mat, rows, cols, what) if mat.rows != rows || mat.cols != cols =>
    ShapeMismatch(s"$what must be ${rows}x$cols: it is ${mat.rows}x${mat.cols}")
  } match
    case Some(f) => Left(f)
    case None =>
      for
        s <- c.multiply(p).multiply(c.transpose).add(rv).inverse.toRight(NotDefinite("C·P·Cᵀ + Rv"))
        gain  = p.multiply(c.transpose).multiply(s)
        innov = y.add(c.multiply(xhat).scale(-1)).add(d.multiply(u).scale(-1))
        xPost = xhat.add(gain.multiply(innov))
        imc   = _MatrixValue.identity(n).add(gain.multiply(c).scale(-1))
        pPost = symmetrised(imc.multiply(p).multiply(imc.transpose).add(gain.multiply(rv).multiply(gain.transpose)))
        xNext = a.multiply(xPost).add(b.multiply(u))
        pNext = symmetrised(a.multiply(pPost).multiply(a.transpose).add(qw))
        _ <- Either.cond(xNext.isFinite && pNext.isFinite, (), NotNumeric("the step left the finite numbers"))
      yield (xNext, pNext)


/** Which Riccati-family function a [[_Riccati]] node is. */
enum RiccatiKind:
  case Dare, Dlqr, Dlyap, KalmanGain, KalmanStep

object RiccatiKind:
  /** The grammar keyword.
   *  @param k the kind
   *  @return its spelling at the prompt */
  def keyword(k: RiccatiKind): String = k match
    case Dare       => "dare"
    case Dlqr       => "dlqr"
    case Dlyap      => "dlyap"
    case KalmanGain => "kalmanGain"
    case KalmanStep => "kalmanStep"

/** `dare(A, B, Q, R)`, `dlqr(A, B, Q, R)`, `dlyap(A, Q)`, `kalmanGain(A, C, Qw, Rv)` and
 *  `kalmanStep(xhat, P, u, y, sys, Qw, Rv)` as one node (F_0065), the `_DistributionQuery`
 *  pattern: the five differ only in the function they call.
 *
 *  `dare` and `dlyap` answer a matrix; `dlqr` the row `[[K, P]]`, `kalmanGain` `[[M, P]]` and
 *  `kalmanStep` `[[xhat', P']]`, so tuple assignment binds them. A number is a `1×1` matrix, and a
 *  bare `0` is the zero block of the shape the call implies (`Qw`, `D`, `u`), the `dlsim` reading;
 *  `sys` is the `[[A, B, C, D]]` row `stateSpace`, `linearize` and `arx2ss` build. An exact
 *  operand demotes. A refusal leaves the call as written, and [[detailed]] carries the reason.
 *
 *  @param kind the function
 *  @param args its arguments, in the order the grammar takes them
 */
case class _Riccati(kind: RiccatiKind, args: List[_Expression]) extends _Expression:
  override def children: List[_Expression] = args
  override def rebuild(c: List[_Expression]): _Expression = _Riccati(kind, c)
  override def toString: String = s"${RiccatiKind.keyword(kind)}(${args.mkString(", ")})"

  /** The answer, or why there is none.
   *  @param env the evaluation environment
   *  @return a matrix (`dare`, `dlyap`) or a pair (the row forms), or the reason */
  def detailed(env: Environment): Either[RiccatiFailure, Either[_MatrixValue, (_MatrixValue, _MatrixValue)]] =
    val ev = args.map(_.eval(env))
    def mat(i: Int, what: String, zero: => Option[(Int, Int)] = None): Either[RiccatiFailure, _MatrixValue] =
      matrixOperand(ev(i), zero).toRight(NotNumeric(s"$what is not a numeric matrix"))
    kind match
      case RiccatiKind.Dare | RiccatiKind.Dlqr =>
        for
          a <- mat(0, "A"); b <- mat(1, "B")
          q <- mat(2, "Q", Some((a.rows, a.rows))); r <- mat(3, "R")
          ans <- dlqr(a, b, q, r, env)
        yield if kind == RiccatiKind.Dare then Left(ans._2) else Right(ans)
      case RiccatiKind.Dlyap =>
        for
          a <- mat(0, "A"); q <- mat(1, "Q", Some((a.rows, a.rows)))
          x <- dlyap(a, q, env)
        yield Left(x)
      case RiccatiKind.KalmanGain =>
        for
          a <- mat(0, "A"); c <- mat(1, "C")
          qw <- mat(2, "Qw", Some((a.rows, a.rows))); rv <- mat(3, "Rv")
          ans <- kalmanGain(a, c, qw, rv, env)
        yield Right(ans)
      case RiccatiKind.KalmanStep =>
        modelRow(args(4), env) match
          case Some(cells) =>
            def model(i: Int, what: String, zero: => Option[(Int, Int)] = None) =
              matrixOperand(cells(i), zero).toRight(NotNumeric(s"$what (in sys) is not a numeric matrix"))
            for
              a  <- model(0, "A"); b <- model(1, "B"); c <- model(2, "C")
              d  <- model(3, "D", Some((c.rows, b.cols)))
              x  <- mat(0, "xhat", Some((a.rows, 1)))
              p  <- mat(1, "P")
              u  <- mat(2, "u", Some((b.cols, 1)))
              y  <- mat(3, "y", Some((c.rows, 1)))
              qw <- mat(5, "Qw", Some((a.rows, a.rows)))
              rv <- mat(6, "Rv")
              ans <- kalmanStep(x, p, u, y, a, b, c, d, qw, rv)
            yield Right(ans)
          case _ => Left(NotNumeric("sys is not the [[A, B, C, D]] row stateSpace builds"))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env) match
      case Right(Left(m))       => Right(m)
      case Right(Right((x, y))) => Left(_Matrix(1, 2, Vector(x, y)))
      case Left(_)              => Left(this)
