package it.grypho.scala.leonardo
package control

import core.*
import matrix.{numericalRank, rankTolerance, subspaceBasis, Subspace}


// Offset-free MPC (F_0300, AIACE L_0003).
//
// A model identified on a nonlinear plant leaves a steady-state offset unless the mismatch is
// estimated.  The plant is augmented with integrating disturbance states `d(k+1) = d(k)` entering
// through `Bd` and `Cd` (the owner's decision: a general model, with helpers for the two standard
// ones), estimated with the state by a Kalman filter on the augmented model, and the steady-state
// target `(x_s, u_s)` is computed for the setpoint and the current disturbance estimate.

/** A plant augmented with disturbance states (F_0300): `x̂ = [x; d]`,
 *  `x̂(k+1) = [[A, Bd], [0, I]]·x̂ + [[B], [0]]·u`, `y = [C, Cd]·x̂`.
 *
 *  @param a  the augmented state matrix, `(n+nd)×(n+nd)`
 *  @param b  the augmented input matrix, `(n+nd)×m`
 *  @param c  the augmented output matrix, `p×(n+nd)`
 *  @param n  the plant's states
 *  @param nd the disturbance states
 */
final case class DisturbanceModel(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, n: Int, nd: Int)

/** The steady-state target (F_0300).
 *
 *  @param x       `x_s`
 *  @param u       `u_s`
 *  @param reached whether the outputs reach the setpoint there; `false` when the input bounds or the
 *                 shape leave only the closest reachable target
 */
final case class SteadyStateTarget(x: Vector[Double], u: Vector[Double], reached: Boolean)

/** The plant `(A, B, C)` augmented with disturbance states entering through `Bd`, `Cd` (F_0300).
 *
 *  **Admissible only when the augmented pair is detectable** (Pannocchia and Rawlings): `(A, C)`
 *  detectable — `A` restricted to the unobservable subspace, the null space of the observability matrix
 *  read by the SVD at the library's rank tolerance, has every eigenvalue strictly inside the unit circle —
 *  and `[[A − I, Bd], [C, Cd]]` of full column rank `n + nd`, so `nd ≤ p`.  Both are rank questions
 *  answered by singular values relative to the largest, never by a determinant (the 2.11 rule).
 *
 *  @param a      the state matrix, `n×n`
 *  @param b      the input matrix, `n×m`
 *  @param c      the output matrix, `p×n`
 *  @param bd     the disturbance's entry into the state, `n×nd`
 *  @param cd     the disturbance's entry into the output, `p×nd`
 *  @param env    the precision that sets the rank tolerance, and the budgets
 *  @return the augmented model, or why the disturbance model is not admissible
 */
def augmentDisturbance(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, bd: _MatrixValue, cd: _MatrixValue,
                       env: Environment): Either[MpcFailure, DisturbanceModel] =
  val (n, m, p, nd) = (a.rows, b.cols, c.rows, bd.cols)
  def bad(d: String) = Left(MpcFailure.InvalidInput(d))
  if a.cols != n || b.rows != n || c.cols != n then bad("A must be n x n, B n x m and C p x n")
  else if bd.rows != n || cd.rows != p || cd.cols != nd then bad(s"Bd must be ${n}x$nd and Cd ${p}x$nd")
  else if !Seq(a, b, c, bd, cd).forall(_.isFinite) then bad("every entry must be finite")
  else if nd > p then Left(MpcFailure.NotDetectable(s"$nd disturbances cannot be told apart through $p outputs"))
  else
    pairDetectable(a, c, env).flatMap { _ =>
      val m1 = Dense.blocks(Vector(Vector(a.add(_MatrixValue.identity(n).scale(-1.0)), bd), Vector(c, cd)))
      fullColumnRank(m1, env).flatMap { full =>
        if !full then Left(MpcFailure.NotDetectable("[[A - I, Bd], [C, Cd]] does not have full column rank"))
        else
          val aAug = Dense.blocks(Vector(Vector(a, bd), Vector(Dense.zeros(nd, n), _MatrixValue.identity(nd))))
          val bAug = Dense.blocks(Vector(Vector(b), Vector(Dense.zeros(nd, m))))
          Right(DisturbanceModel(aAug, bAug, Dense.blocks(Vector(Vector(c, cd))), n, nd))
      }
    }

/** The input-disturbance model (F_0300): `Bd = B`, `Cd = 0`, one disturbance per input.
 *  @param a   the state matrix
 *  @param b   the input matrix
 *  @param c   the output matrix
 *  @param env the precision and budgets
 *  @return the augmented model, or why not */
def inputDisturbanceModel(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, env: Environment): Either[MpcFailure, DisturbanceModel] =
  augmentDisturbance(a, b, c, b, Dense.zeros(c.rows, b.cols), env)

/** The output-disturbance model (F_0300): `Bd = 0`, `Cd = I`, one disturbance per output.
 *  @param a   the state matrix
 *  @param b   the input matrix
 *  @param c   the output matrix
 *  @param env the precision and budgets
 *  @return the augmented model, or why not */
def outputDisturbanceModel(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, env: Environment): Either[MpcFailure, DisturbanceModel] =
  augmentDisturbance(a, b, c, Dense.zeros(a.rows, c.rows), _MatrixValue.identity(c.rows), env)

/** The steady-state Kalman gain of the augmented model (F_0300), through [[kalmanGain]].
 *  @param model the augmented model
 *  @param qw    the process noise covariance of the augmented state, `(n+nd)×(n+nd)`
 *  @param rv    the measurement noise covariance, `p×p`
 *  @param env   the precision of the certificate
 *  @return `(M, P)`, as [[kalmanGain]] answers, or why not */
def disturbanceObserver(model: DisturbanceModel, qw: _MatrixValue, rv: _MatrixValue,
                        env: Environment): Either[MpcFailure, (_MatrixValue, _MatrixValue)] =
  kalmanGain(model.a, model.c, qw, rv, env).left.map(MpcFailure.Riccati(_))

/** The steady-state target `(x_s, u_s)` for the setpoint `r` and the disturbance estimate `d` (F_0300):
 *  `x_s = A·x_s + B·u_s + Bd·d` and `C·x_s + Cd·d = r`.
 *
 *  Solved directly when `[[A − I, B], [C, 0]]` is square and invertible and the answer lies within the
 *  input bounds; otherwise by minimising `‖C·x_s + Cd·d − r‖²` under the steady state equation and
 *  the bounds, so the closest reachable target is returned, `reached` saying
 *  whether it meets the setpoint (each output to `0.5·10⁻ᵖ` of its own terms).  That path is the
 *  one-level case of [[steadyStateTargets]] (F_0358): where several targets meet the setpoint (more
 *  inputs than outputs) the one with the smallest input, each input in its own scale, is chosen;
 *  resting values, priorities and output bounds are that function's.
 *
 *  @param a       the state matrix, `n×n`
 *  @param b       the input matrix, `n×m`
 *  @param c       the output matrix, `p×n`
 *  @param bd      the disturbance's entry into the state, `n×nd`
 *  @param cd      the disturbance's entry into the output, `p×nd`
 *  @param r       the setpoint, `p` entries
 *  @param d       the disturbance estimate, `nd` entries
 *  @param uBounds `(lower, upper)` on `u_s`, `m` entries each, or `None`
 *  @param env     the precision and budgets
 *  @return the target, or why there is none
 */
def steadyStateTarget(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, bd: _MatrixValue, cd: _MatrixValue,
                      r: Vector[Double], d: Vector[Double], uBounds: Option[(Vector[Double], Vector[Double])],
                      env: Environment): Either[MpcFailure, SteadyStateTarget] =
  val (n, m, p, nd) = (a.rows, b.cols, c.rows, bd.cols)
  def bad(s: String) = Left(MpcFailure.InvalidInput(s))
  if a.cols != n || b.rows != n || c.cols != n || bd.rows != n || cd.rows != p || cd.cols != nd then
    bad("the shapes of A, B, C, Bd and Cd do not conform")
  else if r.size != p || d.size != nd then bad(s"the setpoint needs $p entries and the disturbance $nd")
  else if uBounds.exists((lo, hi) => lo.size != m || hi.size != m || lo.zip(hi).exists((l, h) => !(l <= h))) then
    bad(s"the input bounds need $m entries each, lower below upper")
  else if !(Seq(a, b, c, bd, cd).forall(_.isFinite) && (r ++ d).forall(_.isFinite)) then bad("every entry must be finite")
  else
    val bdd = Dense.times(bd, d)
    val cdd = Dense.times(cd, d)
    val tol = 0.5 * math.pow(10, -env.precision)
    def inBounds(u: Vector[Double]) = uBounds.forall((lo, hi) => u.indices.forall(k => lo(k) <= u(k) && u(k) <= hi(k)))
    def reachedAt(x: Vector[Double]): Boolean =
      (0 until p).forall { i =>
        val terms = (0 until n).map(j => math.abs(c(i, j) * x(j))).sum + math.abs(cdd(i)) + math.abs(r(i))
        math.abs((0 until n).map(j => c(i, j) * x(j)).sum + cdd(i) - r(i)) <= tol * math.max(terms, Double.MinPositiveValue)
      }
    val iMinusA = _MatrixValue.identity(n).add(a.scale(-1.0))
    val e = Dense.blocks(Vector(Vector(iMinusA.scale(-1.0), b), Vector(c, Dense.zeros(p, m))))
    val rhs = bdd.map(-_) ++ r.zip(cdd).map(_ - _)
    val direct = Option.when(n + p == n + m)(e.inverse).flatten.map(inv => Dense.times(inv, rhs))
      .filter(w => inBounds(w.drop(n)))
    direct match
      case Some(w) => Right(SteadyStateTarget(w.take(n), w.drop(n), reachedAt(w.take(n))))
      case None =>
        // min ‖C·x + Cd·d − r‖², the one-level case of the target layer (F_0358, Decision A1): among
        // several targets that meet the setpoint, the smallest input in each input's own scale
        val spec = TargetSpec(Vector(Vector.tabulate(p)(i => TargetGoal.Setpoint(i, r(i)))))
        steadyStateTargets(StateSpaceSteadyState(a, b, c, bd, cd, d), spec, uBounds, Vector.fill(m)(0.0), env).map { t =>
          val x = t.x.getOrElse(Vector.empty)
          SteadyStateTarget(x, t.u, reachedAt(x))
        }

/** `(A, C)` detectable: the unobservable part of `A` strictly inside the unit circle. */
private def pairDetectable(a: _MatrixValue, c: _MatrixValue, env: Environment): Either[MpcFailure, Unit] =
  val n = a.rows
  val obs = Dense.blocks((0 until n).scanLeft(c)((ck, _) => ck.multiply(a)).take(n).map(Vector(_)).toVector)
  subspaceBasis(obs, Subspace.Null, rankTolerance(env), env.limits) match
    case None          => Left(MpcFailure.InvalidInput("the observability matrix could not be decomposed"))
    case Some(None)    => Right(())                                     // observable
    case Some(Some(nb)) =>
      nb.transpose.multiply(a).multiply(nb).eigenDecompose match
        case None => Left(MpcFailure.InvalidInput("the unobservable part's eigenvalues could not be computed"))
        case Some(lambdas) =>
          val unstable = lambdas.flatMap(l => _Complex.parts(l)).filter((re, im) => math.hypot(re, im) >= 1.0)
          if unstable.isEmpty then Right(())
          else Left(MpcFailure.NotDetectable("(A, C) has an unobservable mode on or outside the unit circle"))

/** Whether `mat` has full column rank at the library's rank tolerance. */
private def fullColumnRank(mat: _MatrixValue, env: Environment): Either[MpcFailure, Boolean] =
  if mat.rows < mat.cols then Right(false)
  else mat.svd(env.limits) match
    case Some((_, sigma, _)) => Right(numericalRank(sigma, rankTolerance(env)) == mat.cols)
    case None                => Left(MpcFailure.InvalidInput("the rank test could not be computed"))
