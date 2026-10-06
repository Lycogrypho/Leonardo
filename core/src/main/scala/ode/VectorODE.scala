package it.grypho.scala.leonardo
package ode

import core.*


/** The integration methods of the vector tier, always named by the caller (F_0051).
 *
 *  **Never a hidden default**, the `c2d` / `minimize` rule: two methods give different answers
 *  at the same step, so a silent choice would make two correct-looking results disagree.
 */
enum OdeMethod:
  /** Explicit Euler: first order, one stage. */
  case Euler
  /** The classic fourth-order Runge–Kutta method. */
  case RK4
  /** Dormand–Prince 5(4), adaptive: the step is chosen to hold a local-error tolerance tied to
   *  the precision, and `h` is the interval between OUTPUT points, not the step. */
  case RK45

object OdeMethod:
  /** The keyword a method is written with. */
  def keyword(m: OdeMethod): String = m match
    case Euler => "euler"
    case RK4   => "rk4"
    case RK45  => "rk45"


/** How a system's state is named (F_0051 Decision A). */
enum OdeState:
  /** One name bound to the whole `n×1` column: the form for `A(t)*y` and `at(y, i, 1)`. */
  case Whole(name: _Variable)
  /** One name per component, bound to its number: `[[x], [v]]`, so a right-hand side reads as
   *  written (`[[v], [-x]]`). */
  case Components(names: Vector[_Variable])

object OdeState:
  /** The state as the grammar writes it: a bare name, or a column of names. */
  def render(s: OdeState): String = s match
    case Whole(v)       => v.variable
    case Components(vs) => vs.map(v => s"[${v.variable}]").mkString("[", ", ", "]")


/** Why the vector tier declined (F_0052 Decision A: a reason at the Scala level, which the REPL
 *  appends as a note).  The node stays symbolic; this explains it. */
enum OdeFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** The right-hand side did not evaluate to numbers at time `t`. */
  case NotEvaluable(t: Double)
  /** The right-hand side is not an `n×1` column matching the state. */
  case ShapeMismatch(rows: Int, cols: Int, n: Int)
  /** The state left the finite numbers at time `t`. */
  case NonFinite(t: Double)
  /** The adaptive step shrank below any representable change of `t` — a stiff problem or a
   *  singularity at time `t`. */
  case StepSizeUnderflow(t: Double)
  /** [[MaxOdeSteps]] were taken without reaching the end. */
  case MaxSteps(limit: Int)
  /** `ode`'s automatic step failed its certificate: `n` and `2n` steps disagree by `difference`. */
  case NotCertified(difference: Double)
  /** The trajectory would have more than [[MaxTrajectoryRows]] rows. */
  case TooManyRows(rows: Long)


/** The most steps one integration may take, fixed or adaptive.  The scalar RK4's own ceiling
 *  (`SolveODE.MaxSteps`), so the two tiers cannot run away differently. */
val MaxOdeSteps: Int = 1_000_000

/** The most rows `odeSolve` materialises.  The `Session.MaxSampleCount` rule: the trajectory is
 *  built in full before it is returned, so an uncapped `h` is an out-of-memory error rather
 *  than a refusal.  **Measured** (F_0051, JVM): 99,991 rows of a two-state RK4 trajectory take
 *  0.38 s, and one row past the cap is refused before any work. */
val MaxTrajectoryRows: Int = 100_000


/** One integration problem: `y' = rhs(t, y)` with the state named by `state`. */
private[ode] final class VectorProblem(rhs: _Expression, state: OdeState, indepVar: _Variable,
                                       env: Environment):
  /** The derivative at `(t, y)`, or why it is not available. */
  def f(t: Double, y: Vector[Double]): Either[OdeFailure, Vector[Double]] =
    val at = state match
      case OdeState.Whole(v) =>
        env.withBinding(indepVar.variable, _Number(t)).withBinding(v.variable, _MatrixValue(y.size, 1, y.toArray))
      case OdeState.Components(vs) =>
        vs.zip(y).foldLeft(env.withBinding(indepVar.variable, _Number(t))) { case (acc, (v, d)) =>
          acc.withBinding(v.variable, _Number(d))
        }
    rhs.eval(at) match
      case Right(m: _MatrixValue) =>
        if m.cols != 1 || m.rows != y.size then Left(OdeFailure.ShapeMismatch(m.rows, m.cols, y.size))
        else finite(m.toVector, t)
      case Right(_Number(d)) if y.size == 1 => finite(Vector(d), t)
      // An exact column stays a symbolic `_Matrix`; read its cells as numbers (demoted, the
      // `expm` precedent).  A non-column is a shape error, never broadcast.
      case Left(m: _MatrixShaped) =>
        if m.cols != 1 || m.rows != y.size then Left(OdeFailure.ShapeMismatch(m.rows, m.cols, y.size))
        else
          val cells = m.children.collect { case _Number(d) => d }
          if cells.size == y.size then finite(cells.toVector, t) else Left(OdeFailure.NotEvaluable(t))
      case _ => Left(OdeFailure.NotEvaluable(t))

  private def finite(v: Vector[Double], t: Double): Either[OdeFailure, Vector[Double]] =
    if v.forall(d => !d.isNaN && !d.isInfinite) then Right(v) else Left(OdeFailure.NonFinite(t))


// ── state arithmetic ──

private def axpy(a: Double, x: Vector[Double], y: Vector[Double]): Vector[Double] = x.lazyZip(y).map(a * _ + _)

/** `y + h · Σ cᵢ·kᵢ`, the combination every stage and every method needs. */
private def combine(y: Vector[Double], h: Double, terms: Seq[(Double, Vector[Double])]): Vector[Double] =
  terms.foldLeft(y) { case (acc, (c, k)) => if c == 0.0 then acc else axpy(h * c, k, acc) }


/** One explicit Euler step. */
private def eulerStep(p: VectorProblem, t: Double, y: Vector[Double], h: Double): Either[OdeFailure, Vector[Double]] =
  p.f(t, y).map(k => axpy(h, k, y))

/** One classic RK4 step — the same combination `solveODE`'s scalar step uses. */
private def rk4Step(p: VectorProblem, t: Double, y: Vector[Double], h: Double): Either[OdeFailure, Vector[Double]] =
  for
    k1 <- p.f(t, y)
    k2 <- p.f(t + h / 2, axpy(h / 2, k1, y))
    k3 <- p.f(t + h / 2, axpy(h / 2, k2, y))
    k4 <- p.f(t + h, axpy(h, k3, y))
  yield combine(y, h, Seq(1.0 / 6 -> k1, 2.0 / 6 -> k2, 2.0 / 6 -> k3, 1.0 / 6 -> k4))

/** `n` fixed steps of `method` (Euler or RK4) from `(t0, y0)` to `t1`. */
private[ode] def fixedSteps(p: VectorProblem, method: OdeMethod, t0: Double, y0: Vector[Double],
                            t1: Double, n: Int): Either[OdeFailure, Vector[Double]] =
  val h = (t1 - t0) / n
  val step: (Double, Vector[Double]) => Either[OdeFailure, Vector[Double]] = method match
    case OdeMethod.Euler => eulerStep(p, _, _, h)
    case _               => rk4Step(p, _, _, h)
  @annotation.tailrec
  def loop(i: Int, y: Vector[Double]): Either[OdeFailure, Vector[Double]] =
    if i >= n then Right(y)
    else step(t0 + i * h, y) match
      case Left(fail)                                           => Left(fail)
      case Right(next) if next.exists(d => d.isNaN || d.isInfinite) => Left(OdeFailure.NonFinite(t0 + (i + 1) * h))
      case Right(next)                                          => loop(i + 1, next)
  loop(0, y0)


// ── Dormand–Prince 5(4) ──

/** The Dormand–Prince tableau: nodes, the stage matrix, the 5th-order weights, and the
 *  difference between the 5th- and 4th-order weights that estimates the local error. */
private object DP:
  val c: Vector[Double] = Vector(0.0, 1.0 / 5, 3.0 / 10, 4.0 / 5, 8.0 / 9, 1.0, 1.0)
  val a: Vector[Vector[Double]] = Vector(
    Vector(),
    Vector(1.0 / 5),
    Vector(3.0 / 40, 9.0 / 40),
    Vector(44.0 / 45, -56.0 / 15, 32.0 / 9),
    Vector(19372.0 / 6561, -25360.0 / 2187, 64448.0 / 6561, -212.0 / 729),
    Vector(9017.0 / 3168, -355.0 / 33, 46732.0 / 5247, 49.0 / 176, -5103.0 / 18656),
    Vector(35.0 / 384, 0.0, 500.0 / 1113, 125.0 / 192, -2187.0 / 6784, 11.0 / 84))
  val b5: Vector[Double] = Vector(35.0 / 384, 0.0, 500.0 / 1113, 125.0 / 192, -2187.0 / 6784, 11.0 / 84, 0.0)
  private val b4: Vector[Double] = Vector(5179.0 / 57600, 0.0, 7571.0 / 16695, 393.0 / 640,
                                          -92097.0 / 339200, 187.0 / 2100, 1.0 / 40)
  val e: Vector[Double] = b5.lazyZip(b4).map(_ - _)

/** One Dormand–Prince attempt: the 5th-order state and the local-error estimate (max norm). */
private def dpAttempt(p: VectorProblem, t: Double, y: Vector[Double],
                      h: Double): Either[OdeFailure, (Vector[Double], Double)] =
  val stages = DP.c.indices.foldLeft[Either[OdeFailure, Vector[Vector[Double]]]](Right(Vector.empty)) {
    (acc, i) => acc.flatMap { ks =>
      val yi = combine(y, h, DP.a(i).zip(ks))
      p.f(t + DP.c(i) * h, yi).map(ks :+ _)
    }
  }
  stages.map { ks =>
    val y5  = combine(y, h, DP.b5.zip(ks))
    val err = combine(Vector.fill(y.size)(0.0), h, DP.e.zip(ks)).map(math.abs).maxOption.getOrElse(0.0)
    (y5, err)
  }

/** Integrates from `(t0, y0)` exactly to `t1` with adaptive Dormand–Prince steps.
 *
 *  The local error per step is held below `tol · (1 + ‖y‖∞)`; the step grows by at most 5× and
 *  shrinks by at most 5× per attempt (the usual 0.9 safety factor), and the last step is cut to
 *  land on `t1`.  A step below `1e-12` of the time scale is a stiff problem or a singularity, and
 *  declines rather than crawls. */
private[ode] def adaptive(p: VectorProblem, t0: Double, y0: Vector[Double], t1: Double,
                          tol: Double, budget: Int): Either[OdeFailure, (Vector[Double], Int)] =
  val dir = math.signum(t1 - t0)
  @annotation.tailrec
  def loop(t: Double, y: Vector[Double], h: Double, steps: Int): Either[OdeFailure, (Vector[Double], Int)] =
    if dir * (t1 - t) <= 1e-14 * math.max(1.0, math.abs(t1)) then Right((y, steps))
    else if steps >= budget then Left(OdeFailure.MaxSteps(MaxOdeSteps))
    else
      val hh = if dir * (t + h - t1) > 0 then t1 - t else h
      if math.abs(hh) < 1e-12 * math.max(1.0, math.abs(t)) then Left(OdeFailure.StepSizeUnderflow(t))
      else dpAttempt(p, t, y, hh) match
        case Left(fail) => Left(fail)
        case Right((y5, err)) =>
          val scale  = tol * (1.0 + y.map(math.abs).maxOption.getOrElse(0.0))
          val factor = if err == 0.0 then 5.0 else math.min(5.0, math.max(0.2, 0.9 * math.pow(scale / err, 0.2)))
          if err <= scale && y5.forall(d => !d.isNaN && !d.isInfinite) then loop(t + hh, y5, hh * factor, steps + 1)
          else loop(t, y, hh * factor, steps + 1)
  if t1 == t0 then Right((y0, 0)) else loop(t0, y0, t1 - t0, 0)


/** The local-error tolerance of `rk45`: three orders below the tolerance `=` uses, so that the
 *  global error over an ordinary interval still sits inside it.  **Measured** (F_0051): the
 *  harmonic oscillator over `2π` ends about 1/400 of the `=` tolerance from its start at every
 *  precision tried (3, 5, 8, 10) — e.g. `1.4e-8` against `5e-6` at the default. */
private[ode] def rk45Tolerance(env: Environment): Double = 0.5 * math.pow(10, -env.precision) * 1e-3


/** One step of `method` from `(t0, y0)` over `h` (F_0051's `odeStep`).
 *
 *  For `euler` and `rk4` this is exactly one step of size `h`, uncertified — the step is the
 *  caller's choice, and a fixed step is what a controller simulation wants.  For `rk45`, `h` is
 *  the interval: the state at `t0 + h`, reached by as many adaptive steps as the tolerance needs.
 *
 *  @return the state at `t0 + h`, or why it was not computed
 */
def odeStep(rhs: _Expression, state: OdeState, indepVar: _Variable, t0: Double, y0: Vector[Double],
            h: Double, method: OdeMethod, env: Environment): Either[OdeFailure, Vector[Double]] =
  validated(state, y0, h).flatMap { _ =>
    val p = VectorProblem(rhs, state, indepVar, env)
    method match
      case OdeMethod.RK45 => adaptive(p, t0, y0, t0 + h, rk45Tolerance(env), MaxOdeSteps).map(_._1)
      case m              => fixedSteps(p, m, t0, y0, t0 + h, 1)
  }

/** The trajectory from `t0` to `t1` at output spacing `h` (F_0051's `odeSolve`).
 *
 *  One row per output time, `t` first: `t0, t0 + h, …` and `t1` itself, the last interval cut short
 *  when `h` does not divide the span.  Each interval is one step of `euler`/`rk4`, or adaptive
 *  `rk45` steps landing exactly on the output time.
 *
 *  @return the rows `(t, state)`, or why they were not computed
 */
def odeSolve(rhs: _Expression, state: OdeState, indepVar: _Variable, t0: Double, y0: Vector[Double],
             t1: Double, h: Double, method: OdeMethod,
             env: Environment): Either[OdeFailure, Vector[(Double, Vector[Double])]] =
  for
    _    <- validated(state, y0, h)
    _    <- Either.cond(t1 != t0 && math.signum(t1 - t0) == math.signum(h), (),
                        OdeFailure.InvalidInput("h must be non-zero and point from t0 towards t1"))
    rows  = math.ceil(math.abs((t1 - t0) / h) - 1e-9).toLong + 1
    _    <- Either.cond(rows <= MaxTrajectoryRows, (), OdeFailure.TooManyRows(rows))
    path <- trajectory(VectorProblem(rhs, state, indepVar, env), method, t0, y0, t1, h, env)
  yield path

private def trajectory(p: VectorProblem, method: OdeMethod, t0: Double, y0: Vector[Double], t1: Double,
                       h: Double, env: Environment): Either[OdeFailure, Vector[(Double, Vector[Double])]] =
  val times = Iterator.iterate(t0)(_ + h).takeWhile(t => math.signum(t1 - t) == math.signum(h) &&
                math.abs(t1 - t) > 1e-9 * math.abs(h)).toVector :+ t1
  times.sliding(2).foldLeft[Either[OdeFailure, Vector[(Double, Vector[Double])]]](Right(Vector((t0, y0)))) {
    (acc, pair) => acc.flatMap { rows =>
      val (ta, tb) = (pair(0), pair(1))
      val y        = rows.last._2
      val next = method match
        case OdeMethod.RK45 => adaptive(p, ta, y, tb, rk45Tolerance(env), MaxOdeSteps).map(_._1)
        case m              => fixedSteps(p, m, ta, y, tb, 1)
      next.map(s => rows :+ (tb, s))
    }
  }

private def validated(state: OdeState, y0: Vector[Double], h: Double): Either[OdeFailure, Unit] =
  val n = y0.size
  if n == 0 then Left(OdeFailure.InvalidInput("the initial state is empty"))
  else if y0.exists(d => d.isNaN || d.isInfinite) then Left(OdeFailure.InvalidInput("the initial state must be finite"))
  else if h == 0.0 || h.isNaN || h.isInfinite then Left(OdeFailure.InvalidInput("h must be a finite non-zero number"))
  else state match
    case OdeState.Components(vs) if vs.size != n =>
      Left(OdeFailure.InvalidInput(s"${vs.size} state names for an initial state of $n entries"))
    case OdeState.Components(vs) if vs.distinct.size != vs.size =>
      Left(OdeFailure.InvalidInput("the state names must be distinct"))
    case _ => Right(())


/** `ode`'s automatic vector integration (F_0051 steps 2 and Decision B): RK4 with the scalar
 *  tier's `stepCount`, **certified by step doubling** — the problem is integrated with `n` and
 *  `2n` steps, and answered (the finer result) only when the two agree within the tolerance `=`
 *  uses.  A stiff or under-resolved problem then declines instead of returning a finite,
 *  confident and wrong number.  **The certificate triples the work** (`n + 2n` steps):
 *  measured 121 ms for the oscillator over `2π` and 214 ms for Lotka–Volterra over 5 (JVM),
 *  while a stiff `y' = -10⁶(y - cos t)` declines in 7 ms, its state going non-finite.
 *
 *  @return the state at `target`, or why it was not certified
 */
def odeCertified(rhs: _Expression, state: OdeState, indepVar: _Variable, t0: Double,
                 y0: Vector[Double], target: Double, env: Environment): Either[OdeFailure, Vector[Double]] =
  if target == t0 then Right(y0)
  else
    val p  = VectorProblem(rhs, state, indepVar, env)
    val n  = stepCount(math.abs(target - t0), env.precision)
    for
      coarse <- fixedSteps(p, OdeMethod.RK4, t0, y0, target, n)
      fine   <- fixedSteps(p, OdeMethod.RK4, t0, y0, target, 2 * n)
      diff    = coarse.lazyZip(fine).map((a, b) => math.abs(a - b)).max
      _      <- Either.cond(diff <= 0.5 * math.pow(10, -env.precision), (), OdeFailure.NotCertified(diff))
    yield fine

/** Reads an evaluated initial state as a column of numbers (an exact column demoted, the `expm`
 *  precedent), or `None` when it is not one. */
private[ode] def columnState(e: _Expression, env: Environment): Option[Vector[Double]] =
  e.eval(env) match
    case Right(m: _MatrixValue) if m.cols == 1 => Some(m.toVector)
    case Left(m: _MatrixShaped) if m.cols == 1 =>
      val cells = m.children.collect { case _Number(d) => d }
      Option.when(cells.size == m.rows)(cells.toVector)
    case _ => None
