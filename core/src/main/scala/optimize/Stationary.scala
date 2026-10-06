package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*
import matrix._Matrix
import equation._Equation
import vector._Hessian


/** What the second-order test says about a stationary point. */
enum StationaryKind:
  /** The Hessian is positive definite: a strict local minimum. */
  case Minimum
  /** The Hessian is negative definite: a strict local maximum. */
  case Maximum
  /** The Hessian has eigenvalues of both signs: neither a minimum nor a maximum. */
  case Saddle
  /** The Hessian is singular and otherwise of one sign: **the test cannot decide**.
   *
   *  `x⁴` and `x³` both have a zero Hessian at the origin — a minimum and an inflection —
   *  so this is a statement about the test, not about the point.  It is the Kleene
   *  `unknown` of the second-order test. */
  case Degenerate


/** Every stationary point of `f`, the solutions of `∇f = 0` (F_0010 slice 1).
 *
 *  The gradient's components are ordinary derivatives in the given order, solved by
 *  [[eliminate]]; the answer inherits its completeness guarantee — `None` whenever the set
 *  cannot be proved complete, never a partial list.
 *
 *  @param f    the objective
 *  @param vars the variables, an ordered tuple (the `grad` convention)
 *  @param env  bindings and precision
 *  @return every stationary point, `Some(Nil)` when there provably are none, or `None`
 */
def stationaryPoints(f: _Expression, vars: Vector[_Variable],
                     env: Environment): Option[List[SolutionPoint]] =
  if !distinctVariables(vars) then None
  else eliminate(vars.toList.map(v => derive(f, v)), vars.toList, env)

/** The second-order test at a stationary point: the signs of the Hessian's eigenvalues.
 *
 *  An eigenvalue counts as zero when it is below `1e-9` of the largest in magnitude — a
 *  relative test, so the verdict does not move when `f` is rescaled (the units rule).
 *
 *  @param f     the objective
 *  @param vars  the variables, in the order the point is written in
 *  @param point the stationary point, every coordinate numeric
 *  @param env   bindings and precision
 *  @return the classification, or `None` when the Hessian does not evaluate there
 */
def classifyStationary(f: _Expression, vars: Vector[_Variable], point: Map[String, Double],
                       env: Environment): Option[StationaryKind] =
  numericHessian(f, vars, point, env).flatMap(eigenvaluesOf).map { ev =>
    val scale = ev.map(math.abs).maxOption.getOrElse(0.0)
    val zero  = 1e-9 * scale
    val pos   = ev.exists(_ > zero)
    val neg   = ev.exists(_ < -zero)
    val flat  = ev.exists(l => math.abs(l) <= zero)
    if pos && neg then StationaryKind.Saddle
    else if pos && !flat then StationaryKind.Minimum
    else if neg && !flat then StationaryKind.Maximum
    else StationaryKind.Degenerate
  }

/** Every stationary point of `f` on the surface `g = 0`, by Lagrange multipliers
 *  (F_0010 slice 3).
 *
 *  Solves `∇f = Σλᵢ∇gᵢ` together with `gᵢ = 0` for the variables and the multipliers, and reports
 *  the variables only.  The multipliers are invented names, so they come from `freshVar`
 *  (prefix `__lambda`, the 2.12 rule): a literal name would be captured by a parameter that
 *  happened to share it.  A point whose multipliers are not determined (a degenerate
 *  constraint, where the constraint qualification fails) leaves a continuum in the
 *  multipliers, and the solver declines rather than drop it.
 *
 *  @param f           the objective
 *  @param constraints the constraint expressions, each meaning `gᵢ = 0`
 *  @param vars        the variables, an ordered tuple
 *  @param env         bindings and precision
 *  @return every constrained stationary point, `Some(Nil)` when provably none, or `None`
 */
def lagrangePoints(f: _Expression, constraints: Vector[_Expression], vars: Vector[_Variable],
                   env: Environment): Option[List[SolutionPoint]] =
  if !distinctVariables(vars) || constraints.isEmpty then None
  else
    val lambdas = freshNames(constraints.size, "__lambda", reservedNames(f +: constraints, vars))
    val stationarity = vars.toList.map { x =>
      constraints.zip(lambdas).foldLeft(derive(f, x)) { case (acc, (g, l)) =>
        Sum(acc, Product(_Rational.literalLike(-1, f), Product(l, derive(g, x))))
      }
    }
    eliminate(stationarity ++ constraints.toList, vars.toList ++ lambdas, env)
      .map(points => projected(points, vars, env))


/** The points as the language writes them: one row of `v = value` per point. */
private[leonardo] def pointsMatrix(points: List[SolutionPoint], vars: Vector[_Variable]): _Matrix =
  _Matrix(points.size, vars.size,
          for p <- points.toVector; v <- vars yield _Equation(v, p.getOrElse(v.variable, v)))

/** Reads a points matrix back into numeric points, for the REPL's classification note. */
private[leonardo] def numericPoints(m: _Expression): List[Map[String, Double]] = m match
  case _Matrix(rows, cols, cells) =>
    cells.grouped(cols).toList.take(rows).flatMap { row =>
      val coords = row.collect { case _Equation(v: _Variable, _Number(d)) => v.variable -> d }
      Option.when(coords.size == row.size)(coords.toMap)
    }
  case _ => Nil

/** The variables' coordinates only, deduplicated: several multiplier values can share one point. */
private[optimize] def projected(points: List[SolutionPoint], vars: Vector[_Variable],
                      env: Environment): List[SolutionPoint] =
  val tolerance = 0.5 * math.pow(10, -env.precision)
  points.map(_.view.filterKeys(k => vars.exists(_.variable == k)).toMap)
    .foldLeft(List.empty[SolutionPoint]) { (acc, p) =>
      val same = acc.exists(q => vars.forall { v =>
        (q.get(v.variable), p.get(v.variable)) match
          case (Some(_Number(a)), Some(_Number(b))) => math.abs(a - b) <= tolerance
          case (a, b)                               => a == b
      })
      if same then acc else acc :+ p
    }

/** The Hessian of `f` at a numeric point, or `None` when a cell does not evaluate. */
private[optimize] def numericHessian(f: _Expression, vars: Vector[_Variable],
                                     point: Map[String, Double],
                                     env: Environment): Option[_MatrixValue] =
  val n     = vars.size
  val bound = point.foldLeft(env) { case (acc, (k, d)) => acc.withBinding(k, _Number(d)) }
  hessianCells(f, vars, env).flatMap { cells =>
    sequence(cells.toList.map(c => c.eval(bound) match
      case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
      case _                                               => None))
      .map(ds => _MatrixValue(n, n, ds.toArray))
  }

/** The symbolic Hessian's cells, row-major — through `vector._Hessian`, so there is one
 *  definition of the Hessian in the library. */
private[optimize] def hessianCells(f: _Expression, vars: Vector[_Variable],
                                   env: Environment): Option[Vector[_Expression]] =
  _Hessian(f, vars).eval(env) match
    case Left(_Matrix(_, _, cells)) => Some(cells)
    case _                          => None

/** The real eigenvalues of a symmetric matrix; `None` when the iteration fails or a pair is
 *  genuinely complex (which a symmetric Hessian cannot produce, so that is a failure too). */
private[optimize] def eigenvaluesOf(m: _MatrixValue): Option[Vector[Double]] =
  m.eigenDecompose.flatMap { ev =>
    val parts = ev.flatMap(_Complex.parts)
    Option.when(parts.size == ev.size &&
                parts.forall((re, im) => math.abs(im) <= 1e-9 * math.max(1.0, math.abs(re))))(
      parts.map(_._1))
  }

/** True when no variable is repeated: `stationary(f, x, x)` names no basis. */
private[optimize] def distinctVariables(vars: Vector[_Variable]): Boolean =
  vars.nonEmpty && vars.distinct.sizeIs == vars.size

/** Every name an invented variable must avoid. */
private[optimize] def reservedNames(es: Iterable[_Expression], vars: Vector[_Variable]): Set[String] =
  es.flatMap(_.freeVars).toSet ++ vars.map(_.variable)

/** `n` fresh variables with a common prefix, each avoiding the others too. */
private[optimize] def freshNames(n: Int, prefix: String, reserved: Set[String]): List[_Variable] =
  (0 until n).foldLeft((List.empty[_Variable], reserved)) { case ((acc, taken), _) =>
    val v = freshVar(taken, prefix)
    (acc :+ v, taken + v.variable)
  }._1
