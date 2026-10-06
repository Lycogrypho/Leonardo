package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*
import matrix._Matrix
import equation.{_Equation, _Comparison, CompareOp}
import logic.And


/** The most inequality constraints [[kktPoints]] enumerates active sets for.
 *
 *  The enumeration solves `2ᵐ` subsystems, so the cost at least doubles per constraint.
 *  **Measured** (F_0010, JVM) on `min Σ(xᵢ − 2)²` subject to `xᵢ ≤ 1`, one variable per
 *  constraint: 0.11 s at `m = 8`, 0.16 s at 9, 0.44 s at 10 — growing by two to three per
 *  constraint, so ten keeps the worst case under half a second on the JVM.  The browser is
 *  slower, which is what recurrent task F_0019 re-measures after every release.
 */
val MaxKKTInequalities: Int = 10


/** The constraints of an optimization problem, read from the language.
 *
 *  A group is a column (or row) of cells, or a single expression; a cell `0` is the vacuous
 *  constraint and is dropped, which is how an absent group is written (`0 ≤ 0` and `0 = 0`
 *  both hold trivially, so passing `0` changes nothing — the same answer an empty group would
 *  give, without a new literal).
 */
private[leonardo] object Constraints:

  /** The cells of a constraint group, unevaluated: a literal matrix is read cell by cell, any
   *  other expression is one constraint. */
  def cells(group: _Expression): Vector[_Expression] = group match
    case _Matrix(_, _, elems) => elems
    case other                => Vector(other)

  /** Equality constraints as `h = 0`: an expression, or an equation `l = r` read as `l − r`.
   *  `None` when a cell is an inequality, which does not belong in this group. */
  def equalities(group: _Expression): Option[Vector[_Expression]] =
    sequence(cells(group).toList.map {
      case _Equation(l, r)  => Some(difference(l, r))
      case _: _Comparison   => None
      case e                => Some(e)
    }).map(_.toVector.filterNot(vacuous))

  /** Inequality constraints as `g ≤ 0`: an expression, or `l ≤ r` / `l ≥ r` rewritten to that
   *  form.  A strict inequality is refused, not relaxed — the KKT conditions are stated for
   *  closed constraints, and silently reading `<` as `≤` would answer a different problem. */
  def inequalities(group: _Expression): Option[Vector[_Expression]] =
    sequence(cells(group).toList.map {
      case _Comparison(l, CompareOp.Le, r) => Some(difference(l, r))
      case _Comparison(l, CompareOp.Ge, r) => Some(difference(r, l))
      case _: _Comparison | _: _Equation   => None
      case e                               => Some(e)
    }).map(_.toVector.filterNot(vacuous))

  private def difference(l: _Expression, r: _Expression): _Expression =
    simplifyFully(Sum(l, Product(_Rational.literalLike(-1, Sum(l, r)), r)))

  private def vacuous(e: _Expression): Boolean = e match
    case r: _Rational => r.isZero
    case _Number(d)   => d == 0.0
    case _            => false


/** The Karush–Kuhn–Tucker conditions of `min f` subject to `g ≤ 0`, `h = 0`, stated in the
 *  language as one conjunction (F_0010 slice 4).
 *
 *  Stationarity `∂f/∂xᵢ + Σμⱼ∂gⱼ/∂xᵢ + Σλₖ∂hₖ/∂xᵢ = 0`, primal feasibility `gⱼ ≤ 0` and
 *  `hₖ = 0`, dual feasibility `μⱼ ≥ 0`, complementary slackness `μⱼ·gⱼ = 0` — every one a
 *  relation the language has had since 4.R, so no new carrier.  **The convention is
 *  minimisation with `g ≤ 0`**, stated because the literature also writes `g ≥ 0` and
 *  maximisation, and the sign of every multiplier depends on which.  The multipliers are named
 *  `mu0, mu1, …` and `lambda0, lambda1, …` through `freshVar`, so they never capture a name
 *  the problem already uses.
 *
 *  @param f    the objective
 *  @param g    the inequality group, each cell meaning `gⱼ ≤ 0`
 *  @param h    the equality group, each cell meaning `hₖ = 0`
 *  @param vars the variables, an ordered tuple
 *  @param env  bindings and precision
 *  @return the conditions, or `None` when a group is malformed
 */
def kktConditions(f: _Expression, g: _Expression, h: _Expression, vars: Vector[_Variable],
                  env: Environment): Option[_Expression] =
  for
    _   <- Option.when(distinctVariables(vars))(())
    gs  <- Constraints.inequalities(g)
    hs  <- Constraints.equalities(h)
  yield
    val reserved = reservedNames(f +: (gs ++ hs), vars)
    val mus      = freshNames(gs.size, "mu", reserved)
    val lambdas  = freshNames(hs.size, "lambda", reserved ++ mus.map(_.variable))
    val zero     = _Rational.literalLike(0, f)
    val stationarity = vars.toList.map(x => _Equation(lagrangian(f, gs.zip(mus) ++ hs.zip(lambdas), x), zero))
    val primal  = gs.map(gj => _Comparison(gj, CompareOp.Le, zero)) ++ hs.map(hk => _Equation(hk, zero))
    val dual    = mus.map(m => _Comparison(m, CompareOp.Ge, zero))
    val slack   = gs.zip(mus).map((gj, m) => _Equation(simplifyFully(Product(m, gj)), zero))
    (stationarity ++ primal ++ dual ++ slack).reduceLeft(And(_, _))

/** The KKT points of `min f` subject to `g ≤ 0`, `h = 0`, by active-set enumeration.
 *
 *  For every subset `A` of the inequalities, the active ones are equalities with a multiplier
 *  and the inactive ones have none; each such system goes to [[eliminate]], and its solutions
 *  are kept when `μⱼ ≥ 0` on `A` and `gⱼ ≤ 0` off it.  The union is every KKT point.
 *
 *  **Necessary, not sufficient**: a KKT point is a candidate minimum under a constraint
 *  qualification, and on a non-convex problem it may be a maximum or a saddle.
 *
 *  Declines (`None`) when any subsystem declines — a missing active set could hide a point —
 *  when feasibility cannot be decided numerically, and past [[MaxKKTInequalities]].
 *
 *  @return the variables' coordinates at every KKT point, or `None`
 */
def kktPoints(f: _Expression, g: _Expression, h: _Expression, vars: Vector[_Variable],
              env: Environment): Option[List[SolutionPoint]] =
  for
    _      <- Option.when(distinctVariables(vars))(())
    gs     <- Constraints.inequalities(g)
    hs     <- Constraints.equalities(h)
    _      <- Option.when(gs.size <= MaxKKTInequalities)(())
    perSet <- sequence(activeSets(gs.size).map(a => activeSetPoints(f, gs, hs, a, vars, env)))
  yield projected(perSet.flatten, vars, env)


/** `∂/∂x` of `f + Σ multiplier·constraint`. */
private def lagrangian(f: _Expression, terms: Seq[(_Expression, _Variable)], x: _Variable): _Expression =
  simplifyFully(terms.foldLeft(derive(f, x)) { case (acc, (c, m)) => Sum(acc, Product(m, derive(c, x))) })

/** Every subset of `0 until m`. */
private def activeSets(m: Int): List[Set[Int]] =
  (0 to m).toList.flatMap(k => (0 until m).toList.combinations(k).map(_.toSet))

/** The KKT points whose active set is exactly `active`. */
private def activeSetPoints(f: _Expression, gs: Vector[_Expression], hs: Vector[_Expression],
                            active: Set[Int], vars: Vector[_Variable],
                            env: Environment): Option[List[SolutionPoint]] =
  val tolerance = 0.5 * math.pow(10, -env.precision)
  val reserved  = reservedNames(f +: (gs ++ hs), vars)
  val act       = active.toVector.sorted
  val mus       = freshNames(act.size, "__mu", reserved)
  val lambdas   = freshNames(hs.size, "__lambda", reserved ++ mus.map(_.variable))
  val terms     = act.map(gs).zip(mus) ++ hs.zip(lambdas)
  val eqs       = vars.toList.map(x => lagrangian(f, terms, x)) ++ act.map(gs) ++ hs
  eliminate(eqs, vars.toList ++ mus ++ lambdas, env).flatMap { points =>
    sequence(points.map { p =>
      val bound = numericBinding(p, env)
      val dualOk   = mus.map(m => valueOf(m, bound).map(_ >= -tolerance))
      val primalOk = gs.indices.filterNot(active.contains).map(j => valueOf(gs(j), bound).map(_ <= tolerance))
      sequence((dualOk ++ primalOk).toList).map(oks => Option.when(oks.forall(identity))(p))
    }).map(_.flatten)
  }

/** The environment with every numeric coordinate of `p` bound. */
private def numericBinding(p: SolutionPoint, env: Environment): Environment =
  p.foldLeft(env) {
    case (acc, (k, v: _Value)) => v match
      case _Number(_) => acc.withBinding(k, v)
      case _          => acc
    case (acc, _) => acc
  }

/** The real value of `e`, or `None` when it does not reduce to one. */
private def valueOf(e: _Expression, env: Environment): Option[Double] = e.eval(env) match
  case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
  case _                                               => None
