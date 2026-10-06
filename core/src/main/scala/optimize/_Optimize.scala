package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix


/** Shared rendering of an optimization node's variable tuple: the `grad` convention,
 *  trailing names after the other arguments. */
private def tuple(vars: Vector[_Variable]): String = vars.map(_.variable).mkString(", ")


/** `stationary(f, x, y, …)` — every stationary point of `f` (F_0010 slice 1).
 *
 *  Evaluates to the points as a `k×n` matrix with one row of `v = value` per point (the
 *  `solveSystem` row shape), to `false` when there provably are none, and stays symbolic when
 *  the set cannot be proved complete.  Unlike `solve`, the REPL does **not** bind the answer:
 *  several points cannot all be the value of `x`.
 *
 *  The variables follow the `_Taylor` convention: free in the result, so excluded from
 *  `children` and carried through `rebuild`, which keeps `substitute` from renaming them.
 *
 *  @param f    the objective
 *  @param vars the variables, an ordered tuple
 */
case class _Stationary(f: _Expression, vars: Vector[_Variable]) extends _Expression:
  override def toString: String = s"stationary($f, ${tuple(vars)})"
  override def children: List[_Expression] = List(f)
  override def rebuild(c: List[_Expression]): _Expression = _Stationary(c.head, vars)

  override def eval(env: Environment): Either[_Expression, _Value] =
    pointsResult(stationaryPoints(f, vars, env), vars, this)


/** `convex(f, x, y, …)` — `true`, `false`, or symbolic when neither can be established
 *  (F_0010 slice 2; see [[convexity]] for what each answer requires).
 *
 *  @param f    the function
 *  @param vars the variables, an ordered tuple
 */
case class _Convex(f: _Expression, vars: Vector[_Variable]) extends _Expression:
  override def toString: String = s"convex($f, ${tuple(vars)})"
  override def children: List[_Expression] = List(f)
  override def rebuild(c: List[_Expression]): _Expression = _Convex(c.head, vars)

  override def eval(env: Environment): Either[_Expression, _Value] =
    convexity(f, vars, env).fold(Left(this))(b => Right(_Bool(b)))


/** `lagrange(f, [[g1], [g2]], x, y, …)` — the stationary points of `f` on `g = 0`, by
 *  Lagrange multipliers (F_0010 slice 3).  Same answer shape as [[_Stationary]].
 *
 *  A constraint cell is an expression meaning `g = 0` or an equation `l = r`; a single
 *  constraint may be written without the matrix.
 *
 *  @param f           the objective
 *  @param constraints the constraint group
 *  @param vars        the variables, an ordered tuple
 */
case class _Lagrange(f: _Expression, constraints: _Expression, vars: Vector[_Variable]) extends _Expression:
  override def toString: String = s"lagrange($f, $constraints, ${tuple(vars)})"
  override def children: List[_Expression] = List(f, constraints)
  override def rebuild(c: List[_Expression]): _Expression = _Lagrange(c.head, c(1), vars)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val points = Constraints.equalities(constraints).flatMap(gs => lagrangePoints(f, gs, vars, env))
    pointsResult(points, vars, this)


/** `kkt(f, g, h, x, y, …)` — the Karush–Kuhn–Tucker conditions of `min f` subject to
 *  `g ≤ 0` and `h = 0`, **stated** as one conjunction of relations (F_0010 slice 4).
 *
 *  Evaluates to the conditions rather than to their solutions: the conditions are an
 *  equivalent restatement anyone can read, check and hand to `solve`, while solving them is
 *  [[kktPoints]], library API.  Pass `0` for an absent group — the constraint `0 ≤ 0` (or
 *  `0 = 0`) holds trivially and is dropped.
 *
 *  @param f    the objective
 *  @param g    the inequality group, cells meaning `g ≤ 0` (or written as `l <= r`, `l >= r`)
 *  @param h    the equality group, cells meaning `h = 0` (or written as `l = r`)
 *  @param vars the variables, an ordered tuple
 */
case class _KKT(f: _Expression, g: _Expression, h: _Expression, vars: Vector[_Variable]) extends _Expression:
  override def toString: String = s"kkt($f, $g, $h, ${tuple(vars)})"
  override def children: List[_Expression] = List(f, g, h)
  override def rebuild(c: List[_Expression]): _Expression = _KKT(c.head, c(1), c(2), vars)

  override def eval(env: Environment): Either[_Expression, _Value] =
    Left(kktConditions(f, g, h, vars, env).getOrElse(this))


/** `minimize(f, [[x], [y]], x0, method)` and `minimize(f, [[x], [y]], x0, lb, ub, method)` —
 *  the numeric minimiser as an `n×1` column (F_0010 slice 5).
 *
 *  **The variables are a column, unlike the symbolic four**, because a varargs tuple cannot be
 *  followed by a starting point, bounds and a method; the column is also the shape `x0` and
 *  the answer have, so one is a drop-in warm start for the next.  The variables are binders
 *  (the answer does not mention them), excluded from `children`.
 *
 *  Declines — stays symbolic — whenever [[minimize]] does; the REPL appends why.
 *
 *  @param f      the objective
 *  @param vars   the variables, in the order of `x0`
 *  @param x0     the starting point: an `n×1` column, or a number when `n = 1`
 *  @param bounds the lower and upper bounds, same shape, entries may be `inf` / `-inf`
 *  @param method the method, always named
 */
case class _Minimize(f: _Expression, vars: Vector[_Variable], x0: _Expression,
                     bounds: Option[(_Expression, _Expression)], method: MinimizeMethod) extends _Expression:
  override def toString: String =
    val vs = vars.map(v => s"[${v.variable}]").mkString("[", ", ", "]")
    val bs = bounds.fold("")((lo, hi) => s", $lo, $hi")
    s"minimize($f, $vs, $x0$bs, ${MinimizeMethod.keyword(method)})"
  override def children: List[_Expression] = f :: x0 :: bounds.toList.flatMap((lo, hi) => List(lo, hi))
  override def rebuild(c: List[_Expression]): _Expression =
    _Minimize(c.head, vars, c(1), bounds.map(_ => (c(2), c(3))), method)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this),
                       r => Right(_MatrixValue(r.x.size, 1, r.x.toArray)))

  /** The solver's full answer: the certified result, or why there is none.  What the REPL
   *  queries to explain a decline (F_0052 Decision A); `eval` is defined over it. */
  def detailed(env: Environment): Either[MinimizeFailure, MinimizeResult] =
    val n = vars.size
    val read = for
      start <- columnOf(x0, n, env)
      box   <- bounds match
                 case None           => Some(None)
                 case Some((lo, hi)) => for l <- columnOf(lo, n, env); u <- columnOf(hi, n, env) yield Some((l, u))
    yield (start, box)
    read match
      case None => Left(MinimizeFailure.InvalidInput(
                     s"the starting point and bounds must be numeric columns of $n entries"))
      case Some((start, box)) => minimize(f, vars, start, box, method, env)


/** The answer shape shared by `stationary` and `lagrange`. */
private def pointsResult(points: Option[List[SolutionPoint]], vars: Vector[_Variable],
                         self: _Expression): Either[_Expression, _Value] =
  points match
    case None      => Left(self)
    case Some(Nil) => Right(_Bool(false))
    case Some(ps)  => Left(pointsMatrix(ps, vars))

/** Reads a numeric `n×1` column (or `1×n` row, or a number when `n = 1`), infinite entries
 *  allowed — they are how an absent bound is written. */
private def columnOf(e: _Expression, n: Int, env: Environment): Option[Vector[Double]] =
  e.eval(env) match
    case Right(m: _MatrixValue) if m.rows * m.cols == n && (m.rows == 1 || m.cols == 1) =>
      Some(m.toVector)
    case Right(_Number(d)) if n == 1 => Some(Vector(d))
    case Left(_Matrix(rows, cols, cells)) if rows * cols == n && (rows == 1 || cols == 1) =>
      sequence(cells.toList.map(c => c.eval(env) match
        case Right(_Number(d)) => Some(d)
        case _                 => None)).map(_.toVector)
    case _ => None
