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


/** `qp(H, f, A, b[, Aeq, beq], lb, ub)` — the convex quadratic program
 *  `min ½ zᵀHz + fᵀz` subject to `A z ≤ b`, `Aeq z = beq`, `lb ≤ z ≤ ub` (F_0053).
 *
 *  **An absent group is `0`** (Decision A, the `kkt` precedent): `qp(H, f, 0, 0, lb, ub)` has no
 *  inequalities. An absent bound is `-inf` / `inf`, and a number for `lb` or `ub` applies to
 *  every variable, so `lb = 0` reads as "all non-negative". Evaluates to the `n×1` minimiser and
 *  stays symbolic whenever [[qpSolve]] declines; the REPL appends why, or, on success, which
 *  constraints are active (Decision C). The warm start is library API only.
 *
 *  @param h  the Hessian, symmetric positive definite
 *  @param f  the linear term
 *  @param a  the inequality matrix, or `0`
 *  @param b  its right-hand side, or `0`
 *  @param eq the equality matrix and right-hand side, when given
 *  @param lb the lower bounds
 *  @param ub the upper bounds
 */
case class _QP(h: _Expression, f: _Expression, a: _Expression, b: _Expression,
               eq: Option[(_Expression, _Expression)], lb: _Expression, ub: _Expression) extends _Expression:
  override def toString: String =
    val es = eq.fold("")((p, q) => s", $p, $q")
    s"qp($h, $f, $a, $b$es, $lb, $ub)"
  override def children: List[_Expression] =
    List(h, f, a, b) ++ eq.toList.flatMap((p, q) => List(p, q)) ++ List(lb, ub)
  override def rebuild(c: List[_Expression]): _Expression =
    if eq.isDefined then _QP(c.head, c(1), c(2), c(3), Some((c(4), c(5))), c(6), c(7))
    else _QP(c.head, c(1), c(2), c(3), None, c(4), c(5))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), r => Right(_MatrixValue(r.z.size, 1, r.z.toArray)))

  /** The solver's full answer, or why there is none; what the REPL queries (F_0052 Decision A). */
  def detailed(env: Environment): Either[QpFailure, QpResult] =
    numericMatrix(h, env).filter(m => m.rows == m.cols) match
      case None => Left(QpFailure.InvalidInput("H must be a square numeric matrix"))
      case Some(hm) =>
        val n = hm.rows
        val read = for
          fv <- columnOf(f, n, env).toRight(s"f must be a numeric column of $n entries")
          in <- constraintGroup(a, b, n, env).toRight("A and b must be a numeric matrix with " +
                  s"$n columns and a matching column, or 0 and 0 for none")
          eg <- eq.fold(Right(None))((p, q) => constraintGroup(p, q, n, env).toRight("Aeq and beq must be " +
                  s"a numeric matrix with $n columns and a matching column, or 0 and 0 for none"))
          lo <- boundsOf(lb, n, env).toRight(s"lb must be a number or a column of $n entries")
          hi <- boundsOf(ub, n, env).toRight(s"ub must be a number or a column of $n entries")
        yield (fv, in, eg, lo, hi)
        read match
          case Left(detail) => Left(QpFailure.InvalidInput(detail))
          case Right((fv, in, eg, lo, hi)) => qpSolve(hm, fv, in, eg, lo, hi, Set.empty, env)

/** `nlp(f, [[x], [y]], x0, g, h, method)` and `nlp(f, [[x], [y]], x0, g, h, lb, ub, method)` —
 *  a KKT point of `min f` subject to `g ≤ 0`, `h = 0` and `lb ≤ x ≤ ub`, as an `n×1` column
 *  (F_0066, MPC T3-02).
 *
 *  The variables are a column of names, the `minimize` shape; a constraint group is a column, one
 *  expression, or `0` when absent (the `kkt` reading: a cell means `g ≤ 0` or `h = 0`, or is
 *  written as a relation `l <= r`, `l >= r`, `l = r`); bounds are a number for every variable
 *  or a column, `-inf`/`inf` for none (the `qp` reading). Declines — stays symbolic — whenever
 *  [[nlpSolve]] does; the REPL appends why.
 *
 *  @param f      the objective
 *  @param vars   the variables, in the order of `x0`
 *  @param x0     the starting point: an `n×1` column, or a number when `n = 1`
 *  @param g      the inequality group
 *  @param h      the equality group
 *  @param bounds the lower and upper bounds
 *  @param method the method, always named
 */
case class _Nlp(f: _Expression, vars: Vector[_Variable], x0: _Expression, g: _Expression, h: _Expression,
                bounds: Option[(_Expression, _Expression)], method: NlpMethod) extends _Expression:
  override def toString: String =
    val vs = vars.map(v => s"[${v.variable}]").mkString("[", ", ", "]")
    val bs = bounds.fold("")((lo, hi) => s", $lo, $hi")
    s"nlp($f, $vs, $x0, $g, $h$bs, ${NlpMethod.keyword(method)})"
  override def children: List[_Expression] =
    f :: x0 :: g :: h :: bounds.toList.flatMap((lo, hi) => List(lo, hi))
  override def rebuild(c: List[_Expression]): _Expression =
    _Nlp(c.head, vars, c(1), c(2), c(3), bounds.map(_ => (c(4), c(5))), method)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), r => Right(_MatrixValue(r.x.size, 1, r.x.toArray)))

  /** The solver's full answer, or why there is none; what the REPL queries (F_0052 Decision A). */
  def detailed(env: Environment): Either[NlpFailure, NlpResult] =
    val n = vars.size
    val read = for
      start <- columnOf(x0, n, env).toRight(s"the starting point must be a numeric column of $n entries")
      gs    <- Constraints.inequalities(g).toRight("the inequality group holds a strict inequality or an equation; " +
                 "write each as an expression meaning <= 0, or as l <= r or l >= r")
      hs    <- Constraints.equalities(h).toRight("the equality group holds an inequality")
      box   <- bounds match
                 case None           => Right(None)
                 case Some((lo, hi)) =>
                   (boundsOf(lo, n, env), boundsOf(hi, n, env)) match
                     case (Some(l), Some(u)) => Right(Some((l, u)))
                     case _                  => Left(s"each bound must be a number or a column of $n entries")
    yield (start, gs, hs, box)
    read match
      case Left(detail) => Left(NlpFailure.InvalidInput(detail))
      case Right((start, gs, hs, box)) => nlpSolve(f, vars, start, gs, hs, box, method, env)


/** `lp(f, A, b[, Aeq, beq], lb, ub)` — the linear program `min fᵀx` subject to `A x ≤ b`,
 *  `Aeq x = beq`, `lb ≤ x ≤ ub`, solved **exactly** (F_0011).
 *
 *  `qp`'s shape without the Hessian: an absent group is `0`, an absent bound `-inf` / `inf`, a
 *  number for `lb` or `ub` applies to every variable.  Answers the minimiser as a column —
 *  exact cells when every coefficient was exact — and stays as written when [[lpSolve]]
 *  declines; the REPL appends why, or the optimal value and the active constraints.
 *
 *  @param f  the cost vector
 *  @param a  the inequality matrix, or `0`
 *  @param b  its right-hand side, or `0`
 *  @param eq the equality matrix and right-hand side, when given
 *  @param lb the lower bounds
 *  @param ub the upper bounds
 */
case class _LP(f: _Expression, a: _Expression, b: _Expression, eq: Option[(_Expression, _Expression)],
               lb: _Expression, ub: _Expression) extends _Expression:
  override def toString: String =
    val es = eq.fold("")((p, q) => s", $p, $q")
    s"lp($f, $a, $b$es, $lb, $ub)"
  override def children: List[_Expression] =
    List(f, a, b) ++ eq.toList.flatMap((p, q) => List(p, q)) ++ List(lb, ub)
  override def rebuild(c: List[_Expression]): _Expression =
    if eq.isDefined then _LP(c.head, c(1), c(2), Some((c(3), c(4))), c(5), c(6))
    else _LP(c.head, c(1), c(2), None, c(3), c(4))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), r => _Matrix(r.x.size, 1, r.x).eval(env))

  /** The solver's full answer, or why there is none; what the REPL queries. */
  def detailed(env: Environment): Either[LpFailure, LpResult] =
    def column(e: _Expression): Option[Vector[_Value]] =
      valueCells(e, env).collect { case (r, c, cells) if r == 1 || c == 1 => cells }
    def group(m: _Expression, rhs: _Expression, n: Int): Option[Option[(Vector[Vector[_Value]], Vector[_Value])]] =
      (m.eval(env), rhs.eval(env)) match
        case (Right(_Number(0.0)), Right(_Number(0.0))) => Some(None)
        case _ =>
          for
            (rows, cols, cells) <- valueCells(m, env)
            if cols == n
            bs <- column(rhs)
            if bs.size == rows
          yield Some((cells.grouped(n).toVector, bs))
    def bounds(e: _Expression, n: Int): Option[Vector[_Value]] =
      valueCells(e, env) match
        case Some((1, 1, Vector(v))) => Some(Vector.fill(n)(v))
        case _                       => column(e).filter(_.size == n)
    column(f) match
      case None => Left(LpFailure.InvalidInput("f must be a numeric column"))
      case Some(fv) =>
        val n = fv.size
        val read = for
          in <- group(a, b, n).toRight(s"A and b must be a numeric matrix with $n columns and a matching column, or 0 and 0 for none")
          eg <- eq.fold(Right(None))((p, q) => group(p, q, n).toRight(
                  s"Aeq and beq must be a numeric matrix with $n columns and a matching column, or 0 and 0 for none"))
          lo <- bounds(lb, n).toRight(s"lb must be a number or a column of $n entries")
          hi <- bounds(ub, n).toRight(s"ub must be a number or a column of $n entries")
        yield (in, eg, lo, hi)
        read match
          case Left(detail)              => Left(LpFailure.InvalidInput(detail))
          case Right((in, eg, lo, hi)) => lpSolve(fv, in, eg, lo, hi, env.limits)

/** The cells of an evaluated operand as values, **keeping exactness**: a number is `1×1`, a dense
 *  matrix reads as `Double`s, and a matrix with exact cells — which stays a symbolic `_Matrix` —
 *  keeps its rationals, where [[numericMatrix]] would demote them. */
private def valueCells(e: _Expression, env: Environment): Option[(Int, Int, Vector[_Value])] =
  def number(x: _Expression): Option[_Value] = x match
    case r: _Rational => Some(r)
    case _Number(d)   => Some(_Number(d))
    case _            => None
  e.eval(env) match
    case Right(m: _MatrixValue)  => Some((m.rows, m.cols, m.toVector.map(_Number(_))))
    case Right(v)                => number(v).map(x => (1, 1, Vector(x)))
    case Left(m: _MatrixShaped)  => sequence(m.children.map(number)).map(cs => (m.rows, m.cols, cs.toVector))
    case _                       => None

/** A numeric matrix, dense or exact (demoted, the `lu` precedent); a number is `1×1`. */
private[optimize] def numericMatrix(e: _Expression, env: Environment): Option[_MatrixValue] =
  e.eval(env) match
    case Right(_Number(d)) => Some(_MatrixValue(1, 1, Array(d)))
    case r                 => _MatrixValue.ofEvaluated(r)

/** A constraint group: `Some(None)` for the absent `0`/`0`, `Some(Some((A, b)))` for a matrix
 *  with `n` columns and a matching right-hand side, `None` when neither. */
private def constraintGroup(a: _Expression, b: _Expression, n: Int,
                            env: Environment): Option[Option[(_MatrixValue, Vector[Double])]] =
  (a.eval(env), b.eval(env)) match
    case (Right(_Number(0.0)), Right(_Number(0.0))) => Some(None)
    case _ =>
      for
        am <- numericMatrix(a, env).filter(_.cols == n)
        bv <- columnOf(b, am.rows, env)
      yield Some((am, bv))

/** Bounds: a number applies to every variable, otherwise a column of `n` entries. */
private[optimize] def boundsOf(e: _Expression, n: Int, env: Environment): Option[Vector[Double]] =
  e.eval(env) match
    case Right(_Number(d)) => Some(Vector.fill(n)(d))
    case _                 => columnOf(e, n, env)


/** The answer shape shared by `stationary` and `lagrange`. */
private def pointsResult(points: Option[List[SolutionPoint]], vars: Vector[_Variable],
                         self: _Expression): Either[_Expression, _Value] =
  points match
    case None      => Left(self)
    case Some(Nil) => Right(_Bool(false))
    case Some(ps)  => Left(pointsMatrix(ps, vars))

/** Reads a numeric `n×1` column (or `1×n` row, or a number when `n = 1`), infinite entries
 *  allowed — they are how an absent bound is written. */
private[optimize] def columnOf(e: _Expression, n: Int, env: Environment): Option[Vector[Double]] =
  numericMatrix(e, env).collect {
    case m if m.rows * m.cols == n && (m.rows == 1 || m.cols == 1) => m.toVector
  }
