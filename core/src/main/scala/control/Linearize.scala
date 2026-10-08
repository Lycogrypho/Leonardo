package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix._Matrix
import vector._Jacobian


/** Why [[linearize]] declined (F_0052 Decision A: the reason travels as data, and the REPL
 *  turns it into a note). */
enum LinearizeFailure:
  /** The arguments do not describe a system and a point: a field or point that is not a
   *  column, a point of the wrong length, a name listed twice, or a point that mentions one of
   *  the state or input names. */
  case InvalidInput(detail: String)
  /** The point violates a domain requirement of `F` or `G` — `ln` of a negative, `asin` of 2.
   *  @param what       which component, e.g. `F row 2`
   *  @param variable   the coordinate the analysis was asked about
   *  @param constraint the requirement that fails there */
  case OutsideDomain(what: String, variable: _Variable, constraint: Constraint)
  /** A value or a first partial is not a real number at the point (a kink, a pole, a complex
   *  value) — `abs(x)` has no derivative at `0`.
   *  @param what which entry, e.g. `A(2, 1)` */
  case NotReal(what: String)


/** A linearisation `x' ≈ A·δx + B·δu + F₀`, `y ≈ C·δx + D·δu + G₀` about an operating point.
 *
 *  Each field is an expression: a dense matrix at a numeric point, a symbolic `_Matrix` at a
 *  point with free parameters, exact cells when the input is exact.
 *
 *  @param a  `∂F/∂x`, `n×n`
 *  @param b  `∂F/∂u`, `n×m`
 *  @param c  `∂G/∂x`, `p×n`
 *  @param d  `∂G/∂u`, `p×m`
 *  @param f0 `F(x₀, u₀)`, `n×1` — zero at an equilibrium
 *  @param g0 `G(x₀, u₀)`, `p×1`
 */
final case class Linearization(a: _Expression, b: _Expression, c: _Expression, d: _Expression,
                               f0: _Expression, g0: _Expression):
  /** The state-space row `[[A, B, C, D]]` — the shape [[stateSpace]] builds, so `c2dExact`,
   *  `controllable` and `observable` take its entries unchanged. */
  def model: _Expression = stateSpace(a, b, c, d)
  /** The row `[[A, B, C, D, F0, G0]]`, the model with the offsets a non-equilibrium point needs. */
  def affine: _Expression = _Matrix(1, 6, Vector(a, b, c, d, f0, g0))


/** Linearises `x' = F(x, u)`, `y = G(x, u)` about the operating point `(x₀, u₀)` (F_0059).
 *
 *  `A = ∂F/∂x`, `B = ∂F/∂u`, `C = ∂G/∂x`, `D = ∂G/∂u` — four [[vector._Jacobian]]s, so the
 *  library has one definition of a Jacobian — evaluated at the point, plus the offsets
 *  `F(x₀, u₀)` and `G(x₀, u₀)` that are zero only at an equilibrium. The tuples are **ordered
 *  and explicit**, never inferred from the free names (the `grad` rule): the column order of `A`
 *  is the order given. A field or a point may be written as a column or, for one component, as
 *  a bare expression (a number is a `1×1` matrix, the `mpcMatrices` reading).
 *
 *  **Declines rather than linearising at a point where the model is not defined there.** The
 *  domain analysis is consulted first, at a numeric point — its one use as a *guard*, allowed
 *  because this call is new and changes no existing result — and then every value and partial
 *  must be real: a complex value, a pole, or a kink such as `abs(x)` at `0` (whose derivative is
 *  `0/0`) is a refusal with the entry named, never a number. A point with free parameters gives a
 *  symbolic linearisation; a closed entry that did not fold is a failed computation, not a
 *  symbolic one, and declines (the `laurent` rule).
 *
 *  @param f   the dynamics, `n×1`
 *  @param g   the outputs, `p×1`
 *  @param x   the state names, in order
 *  @param u   the input names, in order
 *  @param x0  the operating state, `n×1`
 *  @param u0  the operating input, `m×1`
 *  @param env the environment (precision, working precision, bindings of other parameters)
 *  @return the linearisation, or the reason it was declined
 */
def linearize(f: _Expression, g: _Expression, x: Vector[_Variable], u: Vector[_Variable],
              x0: _Expression, u0: _Expression, env: Environment): Either[LinearizeFailure, Linearization] =
  import LinearizeFailure.*
  val coords = x ++ u
  for
    _  <- Either.cond(x.nonEmpty && u.nonEmpty, (), InvalidInput("the state and input tuples must each name at least one variable"))
    _  <- coords.diff(coords.distinct).headOption.toLeft(()).left.map(v =>
            InvalidInput(s"${v.variable} is listed twice among the states and inputs"))
    fc <- columnOf(f, "F", env)
    gc <- columnOf(g, "G", env)
    xp <- columnOf(x0, "x0", env).filterOrElse(_.sizeIs == x.size,
            InvalidInput(s"x0 has the wrong length: the state has ${x.size} components"))
    up <- columnOf(u0, "u0", env).filterOrElse(_.sizeIs == u.size,
            InvalidInput(s"u0 has the wrong length: the input has ${u.size} components"))
    point = coords.map(_.variable).zip(xp ++ up).toMap
    _  <- (xp ++ up).flatMap(_.freeVars).find(point.contains).toLeft(()).left.map(n =>
            InvalidInput(s"the operating point refers to $n, one of the variables being linearised in"))
    _  <- domainCheck(fc.map("F" -> _) ++ gc.map("G" -> _), coords, point, env).toLeft(())
    f0 <- valuesAt(fc.map(Vector(_)), "F", point, env, rowLabels = true)
    g0 <- valuesAt(gc.map(Vector(_)), "G", point, env, rowLabels = true)
    a  <- jacobianAt(fc, x, "A", point, env)
    b  <- jacobianAt(fc, u, "B", point, env)
    c  <- jacobianAt(gc, x, "C", point, env)
    d  <- jacobianAt(gc, u, "D", point, env)
  yield Linearization(a, b, c, d, f0, g0)

/** The components of a column, or of one bare expression; `InvalidInput` for anything else.
 *
 *  A literal matrix is read without evaluating it, for `vector.componentsOf`'s reason: the
 *  field must be differentiated before a value is substituted. */
private def columnOf(e: _Expression, label: String, env: Environment): Either[LinearizeFailure, Vector[_Expression]] =
  def notColumn = LinearizeFailure.InvalidInput(s"$label must be a column (an n×1 matrix)")
  e match
    case m: _Matrix => Either.cond(m.cols == 1, m.elems, notColumn)
    case _ => e.eval(env).toExpression match
      case m: _Matrix      => Either.cond(m.cols == 1, m.elems, notColumn)
      case v: _MatrixValue => Either.cond(v.cols == 1, _Matrix.fromValue(v).elems, notColumn)
      case _               => Right(Vector(e))

/** The first domain requirement a component of `F` or `G` violates at the point, when the point
 *  is numeric: each coordinate is asked about in turn with the others bound. */
private def domainCheck(components: Vector[(String, _Expression)], coords: Vector[_Variable],
                        point: Map[String, _Expression], env: Environment): Option[LinearizeFailure] =
  val numeric = point.view.mapValues(_.eval(env)).collect { case (k, Right(_Number(d))) => k -> d }.toMap
  Option.when(numeric.sizeIs == point.size)(()).flatMap { _ =>
    val rows = components.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    (for
      (label, cells) <- Vector("F", "G").flatMap(l => rows.get(l).map(l -> _))
      (cell, i)      <- cells.zipWithIndex
      v              <- coords
    yield
      val others = numeric.removed(v.variable).foldLeft(env)((e, kv) => e.withBinding(kv._1, _Number(kv._2)))
      violatedAt(cell, v, numeric(v.variable), others).map(c =>
        LinearizeFailure.OutsideDomain(s"$label row ${i + 1}", v, c))
    ).flatten.headOption
  }

/** The `∂field/∂vars` block evaluated at the point, named `label` in a refusal. */
private def jacobianAt(field: Vector[_Expression], vars: Vector[_Variable], label: String,
                       point: Map[String, _Expression], env: Environment): Either[LinearizeFailure, _Expression] =
  _Jacobian(_Matrix(field.size, 1, field), vars).eval(env) match
    case Left(j: _Matrix) => valuesAt(j.elems.grouped(j.cols).toVector, label, point, env, rowLabels = false)
    case _                => Left(LinearizeFailure.NotReal(s"$label (no Jacobian)"))

/** A grid of expressions evaluated at the point: a dense matrix when every entry is a number,
 *  a symbolic one when the point has free parameters, `NotReal` naming the first entry that is
 *  neither (complex, non-finite, or closed but unreduced). */
private def valuesAt(rows: Vector[Vector[_Expression]], label: String,
                     point: Map[String, _Expression], env: Environment,
                     rowLabels: Boolean): Either[LinearizeFailure, _Expression] =
  val cells = for (row, i) <- rows.zipWithIndex; (e, j) <- row.zipWithIndex yield
    val at = if rowLabels then s"$label row ${i + 1}" else s"$label(${i + 1}, ${j + 1})"
    substitute(e, point).eval(env) match
      case Right(r: _Rational)                       => Right(r)
      case Right(n @ _Number(v)) if v.isFinite        => Right(n)
      case Left(s) if s.freeVars.nonEmpty            => Right(s)
      case _                                         => Left(LinearizeFailure.NotReal(at))
  sequenceEither(cells).map { cs =>
    _Matrix(rows.size, rows.head.size, cs).eval(env).toExpression
  }

/** The first `Left`, or every `Right` in order. */
private def sequenceEither[E, A](xs: Vector[Either[E, A]]): Either[E, Vector[A]] =
  xs.collectFirst { case Left(e) => e }.toLeft(xs.collect { case Right(a) => a })


/** `linearize(F, G, x, u, x0, u0[, affine])` — [[linearize]] as a node (F_0059).
 *
 *  Evaluates to the state-space row `[[A, B, C, D]]`, or with `affine` to
 *  `[[A, B, C, D, F0, G0]]`; tuple assignment binds the parts. The state and input names are
 *  **binders**: excluded from `children` and carried through `rebuild`, so `substitute` cannot
 *  rewrite the variables the derivatives are taken in. A refusal leaves the call as written,
 *  and [[detailed]] carries the reason the REPL prints.
 *
 *  @param f      the dynamics column
 *  @param g      the output column
 *  @param x      the state names, in order
 *  @param u      the input names, in order
 *  @param x0     the operating state
 *  @param u0     the operating input
 *  @param affine whether to append the offsets `F0`, `G0`
 */
case class _Linearize(f: _Expression, g: _Expression, x: Vector[_Variable], u: Vector[_Variable],
                      x0: _Expression, u0: _Expression, affine: Boolean = false) extends _Expression:
  override def children: List[_Expression] = List(f, g, x0, u0)
  override def rebuild(c: List[_Expression]): _Expression = _Linearize(c(0), c(1), x, u, c(2), c(3), affine)

  override def toString: String =
    def names(vs: Vector[_Variable]) = vs.map(v => s"[${v.variable}]").mkString("[", ", ", "]")
    s"linearize($f, $g, ${names(x)}, ${names(u)}, $x0, $u0${if affine then ", affine" else ""})"

  /** The linearisation, or why it was declined.
   *  @param env the evaluation environment
   *  @return the result or the reason */
  def detailed(env: Environment): Either[LinearizeFailure, Linearization] =
    linearize(f, g, x, u, x0, u0, env)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), lin => Left(if affine then lin.affine else lin.model))
