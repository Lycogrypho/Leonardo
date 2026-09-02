package it.grypho.scala.leonardo
package vector

import core.*
import scalar.*
import matrix._Matrix


/** The coordinate system a vector operator is evaluated in (issue 6.24).
 *
 *  Only [[Cartesian]] is implemented.  The other two ship as cases from the start so the
 *  node shape is final: issue 6.26 replaces the Cartesian formulas with the general
 *  orthogonal-curvilinear ones (parameterised by the scale factors `(1,1,1)`, `(1,r,1)` and
 *  `(1,r,r·sinθ)`), which is an `eval` change and touches nothing else.  Until then a
 *  non-Cartesian request **stays symbolic** — answering it with the Cartesian formula would
 *  be a confidently wrong result.
 */
enum CoordinateSystem:
  case Cartesian, Cylindrical, Spherical


/** Shared behaviour of the six vector-calculus operators.
 *
 *  Each carries the field or scalar `e`, the ordered coordinate tuple, and the coordinate
 *  system.  The coordinates are excluded from `children` and carried through `rebuild` (the
 *  `scalar._Taylor` convention — see the package overview), so `substitute` cannot rewrite
 *  the variables the answer is phrased in.
 */
sealed trait _VectorOperator extends _Expression:
  /** The scalar field or vector field the operator is applied to. */
  def e: _Expression
  /** The ordered coordinate tuple; free in the result, never substituted. */
  def coords: Vector[_Variable]
  /** The coordinate system; only `Cartesian` evaluates today. */
  def system: CoordinateSystem

  override def children: List[_Expression] = List(e)

  /** The operator's own name, for `toString`. */
  protected def opName: String

  override def toString: String = s"$opName($e, ${coords.mkString(", ")})"

  /** True when the request is one this tier can answer at all: a Cartesian system and a
   *  coordinate tuple with no repeats.  `grad(f, x, x)` names no basis and is refused rather
   *  than silently producing a duplicated component. */
  protected def wellPosed: Boolean =
    system == CoordinateSystem.Cartesian && coords.nonEmpty && coords.distinct.size == coords.size

  /** Reads `e` as an n×1 vector field: its components, or `None` when it is not one.
   *
   *  **A literal matrix is read without evaluating it**, and that ordering is load-bearing:
   *  the components must be differentiated *before* any point is substituted.  Evaluating
   *  first would collapse `[[x^2], [y^3]]` to a dense `_MatrixValue` as soon as the
   *  coordinates happen to be bound — every cell a constant, every derivative zero, and a
   *  confidently wrong answer rather than a refusal.
   *
   *  Only a non-literal argument is evaluated, which covers the variable-bound-to-a-matrix
   *  case (the issue-1.2 lesson that matrix-vs-scalar cannot always be decided at parse
   *  time).  An `Environment` holds `_Value`s, so such a binding is necessarily a *dense*
   *  `_MatrixValue`: its cells are constants and their derivatives are legitimately zero.
   *
   *  A field is a column (`n×1`); a row vector is a different object and is refused.
   */
  protected def componentsOf(x: _Expression, env: Environment): Option[Vector[_Expression]] =
    x match
      case m: _Matrix if m.cols == 1 => Some(m.elems)
      case _: _Matrix                => None      // a row vector is not a field
      case _ => x.eval(env).toExpression match
        case m: _Matrix if m.cols == 1      => Some(m.elems)
        case v: _MatrixValue if v.cols == 1 => Some(_Matrix.fromValue(v).elems)
        case _                              => None

  /** Builds a column vector from its components, simplified. */
  protected def column(cs: Vector[_Expression]): _Matrix =
    _Matrix(cs.size, 1, cs.map(simplifyFully))

  /** `∂x/∂coords(i)`, simplified. */
  protected def d(x: _Expression, i: Int): _Expression = simplifyFully(derive(x, coords(i)))

  /** Sums a list of terms, folding to `0` when empty. */
  protected def total(terms: Vector[_Expression]): _Expression =
    if terms.isEmpty then _Number(0) else simplifyFully(terms.reduce(Sum.apply))


/** `grad(f, x, y, …)` — the gradient of a scalar field, as an n×1 column vector.
 *
 *  @param e      the scalar field
 *  @param coords the ordered coordinate tuple
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Grad(e: _Expression, coords: Vector[_Variable],
                 system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "grad"
  override def rebuild(c: List[_Expression]): _Expression = _Grad(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] =
    if !wellPosed then Left(this)
    else Left(column(coords.indices.toVector.map(i => d(e, i))))


/** `div(F, x, y, …)` — the divergence of a vector field, a **scalar**.
 *
 *  Requires `F` to be an n×1 field with exactly one component per coordinate: `div` sums
 *  `∂Fᵢ/∂xᵢ`, so a count mismatch has no meaning and stays symbolic.
 *
 *  @param e      the vector field (n×1)
 *  @param coords the ordered coordinate tuple
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Div(e: _Expression, coords: Vector[_Variable],
                system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "div"
  override def rebuild(c: List[_Expression]): _Expression = _Div(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val result =
      for
        comps <- Option.when(wellPosed)(()).flatMap(_ => componentsOf(e, env))
        if comps.sizeIs == coords.size
      yield total(comps.indices.toVector.map(i => d(comps(i), i)))
    result.fold(Left(this))(r => r.eval(env) match
      case Right(v) => Right(v)
      case Left(x)  => Left(x))


/** `curl(F, x, y, z)` — the curl of a 3-D vector field, as a 3×1 column vector.
 *
 *  **Three dimensions only.**  In 2-D the natural object is a *scalar* curl — a different
 *  result type, not a degenerate case of this one — and in higher dimensions the
 *  cross-product form does not exist at all.  Every other arity is refused rather than
 *  guessed, the rule issue 3.3 sets for an analysis that must describe what the library
 *  actually computes.
 *
 *  @param e      the vector field (3×1)
 *  @param coords the three coordinates, in order
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Curl(e: _Expression, coords: Vector[_Variable],
                 system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "curl"
  override def rebuild(c: List[_Expression]): _Expression = _Curl(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val result =
      for
        _     <- Option.when(wellPosed && coords.sizeIs == 3)(())
        comps <- componentsOf(e, env)
        if comps.sizeIs == 3
      yield
        // (∂F₃/∂x₂ − ∂F₂/∂x₃, ∂F₁/∂x₃ − ∂F₃/∂x₁, ∂F₂/∂x₁ − ∂F₁/∂x₂)
        column(Vector.tabulate(3) { i =>
          val (j, k) = ((i + 1) % 3, (i + 2) % 3)
          Sum(d(comps(k), j), Product(_Number(-1), d(comps(j), k)))
        })
    result.fold(Left(this))(Left(_))


/** `laplacian(f, x, y, …)` — the Laplacian of a scalar field, a **scalar**.
 *
 *  Defined as `div(grad(f))` rather than as its own sum of second derivatives, so the two
 *  can never disagree — and so issue 6.26 gets the curvilinear Laplacian for free once
 *  `grad` and `div` carry the scale factors.
 *
 *  A **vector** argument stays symbolic: the vector Laplacian is not the component-wise
 *  scalar one outside Cartesian coordinates, so answering it here would set a precedent that
 *  6.26 would have to break.
 *
 *  @param e      the scalar field
 *  @param coords the ordered coordinate tuple
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Laplacian(e: _Expression, coords: Vector[_Variable],
                      system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "laplacian"
  override def rebuild(c: List[_Expression]): _Expression = _Laplacian(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] =
    if !wellPosed || componentsOf(e, env).isDefined then Left(this)
    else _Div(_Grad(e, coords, system), coords, system).eval(env)


/** `jacobian(F, x, y, …)` — the m×n matrix of first partials, `Jᵢⱼ = ∂Fᵢ/∂xⱼ`.
 *
 *  @param e      the vector field (m×1)
 *  @param coords the n coordinates, in order
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Jacobian(e: _Expression, coords: Vector[_Variable],
                     system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "jacobian"
  override def rebuild(c: List[_Expression]): _Expression = _Jacobian(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val result =
      for
        _     <- Option.when(wellPosed)(())
        comps <- componentsOf(e, env)
      yield _Matrix(comps.size, coords.size,
                    for f <- comps; j <- coords.indices.toVector yield d(f, j))
    result.fold(Left(this))(Left(_))


/** `hessian(f, x, y, …)` — the n×n matrix of second partials, `Hᵢⱼ = ∂²f/∂xᵢ∂xⱼ`.
 *
 *  Symmetric for any field whose mixed partials commute (Clairaut), which the test suite
 *  pins rather than assumes.
 *
 *  @param e      the scalar field
 *  @param coords the ordered coordinate tuple
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Hessian(e: _Expression, coords: Vector[_Variable],
                    system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "hessian"
  override def rebuild(c: List[_Expression]): _Expression = _Hessian(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] =
    if !wellPosed then Left(this)
    else
      val n = coords.size
      Left(_Matrix(n, n,
                   for i <- coords.indices.toVector; j <- coords.indices.toVector
                   yield simplifyFully(derive(e, coords(i), coords(j)))))
