package it.grypho.scala.leonardo
package vector

import core.*
import scalar.*
import matrix._Matrix


/** The coordinate system a vector operator is evaluated in (issues 6.24 and 6.26).
 *
 *  All three are **orthogonal curvilinear**, so a single set of formulas parameterised by the
 *  scale factors (Lamé coefficients) covers them — see [[_VectorOperator.scaleFactors]].
 *  Writing the cylindrical and spherical operators out by hand would have put three
 *  definitions of each in the source, free to drift apart; there is one.
 *
 *  **`Cylindrical` and `Spherical` are inherently three-dimensional**, and the coordinates
 *  are identified by *position*, not by name — the user may call them anything:
 *
 *  - `Cylindrical` — `(r, θ, z)`: radial, azimuthal, axial.
 *  - `Spherical` — `(r, θ, φ)` with **`θ` the polar angle** (measured from the axis) and `φ`
 *    the azimuthal one.  This is the physics convention; it cannot be inferred from the
 *    variable names, and choosing silently would make every spherical result wrong for half
 *    its readers, so it is stated here, in the cheat sheet and in the README.
 *  - `SphericalMaths` — the mathematics convention `(r, θ, φ)` with `θ` **azimuthal** and `φ`
 *    **polar**: the same geometry with the last two coordinates exchanged (issue 6.27).
 *
 *  **The difference between the two spherical cases is argument *order*, not naming.**  The
 *  operators read `coords` by position and never look at the spellings, so a reader who calls
 *  the polar angle `φ` but passes it second is already served by `Spherical`; `SphericalMaths`
 *  exists for those who pass it *third*.
 */
enum CoordinateSystem:
  case Cartesian, Cylindrical, Spherical, SphericalMaths


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

  override def toString: String =
    val tail = if system == CoordinateSystem.Cartesian then "" else s", ${system.toString.toLowerCase}"
    s"$opName($e, ${coords.mkString(", ")}$tail)"

  /** The scale factors `(h₁, h₂, …)` for this coordinate system and tuple (issue 6.26).
   *
   *  Every operator below is written once, in terms of these; Cartesian is the `h = 1` case,
   *  which is why generalising the formulas left its results untouched.  `None` when the
   *  system cannot describe this tuple — cylindrical and spherical are three-dimensional, and
   *  a request in any other arity is refused rather than answered in the wrong geometry.
   */
  protected def scaleFactors: Option[Vector[_Expression]] = system match
    case CoordinateSystem.Cartesian   => Some(Vector.fill(coords.size)(_Number(1)))
    // (r, θ, z): only the azimuthal arc scales, by r
    case CoordinateSystem.Cylindrical =>
      Option.when(coords.sizeIs == 3)(Vector(_Number(1), coords(0), _Number(1)))
    // (r, θ, φ), θ polar: the azimuthal arc scales by r·sin θ
    case CoordinateSystem.Spherical   =>
      Option.when(coords.sizeIs == 3)(
        Vector(_Number(1), coords(0), Product(coords(0), Sin(coords(1)))))
    // (r, θ, φ), θ azimuthal and φ polar — the same geometry, last two exchanged (6.27)
    case CoordinateSystem.SphericalMaths =>
      Option.when(coords.sizeIs == 3)(
        Vector(_Number(1), Product(coords(0), Sin(coords(2))), coords(0)))

  /** True when the request is one this tier can answer at all: a coordinate tuple with no
   *  repeats, in an arity the coordinate system supports.  `grad(f, x, x)` names no basis and
   *  is refused rather than silently producing a duplicated component. */
  protected def wellPosed: Boolean =
    coords.nonEmpty && coords.distinct.sizeIs == coords.size && scaleFactors.isDefined

  /** The scale factors, or an empty vector when the request is ill-posed (guarded by
   *  [[wellPosed]] at every use site). */
  protected def h: Vector[_Expression] = scaleFactors.getOrElse(Vector.empty)

  /** The Jacobian factor `h₁·h₂·…`, the volume element's coefficient. */
  protected def jacobianFactor: _Expression =
    if h.isEmpty then _Number(1) else h.reduce(Product.apply)

  /** Orientation of the ordered basis: `+1` right-handed, `-1` left-handed (issue 6.27).
   *
   *  **Only `curl` reads this, and it must.**  The curl formula below is derived for a
   *  right-handed `(q₁, q₂, q₃)`; `SphericalMaths` exchanges the last two coordinates of
   *  `Spherical`, and swapping two basis vectors flips the orientation, so the same formula
   *  returns `−curl` there.  `grad`, `div` and `laplacian` involve no cross product and are
   *  orientation-free, which is why the sign lives here rather than in each operator.
   */
  protected def handedness: Int = system match
    case CoordinateSystem.SphericalMaths => -1
    case _                               => 1

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
    // component i is (1/hᵢ)·∂f/∂qᵢ — the h = 1 case is the plain partial derivative
    else Left(column(coords.indices.toVector.map(i => Ratio(d(e, i), h(i)))))


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
        // (1/J)·Σᵢ ∂/∂qᵢ (J/hᵢ · Fᵢ) with J = h₁h₂…; the h = 1 case is Σᵢ ∂Fᵢ/∂qᵢ.
        // The whole product is differentiated, so the scale factors' own variation counts —
        // that is what makes 1/r divergence-free in cylindrical coordinates.
        j = jacobianFactor
      yield simplifyFully(
        Ratio(total(comps.indices.toVector.map(i => d(Product(Ratio(j, h(i)), comps(i)), i))), j))
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
        // component i is (1/hⱼhₖ)·[∂(hₖFₖ)/∂qⱼ − ∂(hⱼFⱼ)/∂qₖ] over the cyclic (i, j, k);
        // the h = 1 case is (∂F₃/∂x₂ − ∂F₂/∂x₃, ∂F₁/∂x₃ − ∂F₃/∂x₁, ∂F₂/∂x₁ − ∂F₁/∂x₂).
        // The formula assumes a RIGHT-handed ordering, so a left-handed system negates it
        // (see `handedness`) -- curl is a pseudo-vector, and this is the whole of 6.27's risk.
        val raw = Vector.tabulate(3) { i =>
          val (j, k) = ((i + 1) % 3, (i + 2) % 3)
          Ratio(Sum(d(Product(h(k), comps(k)), j),
                    Product(_Number(-1), d(Product(h(j), comps(j)), k))),
                Product(h(j), h(k)))
        }
        column(if handedness == 1 then raw else raw.map(Product(_Number(-1), _)))
    result.fold(Left(this))(Left(_))


/** `laplacian(f, x, y, …)` — the Laplacian of a scalar field, a **scalar**.
 *
 *  Defined as `div(grad(f))` rather than as its own sum of second derivatives, so the two can
 *  never disagree.  **That definition is why issue 6.26 needed no change here**: composing
 *  the curvilinear `grad` and `div` yields `(1/J)·Σᵢ ∂/∂qᵢ (J/hᵢ² · ∂f/∂qᵢ)`, which is exactly
 *  the general orthogonal-curvilinear Laplacian — a formula written out separately would have
 *  been a second definition to keep in step, and this one cannot drift.
 *
 *  A **vector** argument stays symbolic, and now for a load-bearing reason rather than a
 *  cautious one: outside Cartesian coordinates the vector Laplacian is *not* the
 *  component-wise scalar Laplacian (it is `grad(div F) − curl(curl F)`), so answering it by
 *  mapping this scalar formula over the components would be wrong in exactly the systems
 *  6.26 adds.
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
