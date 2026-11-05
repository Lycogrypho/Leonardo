package it.grypho.scala.leonardo
package vector

import core.*
import scalar.*
import matrix.{_Matrix, MatScale}


/** The coordinate system a vector operator is evaluated in.
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
 *    **polar**: the same geometry with the last two coordinates exchanged.
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

  /** The scale factors `(h₁, h₂, …)` for this coordinate system and tuple.
   *
   *  Every operator below is written once, in terms of these; Cartesian is the `h = 1` case,
   *  which is why generalising the formulas left its results untouched.  `None` when the
   *  system cannot describe this tuple — cylindrical and spherical are three-dimensional, and
   *  a request in any other arity is refused rather than answered in the wrong geometry.
   */
  protected def scaleFactors: Option[Vector[_Expression]] = scaleFactorsIn(false)

  /** [[scaleFactors]] with the unit factors in the computation's tier (F_0273): a `Double` `1`
   *  dividing a component demoted an exact field, `div([[x], [y], [z]], x, y, z)` answering `3.0`. */
  protected def scaleFactorsIn(exact: Boolean): Option[Vector[_Expression]] =
    val one: _Expression = if exact then _Rational(1) else _Number(1)
    system match
      case CoordinateSystem.Cartesian   => Some(Vector.fill(coords.size)(one))
      // (r, θ, z): only the azimuthal arc scales, by r
      case CoordinateSystem.Cylindrical =>
        Option.when(coords.sizeIs == 3)(Vector(one, coords(0), one))
      // (r, θ, φ), θ polar: the azimuthal arc scales by r·sin θ
      case CoordinateSystem.Spherical   =>
        Option.when(coords.sizeIs == 3)(
          Vector(one, coords(0), Product(coords(0), Sin(coords(1)))))
      // (r, θ, φ), θ azimuthal and φ polar — the same geometry, last two exchanged
      case CoordinateSystem.SphericalMaths =>
        Option.when(coords.sizeIs == 3)(
          Vector(one, Product(coords(0), Sin(coords(2))), coords(0)))

  /** Whether this operator computes in the exact tier: its field, the bound values of its names,
   *  or the session's exact mode (F_0273, through `containsExactIn`). */
  protected def exactIn(env: Environment): Boolean = _Rational.containsExactIn(e, env)

  /** True when the request is one this tier can answer at all: a coordinate tuple with no
   *  repeats, in an arity the coordinate system supports.  `grad(f, x, x)` names no basis and
   *  is refused rather than silently producing a duplicated component. */
  protected def wellPosed: Boolean =
    coords.nonEmpty && coords.distinct.sizeIs == coords.size && scaleFactors.isDefined

  /** The scale factors, or an empty vector when the request is ill-posed (guarded by
   *  [[wellPosed]] at every use site). */
  protected def hIn(using exact: Boolean): Vector[_Expression] = scaleFactorsIn(exact).getOrElse(Vector.empty)

  /** The Jacobian factor `h₁·h₂·…`, the volume element's coefficient. */
  protected def jacobianFactorIn(using exact: Boolean): _Expression =
    if hIn.isEmpty then (if exact then _Rational(1) else _Number(1)) else hIn.reduce(Product.apply)

  /** Orientation of the ordered basis: `+1` right-handed, `-1` left-handed.
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
   *  case (the general lesson that matrix-vs-scalar cannot always be decided at parse
   *  time).  An `Environment` holds `_Value`s, so such a binding is necessarily a *dense*
   *  `_MatrixValue`: its cells are constants and their derivatives are legitimately zero.
   *
   *  A field is a column (`n×1`); a row vector is a different object and is refused.
   */
  protected def componentsOf(x: _Expression, env: Environment): Option[Vector[_Expression]] =
    x match
      case m: _Matrix if m.cols == 1 => Some(m.elems)
      case _: _Matrix                => None      // a row vector is not a field
      // A nested operator as built, never evaluated (F_0285): `div(grad(f, x, y), x, y)` with `x`
      // bound would otherwise differentiate cells the binding had already folded.
      case op: _VectorOperator => op.unevaluated(env) match
        case Some(m: _Matrix) if m.cols == 1 => Some(m.elems)
        case _                               => None
      // A literal column scaled by a scalar (F_0293): `[[x], [y], [z]]/r` is the field `(x/r, y/r, z/r)`,
      // read cell by cell before anything is evaluated, for the reason a literal is.
      case Ratio(m: _Matrix, d) if m.cols == 1 && !d.isInstanceOf[_MatrixShaped] && !d.isInstanceOf[_MatrixValue] =>
        Some(m.elems.map(Ratio(_, d)))
      case MatScale(k, m: _Matrix) if m.cols == 1 && !k.isInstanceOf[_MatrixShaped] && !k.isInstanceOf[_MatrixValue] =>
        Some(m.elems.map(Product(k, _)))
      case _ => x.eval(env).toExpression match
        case m: _Matrix if m.cols == 1      => Some(m.elems)
        case v: _MatrixValue if v.cols == 1 => Some(_Matrix.fromValue(v).elems)
        case _                              => None

  /** Builds a column vector from its components, simplified. */
  protected def columnIn(cs: Vector[_Expression])(using exact: Boolean): _Matrix =
    _Matrix(cs.size, 1, cs.map(simplifyFullyInTier(_, exact)))

  /** `∂x/∂coords(i)`, simplified, in the operator's tier (F_0273). */
  protected def dIn(x: _Expression, i: Int)(using exact: Boolean): _Expression =
    simplifyFullyInTier(deriveInTier(x, coords(i), exact), exact)

  /** Sums a list of terms, folding to `0` when empty. */
  protected def totalIn(terms: Vector[_Expression])(using exact: Boolean): _Expression =
    if terms.isEmpty then (if exact then _Rational(0) else _Number(0))
    else simplifyFullyInTier(terms.reduce(Sum.apply), exact)

  /** The operator's result as built, before it is evaluated in the environment (F_0285): the
   *  derivatives with the coordinates free.  The library's own callers (`linearize`, `stationary`,
   *  `nlp`, `lsqfit`) read it cell by cell and put in their own point, so a session binding of a
   *  coordinate's name must not fold it first.
   *
   *  @param env bindings, read for a field held in a variable and for the tier
   *  @return the built result, or `None` when the operator declines
   */
  private[leonardo] def unevaluated(env: Environment): Option[_Expression] = None

  /** [[unevaluated]] evaluated in `env` (F_0285): a bound coordinate folds, so `hessian(f, x, y)`
   *  with `x` and `y` bound is the matrix `det` and `eigen` of it see.  The node itself when the
   *  operator declines. */
  protected def evaluatedIn(env: Environment): Either[_Expression, _Value] =
    unevaluated(env).fold(Left(this))(_.eval(env))

  // The published signatures, in the Double tier (F_0273 renamed the tier-aware ones `…In`):
  // protected members of a sealed trait, so no outside code reaches them, but MiMa counts them.

  /** [[hIn]] in the `Double` tier. */
  protected def h: Vector[_Expression] = hIn(using false)
  /** [[jacobianFactorIn]] in the `Double` tier. */
  protected def jacobianFactor: _Expression = jacobianFactorIn(using false)
  /** [[columnIn]] in the `Double` tier. @param cs the components */
  protected def column(cs: Vector[_Expression]): _Matrix = columnIn(cs)(using false)
  /** [[dIn]] in the `Double` tier. @param x the field @param i the coordinate's index */
  protected def d(x: _Expression, i: Int): _Expression = dIn(x, i)(using false)
  /** [[totalIn]] in the `Double` tier. @param terms the terms */
  protected def total(terms: Vector[_Expression]): _Expression = totalIn(terms)(using false)


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

  override def eval(env: Environment): Either[_Expression, _Value] = evaluatedIn(env)

  override private[leonardo] def unevaluated(env: Environment): Option[_Expression] =
    // A vector argument is refused, as `laplacian` refuses one: the gradient of a vector field
    // is not the component-wise gradient outside Cartesian coordinates (F_0191).
    given Boolean = exactIn(env)
    if !wellPosed || componentsOf(e, env).isDefined then None
    // component i is (1/hᵢ)·∂f/∂qᵢ — the h = 1 case is the plain partial derivative
    else Some(columnIn(coords.indices.toVector.map(i => Ratio(dIn(e, i), hIn(i)))))


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

  override def eval(env: Environment): Either[_Expression, _Value] = evaluatedIn(env)

  override private[leonardo] def unevaluated(env: Environment): Option[_Expression] =
    given exact: Boolean = exactIn(env)
    for
        comps <- Option.when(wellPosed)(()).flatMap(_ => componentsOf(e, env))
        if comps.sizeIs == coords.size
        // (1/J)·Σᵢ ∂/∂qᵢ (J/hᵢ · Fᵢ) with J = h₁h₂…; the h = 1 case is Σᵢ ∂Fᵢ/∂qᵢ.
        // The whole product is differentiated, so the scale factors' own variation counts —
        // that is what makes 1/r divergence-free in cylindrical coordinates.
        j = jacobianFactorIn
    yield simplifyFullyInTier(
        Ratio(totalIn(comps.indices.toVector.map(i => dIn(Product(Ratio(j, hIn(i)), comps(i)), i))), j), exact)


/** `curl(F, x, y, z)` — the curl of a 3-D vector field, as a 3×1 column vector.
 *
 *  **Three dimensions only.**  In 2-D the natural object is a *scalar* curl — a different
 *  result type, not a degenerate case of this one — and in higher dimensions the
 *  cross-product form does not exist at all.  Every other arity is refused rather than
 *  guessed — the rule that an analysis must describe what the library
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

  override def eval(env: Environment): Either[_Expression, _Value] = evaluatedIn(env)

  override private[leonardo] def unevaluated(env: Environment): Option[_Expression] =
    given Boolean = exactIn(env)
    for
        _     <- Option.when(wellPosed && coords.sizeIs == 3)(())
        comps <- componentsOf(e, env)
        if comps.sizeIs == 3
    yield
        // component i is (1/hⱼhₖ)·[∂(hₖFₖ)/∂qⱼ − ∂(hⱼFⱼ)/∂qₖ] over the cyclic (i, j, k);
        // the h = 1 case is (∂F₃/∂x₂ − ∂F₂/∂x₃, ∂F₁/∂x₃ − ∂F₃/∂x₁, ∂F₂/∂x₁ − ∂F₁/∂x₂).
        // The formula assumes a RIGHT-handed ordering, so a left-handed system negates it
        // (see `handedness`) -- curl is a pseudo-vector, and this is the exchange's whole risk.
        // The `−1` in the field's tier (F_0210): a Double one pulled an exact field into
        // decimals, `curl([[y/3], [0], [0]], x, y, z)` answering `-0.33333`.
        val neg = _Rational.literalLike(-1, e, env)
        val raw = Vector.tabulate(3) { i =>
          val (j, k) = ((i + 1) % 3, (i + 2) % 3)
          Ratio(Sum(dIn(Product(hIn(k), comps(k)), j),
                    Product(neg, dIn(Product(hIn(j), comps(j)), k))),
                Product(hIn(j), hIn(k)))
        }
        columnIn(if handedness == 1 then raw else raw.map(Product(neg, _)))


/** `laplacian(f, x, y, …)` — the Laplacian of a scalar field, a **scalar**.
 *
 *  Defined as `div(grad(f))` rather than as its own sum of second derivatives, so the two can
 *  never disagree.  **That definition is why the curvilinear systems needed no change here**: composing
 *  the curvilinear `grad` and `div` yields `(1/J)·Σᵢ ∂/∂qᵢ (J/hᵢ² · ∂f/∂qᵢ)`, which is exactly
 *  the general orthogonal-curvilinear Laplacian — a formula written out separately would have
 *  been a second definition to keep in step, and this one cannot drift.
 *
 *  A **vector** argument stays symbolic, and now for a load-bearing reason rather than a
 *  cautious one: outside Cartesian coordinates the vector Laplacian is *not* the
 *  component-wise scalar Laplacian (it is `grad(div F) − curl(curl F)`), so answering it by
 *  mapping this scalar formula over the components would be wrong in exactly the systems
 *  the curvilinear systems add.
 *
 *  @param e      the scalar field
 *  @param coords the ordered coordinate tuple
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Laplacian(e: _Expression, coords: Vector[_Variable],
                      system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "laplacian"
  override def rebuild(c: List[_Expression]): _Expression = _Laplacian(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] = evaluatedIn(env)

  // The gradient as built, handed to `div` as a literal column: evaluated first, a bound
  // coordinate would fold its cells and `div` would differentiate constants (F_0285).
  override private[leonardo] def unevaluated(env: Environment): Option[_Expression] =
    if !wellPosed || componentsOf(e, env).isDefined then None
    else _Grad(e, coords, system).unevaluated(env).flatMap(g => _Div(g, coords, system).unevaluated(env))


/** `jacobian(F, x, y, …)` — the m×n matrix of first partials, `Jᵢⱼ = ∂Fᵢ/∂xⱼ`.
 *
 *  **Cartesian only** (F_0191): in a curvilinear system the physical-component Jacobian carries
 *  scale-factor and connection terms, so the plain partials would be a confidently wrong matrix —
 *  `jacobian([[r],[0],[0]], r, t, z, cylindrical)` has trace 1 where `div` of the same field is 2.
 *  Any other system stays symbolic.
 *
 *  @param e      the vector field (m×1)
 *  @param coords the n coordinates, in order
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Jacobian(e: _Expression, coords: Vector[_Variable],
                     system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "jacobian"
  override def rebuild(c: List[_Expression]): _Expression = _Jacobian(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] = evaluatedIn(env)

  override private[leonardo] def unevaluated(env: Environment): Option[_Expression] =
    given Boolean = exactIn(env)
    for
      _     <- Option.when(wellPosed && system == CoordinateSystem.Cartesian)(())
      comps <- componentsOf(e, env)
    yield _Matrix(comps.size, coords.size,
                  for f <- comps; j <- coords.indices.toVector yield dIn(f, j))


/** `hessian(f, x, y, …)` — the n×n matrix of second partials, `Hᵢⱼ = ∂²f/∂xᵢ∂xⱼ`.
 *
 *  Symmetric for any field whose mixed partials commute (Clairaut), which the test suite
 *  pins rather than assumes.
 *
 *  **Cartesian only, and a scalar field only** (F_0191): `hessian(r, r, t, p, spherical)`
 *  answered the zero matrix, whose trace disagrees with `laplacian`'s `2/r` — the curvilinear
 *  Hessian needs connection terms — and a vector argument has no Hessian matrix.  Both stay
 *  symbolic.
 *
 *  @param e      the scalar field
 *  @param coords the ordered coordinate tuple
 *  @param system the coordinate system (only `Cartesian` evaluates)
 */
case class _Hessian(e: _Expression, coords: Vector[_Variable],
                    system: CoordinateSystem = CoordinateSystem.Cartesian) extends _VectorOperator:
  override protected def opName: String = "hessian"
  override def rebuild(c: List[_Expression]): _Expression = _Hessian(c.head, coords, system)

  override def eval(env: Environment): Either[_Expression, _Value] = evaluatedIn(env)

  override private[leonardo] def unevaluated(env: Environment): Option[_Expression] =
    if !wellPosed || system != CoordinateSystem.Cartesian || componentsOf(e, env).isDefined then None
    else
      val n     = coords.size
      val exact = exactIn(env)
      Some(_Matrix(n, n,
                   for i <- coords.indices.toVector; j <- coords.indices.toVector
                   yield simplifyFullyInTier(deriveInTier(deriveInTier(e, coords(i), exact), coords(j), exact), exact)))
