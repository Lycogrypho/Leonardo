package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.{Abs, Power, Ratio}


/** Which norm `norm` takes (F_0039 Decision B): the written order, kept so a saved session
 *  re-parses to the call it was typed as.
 */
enum NormOrder:
  /** `norm(x)`: the 2-norm, which for a matrix is the spectral norm. */
  case Default
  /** `norm(x, fro)`: the Frobenius norm, the 2-norm of every cell taken together. */
  case Frobenius
  /** `norm(x, p)`: `1`, `2`, `inf`, or for a vector any real `p ≥ 1`; an expression, so a
   *  bound name may supply it.
   *  @param p the order */
  case Of(p: _Expression)


/** The norm a call resolves to once its order has been read. */
private enum NormKind:
  case One, Two, Inf, Fro
  case P(q: Double, value: _Value)


/** `norm(x)` and `norm(x, kind)` (F_0039): a vector's or a matrix's norm.
 *
 *  **The shape decides the reading** (Decision A, MATLAB's rule, stated because the other
 *  reading exists): a row or a column is a vector — and so is a number, as a `1×1` — anything
 *  else a matrix.  The default is the 2-norm either way, since the vector 2-norm and the spectral
 *  norm agree on a column, so the shape decides only what `1` and `inf` mean: the sum and the
 *  largest modulus of a vector, the largest column and row sum of a matrix.
 *
 *  - **A vector**: `1`, `2`, `inf`, `fro` (the 2-norm) and any `p ≥ 1`; a `p` below 1 is not a
 *    norm and stays unevaluated.  A complex entry counts by its modulus.  In `Double` the 2- and
 *    `p`-norms are **scaled by the largest modulus** before they are raised, so `[[1e200],
 *    [1e200]]` is `1.414…1e200` rather than an overflow (the conditioning rule).  Exact cells
 *    stay exact for `1` and `inf`; the 2-norm's root is taken at the working precision.
 *    Symbolic cells give `sum(abs(xᵢ))` or `(Σ abs(xᵢ)^p)^(1/p)`; their max-norm stays
 *    unevaluated, since a maximum of unknown magnitudes is not decidable.
 *  - **A matrix**: `1` and `inf` on numeric cells, exact on exact ones; `fro` as above; the
 *    2-norm is the **largest singular value through the SVD**, never `eigen(AᵀA)`, which squares
 *    the condition number (2.11) — an exact matrix is demoted for it, the `svd` precedent, and a
 *    complex one stays unevaluated.  Any other `p` stays unevaluated.
 *
 *  A SCALAR result, like [[Determinant]], so `1 + norm(x)` is a number.
 *
 *  @param x     the vector or matrix
 *  @param order which norm
 */
case class _Norm(x: _Expression, order: NormOrder = NormOrder.Default) extends _Expression:
  override def toString: String = order match
    case NormOrder.Default   => s"norm($x)"
    case NormOrder.Frobenius => s"norm($x, fro)"
    case NormOrder.Of(p)     => s"norm($x, $p)"

  override def children: List[_Expression] = order match
    case NormOrder.Of(p) => List(x, p)
    case _               => List(x)

  override def rebuild(c: List[_Expression]): _Expression = order match
    case NormOrder.Of(_) => _Norm(c.head, NormOrder.Of(c(1)))
    case o               => _Norm(c.head, o)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = x.eval(env)
    val (kind, written) = order match
      case NormOrder.Default   => (Some(NormKind.Two), order)
      case NormOrder.Frobenius => (Some(NormKind.Fro), order)
      case NormOrder.Of(p) =>
        val rp = p.eval(env)
        (rp.toOption.flatMap(kindOf), NormOrder.Of(rp.toExpression))
    val grid = r match
      case Right(c: _Complex) => Some(_Matrix(1, 1, Vector(c)))
      case other              => cellGrid(other, scalars = true)
    (grid, kind) match
      case (Some(g), Some(k)) =>
        val norm =
          if g.rows == 1 || g.cols == 1 then vectorNorm(g.elems, k, env)
          else matrixNorm(g, r, k, env)
        norm.getOrElse(Left(this))
      case (Some(_), None) => written match
        case NormOrder.Of(p) if !p.isInstanceOf[_Value] => Left(_Norm(r.toExpression, written)) // a free order
        case _                                         => Left(this)  // not a norm: p < 1, or not a number
      case (None, _) => Left(_Norm(r.toExpression, written))


/** The norm an evaluated order names, or `None` when it names none (a `p` below 1, a NaN). */
private def kindOf(v: _Value): Option[NormKind] = v match
  case _Number(d) if d == 1.0           => Some(NormKind.One)
  case _Number(d) if d == 2.0           => Some(NormKind.Two)
  case _Number(d) if d.isPosInfinity    => Some(NormKind.Inf)
  case _Number(d) if d > 1 && d.isFinite => Some(NormKind.P(d, v))
  case _                                => None

/** A vector's norm, or `None` when it has no answer here (see [[_Norm]]). */
private def vectorNorm(cells: Vector[_Expression], k: NormKind, env: Environment): Option[Either[_Expression, _Value]] =
  val values = cells.collect { case v: _Value => v }
  val parts  = values.flatMap(_Complex.parts)
  // A surd cell is read as its working-precision rational (F_0231), the value it stood for
  // before; the norm of rational cells is itself exact or a surd.
  if parts.size == cells.size && values.forall(_Surd.isExact) then
    exactVectorNorm(values.flatMap(_Surd.exactOf(_, env.workingPrecision)), k, env)
  else if parts.size == cells.size then doubleVectorNorm(parts.map((re, im) => math.hypot(re, im)), k)
  else symbolicVectorNorm(cells, k, env)

/** The norm of moduli in `Double`, the 2- and `p`-norms scaled by the largest so that nothing
 *  representable overflows or underflows on the way: `s·(Σ (mᵢ/s)^p)^(1/p)`. */
private def doubleVectorNorm(ms: Vector[Double], k: NormKind): Option[Either[_Expression, _Value]] =
  val s = ms.max
  def scaled(p: Double): Double =
    if s == 0.0 || !s.isFinite then s
    else if p == 2.0 then s * math.sqrt(ms.foldLeft(0.0)((acc, m) => acc + (m / s) * (m / s)))
    else s * math.pow(ms.foldLeft(0.0)((acc, m) => acc + math.pow(m / s, p)), 1 / p)
  val n = k match
    case NormKind.One           => ms.sum
    case NormKind.Inf           => s
    case NormKind.Two | NormKind.Fro => scaled(2.0)
    case NormKind.P(q, _)       => scaled(q)
  finiteOption(n).map(d => Right(_Number(d)))

/** The norm of exact cells: the 1- and inf-norms exactly, the 2-norm as the exact surd of the
 *  sum of squares (F_0231: `norm([[2], [3]])` is `√13`), a `p`-norm through `Power` in `p`'s own
 *  tier. */
private def exactVectorNorm(rs: Vector[_Rational], k: NormKind, env: Environment): Option[Either[_Expression, _Value]] =
  val policy = env.rationalPolicy
  val abs    = rs.map(_.abs)
  k match
    case NormKind.One => Some(Right(abs.reduceLeft((a, b) => a.add(b, policy))))
    case NormKind.Inf => Some(Right(abs.max))
    case NormKind.Two | NormKind.Fro =>
      _Surd.sqrtOf(abs.map(a => a.multiply(a, policy)).reduceLeft((a, b) => a.add(b, policy))).map(Right(_))
    case NormKind.P(_, _) => symbolicVectorNorm(abs, k, env)

/** The norm of cells that are not all numbers, built from `abs`, `+` and powers: the sum is
 *  accumulated one term at a time and refused past the expression-depth limit, and the max-norm
 *  is not built at all. */
private def symbolicVectorNorm(cells: Vector[_Expression], k: NormKind, env: Environment): Option[Either[_Expression, _Value]] =
  def lit(n: Int): _Value = _Rational.literalLike(n, _Matrix(1, cells.size, cells))
  def rooted(p: _Expression): Option[Either[_Expression, _Value]] =
    accumulated(cells.map(c => Power(Abs(c), p)), env).map(s => Power(s.toExpression, Ratio(lit(1), p)).eval(env))
  k match
    case NormKind.One                => accumulated(cells.map(c => Abs(c)), env)
    case NormKind.Inf                => None
    case NormKind.Two | NormKind.Fro => rooted(lit(2))
    case NormKind.P(_, p)            => rooted(p)

/** A matrix's norm, or `None` when it has no answer here (see [[_Norm]]). */
private def matrixNorm(g: _Matrix, r: Either[_Expression, _Value], k: NormKind,
                       env: Environment): Option[Either[_Expression, _Value]] =
  def largest(lines: Vector[Vector[_Expression]]): Option[Either[_Expression, _Value]] =
    sequence(lines.map(l => vectorNorm(l, NormKind.One, env).flatMap(_.toOption))).flatMap(largestOf)
  k match
    case NormKind.Fro => vectorNorm(g.elems, NormKind.Two, env)
    case NormKind.One => largest(Vector.tabulate(g.cols)(j => Vector.tabulate(g.rows)(i => g(i, j))))
    case NormKind.Inf => largest(Vector.tabulate(g.rows)(i => Vector.tabulate(g.cols)(j => g(i, j))))
    case NormKind.Two if !g.elems.exists(_.isInstanceOf[_Complex]) =>
      _MatrixValue.ofEvaluated(r).flatMap(_.svd(env.limits)).map((_, sigma, _) => Right(_Number(sigma.max)))
    case _ => None

/** The largest of some norms, exactly when every one is exact, else as a `Double`. */
private def largestOf(vs: Vector[_Value]): Option[Either[_Expression, _Value]] =
  val exact = vs.collect { case q: _Rational => q }
  if exact.size == vs.size then Some(Right(exact.max))
  else
    val ds = vs.collect { case _Number(d) => d }
    Option.when(ds.size == vs.size)(Right(_Number(ds.max)))
