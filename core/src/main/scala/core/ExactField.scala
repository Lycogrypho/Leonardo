package it.grypho.scala.leonardo
package core


/** An exact field the matrix kernels eliminate over (F_0231 Decision B): the rationals, or
 *  `Q(√d)` for one radical `d`.  **One code path for both** -- `det`, `inv`, `lu` and `A^n` are
 *  written once over this, the `statistics.Rls` `Field` precedent, so the surd case cannot drift
 *  from the rational one.
 *
 *  @tparam T the element type
 */
private[leonardo] trait ExactField[T]:
  /** The additive identity. */
  def zero: T
  /** The multiplicative identity. */
  def one: T
  /** `a + b`. */
  def add(a: T, b: T): T
  /** `a − b`. */
  def sub(a: T, b: T): T
  /** `a · b`. */
  def mul(a: T, b: T): T
  /** `a / b`, or `None` for a zero divisor. */
  def div(a: T, b: T): Option[T]
  /** `-a`. */
  def negate(a: T): T
  /** Whether `a` is exactly zero. */
  def isZero(a: T): Boolean
  /** `|a|` as a `Double`: only the LU pivot choice reads it, to pick the pivot the dense kernel
   *  picks, and exactness never depends on it. */
  def magnitude(a: T): Double
  /** The largest bit length in `a`, the size the power budget estimates from. */
  def bits(a: T): Int
  /** The denominators of `a`'s rational parts (F_0249): what the power budget clears first. */
  def denominators(a: T): List[BigInt]
  /** The largest bit length among the parts of `a·d`, integers when `d` is a common denominator of
   *  `a`'s parts (F_0249), plus the radicand's for a surd field, as [[bits]] counts it. */
  def scaledBits(a: T, d: BigInt): Int
  /** `a` as a value of the language: a rational, or a surd / rational over `√d`. */
  def toValue(a: T): _Value
  /** A rational as an element of the field (F_0250): a count or a constant a kernel divides by. */
  def fromRational(r: _Rational): T

/** The rationals, under a reduction policy. */
private[leonardo] final class RationalField(policy: GcdPolicy) extends ExactField[_Rational]:
  def zero: _Rational = _Rational.Zero
  def one: _Rational = _Rational.One
  def add(a: _Rational, b: _Rational): _Rational = a.add(b, policy)
  def sub(a: _Rational, b: _Rational): _Rational = a.subtract(b, policy)
  def mul(a: _Rational, b: _Rational): _Rational = a.multiply(b, policy)
  def div(a: _Rational, b: _Rational): Option[_Rational] = a.divide(b, policy)
  def negate(a: _Rational): _Rational = a.negate
  def isZero(a: _Rational): Boolean = a.isZero
  def magnitude(a: _Rational): Double = math.abs(a.toDouble)
  def bits(a: _Rational): Int = a.maxBitLength
  def denominators(a: _Rational): List[BigInt] = List(a.den)
  def scaledBits(a: _Rational, d: BigInt): Int = (a.num * (d / a.den)).bitLength
  def toValue(a: _Rational): _Value = a
  def fromRational(r: _Rational): _Rational = r

/** An element `a + b·√d` of `Q(√d)`. */
private[leonardo] final case class Quad(a: _Rational, b: _Rational)

/** `Q(√d)` for a positive non-square `d`, under a reduction policy.  Division by the conjugate:
 *  `a + b√d` is zero only when `a = b = 0`, since `d` is not a perfect square. */
private[leonardo] final class SurdField(d: BigInt, policy: GcdPolicy) extends ExactField[Quad]:
  private val dq = _Rational(d)
  def zero: Quad = Quad(_Rational.Zero, _Rational.Zero)
  def one: Quad = Quad(_Rational.One, _Rational.Zero)
  def add(x: Quad, y: Quad): Quad = Quad(x.a.add(y.a, policy), x.b.add(y.b, policy))
  def sub(x: Quad, y: Quad): Quad = Quad(x.a.subtract(y.a, policy), x.b.subtract(y.b, policy))
  def mul(x: Quad, y: Quad): Quad =
    Quad(x.a.multiply(y.a, policy).add(x.b.multiply(y.b, policy).multiply(dq, policy), policy),
         x.a.multiply(y.b, policy).add(x.b.multiply(y.a, policy), policy))
  def div(x: Quad, y: Quad): Option[Quad] =
    val den = y.a.multiply(y.a, policy).subtract(y.b.multiply(y.b, policy).multiply(dq, policy), policy)
    if den.isZero then None
    else
      val n = mul(x, Quad(y.a, y.b.negate))
      for qa <- n.a.divide(den, policy); qb <- n.b.divide(den, policy) yield Quad(qa, qb)
  def negate(x: Quad): Quad = Quad(x.a.negate, x.b.negate)
  def isZero(x: Quad): Boolean = x.a.isZero && x.b.isZero
  def magnitude(x: Quad): Double = math.abs(x.a.toDouble + x.b.toDouble * math.sqrt(d.toDouble))
  def bits(x: Quad): Int = math.max(x.a.maxBitLength, x.b.maxBitLength) + d.bitLength
  def denominators(x: Quad): List[BigInt] = List(x.a.den, x.b.den)
  def scaledBits(x: Quad, c: BigInt): Int =
    math.max((x.a.num * (c / x.a.den)).bitLength, (x.b.num * (c / x.b.den)).bitLength) + d.bitLength
  def toValue(x: Quad): _Value = _Surd.of(x.a, x.b, d).getOrElse(x.a)
  def fromRational(r: _Rational): Quad = Quad(r, _Rational.Zero)

/** Exact cells read over one field: every cell rational, or every cell in `Q(√d)` for a single
 *  radical class (F_0231 Decision B). */
private[leonardo] enum FieldCells:
  /** Every cell a rational. */
  case OverQ(cells: Vector[_Rational])
  /** At least one surd, every cell over the one radical `d`. */
  case OverSurd(d: BigInt, cells: Vector[Quad])

/** Reads exact values over one field, or `None` when a value is not exact or two surds do not
 *  share a radical (`√2` beside `√3`): the caller then reads them through `_Surd.exactOf`.
 *
 *  @param values the cells
 *  @return the cells over `Q` or over `Q(√d)`
 */
private[leonardo] def fieldCells(values: Vector[_Expression]): Option[FieldCells] =
  if !values.forall(_Surd.isExact) then None
  else values.collectFirst { case s: _Surd => s.d } match
    case None    => Some(FieldCells.OverQ(values.collect { case r: _Rational => r }))
    case Some(d) =>
      // `√d'` is `(s/d)·√d` exactly when `d·d' = s²`, the combinable test `_Surd` uses.
      val quads = values.map {
        case r: _Rational => Some(Quad(r, _Rational.Zero))
        case s: _Surd     =>
          if s.d == d then Some(Quad(s.a, s.b))
          else _Surd.exactRoot(d * s.d).flatMap(root => _Rational.of(root, d)).map(k => Quad(s.a, s.b.multiply(k)))
        case _ => None
      }
      Option.when(quads.forall(_.isDefined))(FieldCells.OverSurd(d, quads.flatten))

/** Runs `run` over the field exact cells share (F_0231 Decision B): `Q` when every cell is
 *  rational, `Q(√d)` when the surds share one radical, and -- when two radicals meet -- over the
 *  cells' working-precision rationals, which is what every kernel computed before F_0231.  `None`
 *  when a cell is not exact at all.
 *
 *  @param values the cells
 *  @param digits the working precision of the fallback reading
 *  @param policy the rational reduction policy
 *  @param run    the kernel, polymorphic in the element type
 *  @return the kernel's result
 */
private[leonardo] def overExactField[R](values: Vector[_Expression], digits: Int, policy: GcdPolicy)
                                       (run: [T] => (ExactField[T], Vector[T]) => R): Option[R] =
  fieldCells(values) match
    case Some(FieldCells.OverQ(c))       => Some(run(RationalField(policy), c))
    case Some(FieldCells.OverSurd(d, c)) => Some(run(SurdField(d, policy), c))
    case None if values.forall(_Surd.isExact) =>
      val rs = values.flatMap(_Surd.exactOf(_, digits))
      Option.when(rs.size == values.size)(run(RationalField(policy), rs))
    case None => None

/** The exact product of two square matrices over a field. */
private[leonardo] def fieldProduct[T](f: ExactField[T], a: Vector[T], b: Vector[T], n: Int): Vector[T] =
  Vector.tabulate(n, n)((i, j) =>
    (0 until n).foldLeft(f.zero)((s, k) => f.add(s, f.mul(a(i * n + k), b(k * n + j))))
  ).flatten

/** The exact inverse of a square matrix over a field, by Gauss–Jordan elimination; the pivot only
 *  has to be non-zero.  `None` when the matrix is singular. */
private[leonardo] def fieldInverse[T](f: ExactField[T], cells: Vector[T], n: Int): Option[Vector[T]] =
  val augmented = Vector.tabulate(n, 2 * n) { (i, j) =>
    if j < n then cells(i * n + j) else if j - n == i then f.one else f.zero
  }
  @annotation.tailrec
  def go(m: Vector[Vector[T]], k: Int): Option[Vector[Vector[T]]] =
    if k >= n then Some(m)
    else (k until n).find(i => !f.isZero(m(i)(k))) match
      case None => None
      case Some(p) =>
        val swapped = if p == k then m else m.updated(k, m(p)).updated(p, m(k))
        f.div(f.one, swapped(k)(k)) match
          case None => None
          case Some(inv) =>
            val norm  = swapped.updated(k, swapped(k).map(f.mul(_, inv)))
            val pivot = norm(k)
            val cleared = norm.zipWithIndex.map { (row, i) =>
              if i == k || f.isZero(row(k)) then row
              else
                val g = row(k)
                row.zipWithIndex.map((cell, j) => f.sub(cell, f.mul(g, pivot(j))))
            }
            go(cleared, k + 1)
  go(augmented, 0).map(m => Vector.tabulate(n, n)((i, j) => m(i)(j + n)).flatten)

/** The `k`-th power of a square matrix over a field, by binary exponentiation; a negative power is
 *  the inverse's.  Held to two budgets before any product ([[withinPowerBudget]]). */
private[leonardo] def fieldMatrixPower[T](f: ExactField[T], cells: Vector[T], n: Int, k: Long,
                                          maxBits: Long, maxWork: Long): Option[Vector[T]] =
  val base = if k >= 0 then Some(cells) else fieldInverse(f, cells, n)
  base.filter(withinPowerBudget(f, _, n, math.abs(k), maxBits, maxWork)).map { b =>
    val identity = Vector.tabulate(n * n)(i => if i / n == i % n then f.one else f.zero)
    @annotation.tailrec
    def go(result: Vector[T], factor: Vector[T], e: Long): Vector[T] =
      if e == 0 then result
      else
        val next = if (e & 1L) == 1L then fieldProduct(f, result, factor, n) else result
        if e == 1 then next else go(next, fieldProduct(f, factor, factor, n), e >> 1)
    go(identity, b, math.abs(k))
  }

/** Whether `b^k` stays within the budgets (F_0249), estimated before any product.
 *
 *  **Denominators are cleared first**: with `D` the lcm of the cells' denominators, `b = M/D` for
 *  an integer matrix `M`, so `b^k = M^k/D^k`.  Each product adds at most `bits(M) + ⌈log₂ n⌉` bits
 *  to an entry of `M^k`, and `D^k` has `k·bits(D)`; their sum is held to `maxBits`, the
 *  `_Rational.pow` rule.  The integer-only estimate `|k|·(b + ⌈log₂ n⌉)` it replaced ignored that a
 *  reduced denominator is an lcm of path products of distinct denominators: on a 5×5 matrix of
 *  `1/prime` cells it under-counted by up to seven times and admitted `A^5000`.  **And bits are not
 *  the cost of a fraction, its gcd reduction is** (F_0248's measurement): the `n³` reductions of
 *  the last product, each of `size · bits(D^k)`, are held to `maxWork`.  An integer matrix has
 *  `D = 1` and is held by its size alone, as before.
 *
 *  @return whether the power may be attempted
 */
private def withinPowerBudget[T](f: ExactField[T], b: Vector[T], n: Int, k: Long, maxBits: Long, maxWork: Long): Boolean =
  val d     = b.flatMap(f.denominators).foldLeft(BigInt(1))((acc, q) => acc / acc.gcd(q) * q)
  val logN  = 32 - Integer.numberOfLeadingZeros(n)
  val entry = BigInt(k) * (b.map(f.scaledBits(_, d)).maxOption.getOrElse(0) + logN)
  val denom = if d == BigInt(1) then BigInt(0) else BigInt(k) * d.bitLength
  val size  = entry + denom
  size <= maxBits && (denom == BigInt(0) || BigInt(n).pow(3) * size * denom <= maxWork)

/** The exact determinant of a square matrix over a field, by Gaussian elimination: a row swap
 *  flips the sign, and a column with no non-zero pivot means a singular matrix, determinant zero. */
private[leonardo] def fieldDeterminant[T](f: ExactField[T], cells: Vector[T], n: Int): T =
  @annotation.tailrec
  def go(m: Vector[Vector[T]], k: Int, acc: T): T =
    if f.isZero(acc) || k >= n then acc
    else (k until n).find(i => !f.isZero(m(i)(k))) match
      case None => f.zero
      case Some(p) =>
        val swapped = if p == k then m else m.updated(k, m(p)).updated(p, m(k))
        val sign    = if p == k then acc else f.negate(acc)
        val pivot   = swapped(k)
        val reduced = swapped.zipWithIndex.map { (row, i) =>
          if i <= k then row
          else f.div(row(k), pivot(k)) match
            case None    => row
            case Some(g) => row.zipWithIndex.map((cell, j) => if j < k then cell else f.sub(cell, f.mul(g, pivot(j))))
        }
        go(reduced, k + 1, f.mul(sign, pivot(k)))
  if n == 0 then f.one else go(Vector.tabulate(n, n)((i, j) => cells(i * n + j)), 0, f.one)

/** `P·A = L·U` over a field (F_0226): the pivot is the largest in magnitude, so `P` is the one the
 *  dense kernel picks, and a column with no non-zero pivot is skipped, so a singular matrix factors
 *  too.  Returns the cells of `L`, `U` and `P`, row-major. */
private[leonardo] def fieldLu[T](f: ExactField[T], cells: Vector[T], n: Int): (Vector[T], Vector[T], Vector[T]) =
  def step(state: (Vector[Vector[T]], Vector[Int]), k: Int): (Vector[Vector[T]], Vector[Int]) =
    val (m, perm) = state
    val p = (k until n).maxBy(i => f.magnitude(m(i)(k)))
    if f.isZero(m(p)(k)) then state
    else
      val swapped = if p == k then m else m.updated(k, m(p)).updated(p, m(k))
      val order   = if p == k then perm else perm.updated(k, perm(p)).updated(p, perm(k))
      val pivot   = swapped(k)
      val reduced = swapped.zipWithIndex.map { (row, i) =>
        if i <= k then row
        else f.div(row(k), pivot(k)) match
          case None    => row
          case Some(g) => row.zipWithIndex.map { (cell, j) =>
            if j < k then cell else if j == k then g else f.sub(cell, f.mul(g, pivot(j)))
          }
      }
      (reduced, order)
  val (a, perm) = (0 until n).foldLeft((Vector.tabulate(n, n)((i, j) => cells(i * n + j)), Vector.range(0, n)))(step)
  val l = Vector.tabulate(n, n)((i, j) => if i == j then f.one else if j < i then a(i)(j) else f.zero)
  val u = Vector.tabulate(n, n)((i, j) => if j >= i then a(i)(j) else f.zero)
  val p = Vector.tabulate(n, n)((i, j) => if perm(i) == j then f.one else f.zero)
  (l.flatten, u.flatten, p.flatten)
