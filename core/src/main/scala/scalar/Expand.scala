package it.grypho.scala.leonardo
package scalar

import core.*
import scala.collection.immutable.VectorMap
import scala.collection.mutable


/** The most terms an expansion inside [[expand]] may have (F_0076).
 *
 *  **Set by what can consume the result, not by what can build it.**  A sum is a left spine as
 *  deep as it has terms, and the recursive walkers overflow on it (F_0078) — so a longer
 *  expansion would be built only to crash whatever simplifies or prints it next.  **The
 *  shallowest platform decides** (the `Parser.MaxDepth` rule): measured on Node, `simplifyFully`
 *  and `derive` survive a 500-term sum and overflow at 1000, while the JVM survives 2000.
 *  Raise it if F_0078 makes the walkers iterative.  A refused product stays as written, its
 *  operands expanded, and [[expandReporting]] says why.
 */
val MaxExpandTerms: Int = 500

/** The most term pairs one multiplication inside [[expand]] may combine, checked **before** any
 *  is built (F_0076): it bounds the time where [[MaxExpandTerms]] bounds the size, since folding
 *  can make a large product small.  Measured, not round (the `MaxMatrixCells` rule): on the JVM
 *  a 447×447 product (2·10⁵ pairs) takes ~0.45 s and 1000×1000 ~1.5 s, and a browser is
 *  several times slower — so the bound sits near half a second where the page freezes while it
 *  runs (F_0019).  A legitimate expansion needs more only through massive cancellation. */
val MaxExpandWork: Long = 250000L

/** The highest integer power [[expand]] multiplies out; above it the power stays as written. */
val MaxExpandPower: Int = 20

/** Distributes `*` over `+`, multiplies out `(a+b)^n` for integer `1 ≤ n ≤ 20`, and **folds
 *  like terms as it goes** (F_0076).
 *
 *  The polynomial part of the expression is built over a term map — each monomial a multiset
 *  of factors, each coefficient a number — so `(a+b)^20` is the 21 terms a reader expects, not
 *  the 2²⁰ products it used to be, and `x + x` is `2x`.  Anything that is not a sum, a product
 *  or such a power is a **factor** of that polynomial, expanded inside; terms and factors keep
 *  the order they first appear in.  `_ElementWise` containers (matrices, equations) are
 *  expanded element-wise.  Constants it invents (a coefficient `1`, an exponent) are built in
 *  the input's tier, so an exact expression stays exact.
 *
 *  **Folding treats `*` as commutative**, which is right for scalars.  A literal matrix leaf
 *  (`_MatrixValue`, or a symbolic matrix) is not, so a product containing one is distributed
 *  **in order** without folding, as before.  A free variable *later bound* to a matrix cannot be
 *  told apart here — at the REPL `resolveMatrixOps` re-types such products before `expand`
 *  runs; a library caller expanding `A*B + B*A` over matrix-valued names should not.
 *
 *  Three guards: [[MaxExpandPower]], the size budget [[MaxExpandTerms]] and the work budget
 *  [[MaxExpandWork]].  A product past a budget is left undistributed; [[expandReporting]]
 *  returns the reason with the result.
 *
 *  @param e the expression to expand
 *  @return the expanded expression
 */
def expand(e: _Expression): _Expression = expandReporting(e)._1

/** [[expand]], with the reason a product was left undistributed, if one was.
 *
 *  @param e the expression to expand
 *  @return the expansion, and `Some(reason)` when the term budget refused a product
 */
private[leonardo] def expandReporting(e: _Expression): (_Expression, Option[String]) =
  val ctx = Expansion(_Rational.containsExact(e))
  val r   = expandIn(e)(using ctx)
  (r, ctx.refusal)


/** A monomial: each factor with its positive integer exponent. */
private type Monomial = Map[_Expression, Int]

/** A polynomial: monomials with their (non-zero) numeric coefficients, in first-seen order. */
private type Poly = VectorMap[Monomial, _Value]

/** The state of one [[expand]] call: the tier, the first-seen order of factors, the refusal. */
private final class Expansion(val exact: Boolean):
  private val order = mutable.HashMap.empty[_Expression, Int]
  var refusal: Option[String] = None

  def lit(n: Int): _Value = if exact then _Rational(n) else _Number(n)
  def register(f: _Expression): Unit = { order.getOrElseUpdate(f, order.size); () }
  def rank(f: _Expression): Int = order.getOrElse(f, Int.MaxValue)
  def refuse(reason: String): Unit =
    if refusal.isEmpty then refusal = Some(s"expand declined: $reason")


private def expandIn(e: _Expression)(using ctx: Expansion): _Expression = e match
  case Sum(_, _) | Product(_, _) | Power(_, _) => asExpression(polyOf(e))
  case _: _Value | _: _Variable                => e
  case Ratio(a, b)                             => Ratio(expandIn(a), expandIn(b))

  case Exp(a)        => Exp(expandIn(a))
  case Ln(a)         => Ln(expandIn(a))
  case LogBase(a, b) => LogBase(expandIn(a), expandIn(b))
  case Sin(a)   => Sin(expandIn(a))
  case Cos(a)   => Cos(expandIn(a))
  case Tg(a)    => Tg(expandIn(a))
  case Asin(a)  => Asin(expandIn(a))
  case Acos(a)  => Acos(expandIn(a))
  case Atan(a)  => Atan(expandIn(a))

  case _Heaviside(a)              => _Heaviside(expandIn(a))
  case _Derivative(f, v)          => _Derivative(expandIn(f), v)
  case _Integral(f, v)            => _Integral(expandIn(f), v)
  case _DefIntegral(f, v, lo, hi) => _DefIntegral(expandIn(f), v, expandIn(lo), expandIn(hi))
  case _Limit(f, bv, pt, dir)     => _Limit(expandIn(f), bv, expandIn(pt), dir)
  // Element-wise containers (see core._ElementWise): expand each child in place.
  case ew: _ElementWise           => ew.rebuild(ew.children.map(expandIn))
  case other                      => other

/** A part of the expansion: a polynomial still open to folding, or an expression that is
 *  final (non-commutative, or refused by the budget). */
private type Part = Either[_Expression, Poly]

private def asExpression(p: Part)(using Expansion): _Expression = p.fold(identity, render)

private def polyOf(e: _Expression)(using ctx: Expansion): Part = e match
  case Sum(a, b) =>
    (polyOf(a), polyOf(b)) match
      case (Right(pa), Right(pb)) => Right(add(pa, pb))
      case (pa, pb)               => Left(Sum(asExpression(pa), asExpression(pb)))

  case Product(a, b) =>
    (polyOf(a), polyOf(b)) match
      case (Right(pa), Right(pb)) if !nonCommuting(pa) && !nonCommuting(pb) =>
        multiply(pa, pb).toRight(Product(render(pa), render(pb)))
      case (pa, pb) => Left(ordered(asExpression(pa), asExpression(pb)))

  case Power(base, k @ _Number(n)) if n >= 1 && n <= MaxExpandPower && n == math.floor(n) =>
    polyOf(base) match
      case Right(pb) if !nonCommuting(pb) =>
        if pb.size == 1 then Right(raise(pb, n.toInt))
        else
          (1 until n.toInt).foldLeft(Option(pb))((acc, _) => acc.flatMap(multiply(_, pb)))
            .toRight(Power(render(pb), k))
      case pb =>
        val b = asExpression(pb)
        Left((1 until n.toInt).foldLeft(b)((acc, _) => ordered(b, acc)))

  case c: _Value if isCoefficient(c) => Right(constant(c))
  case Power(base, k @ _Number(_))   => Right(factor(Power(expandIn(base), k)))
  case Power(base, k)                => Right(factor(Power(expandIn(base), expandIn(k))))
  case other                         => Right(factor(expandIn(other)))

/** A number a polynomial may carry as a coefficient: real, exact, based or complex. */
private def isCoefficient(v: _Value): Boolean = v match
  case _: _Complex | _Number(_) => true
  case _                        => false

private def constant(c: _Value): Poly = VectorMap(Map.empty[_Expression, Int] -> c)

private def factor(f: _Expression)(using ctx: Expansion): Poly =
  ctx.register(f)
  VectorMap(Map(f -> 1) -> ctx.lit(1))

/** A leaf whose multiplication does not commute, which folding would get wrong. */
private def nonCommuting(p: Poly): Boolean =
  p.keysIterator.exists(_.keysIterator.exists(isMatrixLeaf))

private def isMatrixLeaf(e: _Expression): Boolean = e match
  case _: _MatrixValue | _: _MatrixShaped => true
  case other                              => other.children.exists(isMatrixLeaf)

private def plus(a: _Value, b: _Value): _Value   = Sum(a, b).eval(Environment()).fold(_ => a, identity)
private def times(a: _Value, b: _Value): _Value  = Product(a, b).eval(Environment()).fold(_ => a, identity)
private def isZero(c: _Value): Boolean = c match
  case _Number(0.0) => true
  case _            => false

private def add(a: Poly, b: Poly): Poly =
  val out = mutable.LinkedHashMap.from(a)
  b.foreach((m, c) => out.updateWith(m)(old => Some(old.fold(c)(plus(_, c)))))
  VectorMap.from(out.filterNot((_, c) => isZero(c)))

/** `a·b`, or `None` (and a recorded refusal) past [[MaxExpandWork]] or [[MaxExpandTerms]]. */
private def multiply(a: Poly, b: Poly)(using ctx: Expansion): Option[Poly] =
  if a.size.toLong * b.size > MaxExpandWork then
    ctx.refuse(s"a product of ${a.size} by ${b.size} terms exceeds the limit of $MaxExpandWork term pairs")
    None
  else
    val out = mutable.LinkedHashMap.empty[Monomial, _Value]
    for (ma, ca) <- a; (mb, cb) <- b do
      val m = mb.foldLeft(ma)((acc, fk) => acc.updated(fk._1, acc.getOrElse(fk._1, 0) + fk._2))
      val c = times(ca, cb)
      out.updateWith(m)(old => Some(old.fold(c)(plus(_, c))))
    val result = VectorMap.from(out.filterNot((_, c) => isZero(c)))
    if result.size > MaxExpandTerms then
      ctx.refuse(s"the expansion has ${result.size} terms, more than the limit of $MaxExpandTerms")
      None
    else Some(result)

/** A single-term polynomial to the power `n`: the coefficient multiplied out, exponents scaled. */
private def raise(p: Poly, n: Int): Poly =
  p.map((m, c) => (m.view.mapValues(_ * n).toMap, (1 until n).foldLeft(c)((acc, _) => times(acc, c))))

private def render(p: Poly)(using ctx: Expansion): _Expression =
  if p.isEmpty then ctx.lit(0)
  else p.map((m, c) => term(m, c)).reduceLeft(Sum.apply)

private def term(m: Monomial, c: _Value)(using ctx: Expansion): _Expression =
  val factors = m.toVector.sortBy((f, _) => ctx.rank(f))
    .map((f, k) => if k == 1 then f else Power(f, ctx.lit(k)))
  if factors.isEmpty then c
  else
    val mono = factors.reduceLeft(Product.apply)
    c match
      case _Number(1.0) => mono
      case _            => Product(c, mono)

/** The distribution `expand` always did, in operand order and without folding: for a product
 *  that does not commute.  Counted against the term budget like the folding path. */
private def ordered(x: _Expression, y: _Expression)(using ctx: Expansion): _Expression =
  def terms(e: _Expression): Long = e match
    case Sum(a, b) => terms(a) + terms(b)
    case _         => 1L
  def go(x: _Expression, y: _Expression): _Expression = (x, y) match
    case (Sum(p, q), z) => Sum(go(p, z), go(q, z))
    case (p, Sum(q, r)) => Sum(go(p, q), go(p, r))
    case _              => Product(x, y)
  val (p, q) = (terms(x), terms(y))
  if p * q > MaxExpandTerms then
    ctx.refuse(s"the expansion has ${p * q} terms, more than the limit of $MaxExpandTerms")
    Product(x, y)
  else go(x, y)
