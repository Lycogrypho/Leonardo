package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.{Sum, Product}


/** The products and the diagonal sum a first linear-algebra course needs: `dot` (F_0160),
 *  `cross` (F_0161) and `trace` (F_0162).
 *
 *  Each reads its operands through [[cellGrid]], the one reader of "an evaluated operand as a
 *  grid of cells", so a dense matrix, an exact literal and a symbolic one arrive the same way, and
 *  each builds its answer from `Sum` and `Product` nodes evaluated as it goes — which is what keeps
 *  exact cells exact, lets symbolic ones stay symbolic, and leaves no tier rule to restate.  A
 *  shape the function does not define stays unevaluated as the call written.
 */

/** A row's or a column's shape and cells, or `None` for any other operand.  A number is not a
 *  vector here: a `1×1` grid has to be written as one. */
private def vectorCells(r: Either[_Expression, _Value]): Option[(Int, Int, Vector[_Expression])] =
  cellGrid(r, scalars = false).collect { case m if m.rows == 1 || m.cols == 1 => (m.rows, m.cols, m.elems) }

/** `terms` summed one at a time, each partial sum evaluated before the next term joins it.
 *
 *  A numeric sum therefore never builds a spine at all.  A symbolic one is as deep as it has
 *  terms, so it is refused past `core.MaxExpressionDepth` — the walkers that would consume it
 *  set that limit (F_0078), and building the tree only for the next `simplify` to overflow on it
 *  would be the confident failure the limit exists to prevent.
 *
 *  @param terms the terms, at least one
 *  @param env   the evaluation environment
 *  @return the sum, or `None` when a symbolic sum would be too deep
 */
private[matrix] def accumulated(terms: Vector[_Expression], env: Environment): Option[Either[_Expression, _Value]] =
  terms.tail.foldLeft(Option(terms.head.eval(env))) { (acc, t) =>
    acc.flatMap { a =>
      val next = Sum(a.toExpression, t).eval(env)
      Option.unless(tooDeep(next.toExpression))(next)
    }
  }


/** `dot(u, v)` — the scalar `Σ uᵢvᵢ` of two rows or columns of one length, in any mix (F_0160).
 *
 *  **No conjugate** (F_0160 Decision 1): this is the bilinear form, numpy's `dot`.  The
 *  Hermitian product conjugates its first argument, and the language has no conjugate yet, so a
 *  vector with a complex cell is **refused** rather than given the product of a different
 *  convention.  A SCALAR result, so it is an `_Expression` and not a `_MatrixOperation`:
 *  `2 + dot(u, v)` is a number, where `2 + transpose(u)*v` is a `1×1` matrix sum.
 *
 *  Exact cells give an exact answer, symbolic cells a sum of products; a length mismatch, a
 *  non-vector and a complex cell stay unevaluated.
 *
 *  @param u the first vector
 *  @param v the second
 */
case class _Dot(u: _Expression, v: _Expression) extends _Expression:
  override def toString: String = s"dot($u, $v)"
  override def children: List[_Expression] = List(u, v)
  override def rebuild(c: List[_Expression]): _Expression = _Dot(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ru, rv) = (u.eval(env), v.eval(env))
    (ru, rv) match
      // The dense pair needs no grid at all: a long numeric vector is summed in place.
      case (Right(a: _MatrixValue), Right(b: _MatrixValue))
        if (a.rows == 1 || a.cols == 1) && (b.rows == 1 || b.cols == 1) && a.rows * a.cols == b.rows * b.cols =>
        val (xs, ys) = (a.toVector, b.toVector)
        Right(_Number(xs.indices.foldLeft(0.0)((s, i) => s + xs(i) * ys(i))))
      case _ =>
        (vectorCells(ru), vectorCells(rv)) match
          case (Some((_, _, us)), Some((_, _, vs)))
            if us.size == vs.size && !(us ++ vs).exists(_.isInstanceOf[_Complex]) =>
            accumulated(us.lazyZip(vs).map((a, b) => Product(a, b): _Expression), env).getOrElse(Left(this))
          case (Some(_), Some(_)) => Left(this)
          case _                  => Left(_Dot(ru.toExpression, rv.toExpression))


/** `cross(u, v)` — the right-handed product of two 3-vectors, in the first argument's shape
 *  (F_0161): `(u₂v₃ − u₃v₂, u₃v₁ − u₁v₃, u₁v₂ − u₂v₁)`, so `cross(e₁, e₂) = e₃`.
 *
 *  **Right-handed, stated**: `curl`'s `sphericalmaths` case is the reminder that handedness is
 *  not to be left implicit.  Any other length stays unevaluated — there is no 2-D "scalar cross
 *  product", which would be a different result type, the `curl` precedent.  The `−1` the
 *  differences need is built in the operands' tier (the numeric tier rule), so exact cells give
 *  exact cells.  A matrix result, so this is a `_MatrixOperation`: `2 * cross(u, v)` scales it.
 *
 *  @param u the first vector, whose shape the result keeps
 *  @param v the second
 */
case class _Cross(u: _Expression, v: _Expression) extends _MatrixOperation:
  override def toString: String = s"cross($u, $v)"
  override def children: List[_Expression] = List(u, v)
  override def rebuild(c: List[_Expression]): _Expression = _Cross(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ru, rv) = (u.eval(env), v.eval(env))
    (vectorCells(ru), vectorCells(rv)) match
      case (Some((rows, cols, us)), Some((_, _, vs))) if us.size == 3 && vs.size == 3 =>
        val neg = _Rational.literalLike(-1, _Matrix(1, 6, us ++ vs))
        def term(i: Int, j: Int): _Expression =
          Sum(Product(us(i), vs(j)), Product(neg, Product(us(j), vs(i))))
        _Matrix(rows, cols, Vector(term(1, 2), term(2, 0), term(0, 1))).eval(env)
      case (Some(_), Some(_)) => Left(this)
      case _                  => Left(_Cross(ru.toExpression, rv.toExpression))


/** `trace(A)` — the sum of a square matrix's diagonal (F_0162): exact on exact cells, symbolic on
 *  symbolic ones, and unevaluated for a non-square matrix.  A SCALAR result, like [[Determinant]].
 *
 *  @param m the matrix
 */
case class _Trace(m: _Expression) extends _Expression:
  override def toString: String = s"trace($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _Trace(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = m.eval(env)
    r match
      case Right(a: _MatrixValue) if a.rows == a.cols =>
        Right(_Number((0 until a.rows).foldLeft(0.0)((s, i) => s + a(i, i))))
      case _ =>
        cellGrid(r, scalars = false) match
          case Some(g) if g.rows == g.cols =>
            accumulated(Vector.tabulate(g.rows)(i => g(i, i)), env).getOrElse(Left(this))
          case Some(_) => Left(this)
          case None    => Left(_Trace(r.toExpression))
