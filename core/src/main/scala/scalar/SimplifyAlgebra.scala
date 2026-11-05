package it.grypho.scala.leonardo
package scalar

import core.*


// Like terms, the Pythagorean pair and common factors (F_0292).
//
// `simplify` is structural and cheap: it folds constants and identities but neither collects like
// terms across a sum nor cancels a factor shared by a numerator and a denominator, so
// `2*sin(x)*cos(x) - 2*cos(x)*sin(x)` stayed, `cos(t)*r*cos(t) + r*sin(t)*sin(t)` was not `r`, and
// `((x + h)^2 - x^2)/h` kept its `/h`.  This pass does those three things, and is applied where the
// owner decided (2026-11-04): the `simplify` command and the answer `derive` returns -- not inside
// `simplifyFully`, which every other pass runs in its fixpoints.  **A subtree is replaced only when
// something actually combined**, so an expression with nothing to collect keeps its printed form.

/** A product as a coefficient and the powers of its bases, a base appearing once (first-seen
 *  order), exponents numeric. */
private final case class AlgTerm(coef: _Value, factors: List[(_Expression, _Value)]):
  /** The factors as a multiset, the grouping key: order does not matter, exponents by value. */
  def key: Set[(_Expression, Double)] = factors.map((b, k) => (b, asDouble(k))).toSet
  def exponentOf(b: _Expression): Double = factors.find(_._1 == b).fold(0.0)(f => asDouble(f._2))

private def asDouble(v: _Value): Double = v match
  case _Number(d) => d
  case _          => Double.NaN

private def addV(a: _Value, b: _Value): _Value = Sum(a, b).eval(EmptyEnv).getOrElse(a)
private def mulV(a: _Value, b: _Value): _Value = Product(a, b).eval(EmptyEnv).getOrElse(a)

/** `x` as a coefficient and powers of bases.  A numeric power of a base is read as one; a
 *  quotient's denominator contributes negated exponents. */
private def algTerm(x: _Expression)(using t: Tier): AlgTerm =
  def raw(e: _Expression, sign: Int): List[Either[_Value, (_Expression, _Value)]] = e match
    case Product(a, b) => raw(a, sign) ++ raw(b, sign)
    case Ratio(a, b)   => raw(a, sign) ++ raw(b, -sign)
    case v @ (_: _Number | _: _Rational) =>
      List(Left(if sign > 0 then v.asInstanceOf[_Value]
                else Ratio(t.n(1), v).eval(EmptyEnv).getOrElse(v.asInstanceOf[_Value])))
    case Power(b, k: _Value) if asDouble(k).isFinite && !b.isInstanceOf[_Number] =>
      List(Right((b, if sign > 0 then k else mulV(t.n(-1).asInstanceOf[_Value], k))))
    case other => List(Right((other, t.n(sign).asInstanceOf[_Value])))
  val parts = raw(x, 1)
  val coef  = parts.collect { case Left(v) => v }.foldLeft(t.n(1).asInstanceOf[_Value])(mulV)
  val merged = parts.collect { case Right(f) => f }.foldLeft(List.empty[(_Expression, _Value)]) { case (acc, (b, k)) =>
    acc.indexWhere(_._1 == b) match
      case -1 => acc :+ (b, k)
      case i  => acc.updated(i, (b, addV(acc(i)._2, k)))
  }.filterNot((_, k) => isZeroLiteral(k))
  AlgTerm(coef, merged)

/** The term as an expression. */
private def algExpr(term: AlgTerm)(using t: Tier): _Expression =
  val body = term.factors.map((b, k) => if isOneLiteral(k) then b else Power(b, k))
    .reduceLeftOption[_Expression](Product(_, _))
  body match
    case None                              => term.coef
    case Some(b) if isOneLiteral(term.coef) => b
    case Some(b)                           => Product(term.coef, b)

/** The terms of a sum with like terms collected and each `c*m*cos(u)^2 + c*m*sin(u)^2` pair read as
 *  `c*m`; `None` when nothing combined. */
private def collected(terms: List[AlgTerm])(using t: Tier): Option[List[AlgTerm]] =
  val grouped = terms.foldLeft(List.empty[AlgTerm]) { (acc, x) =>
    acc.indexWhere(_.key == x.key) match
      case -1 => acc :+ x
      case i  => acc.updated(i, acc(i).copy(coef = addV(acc(i).coef, x.coef)))
  }.filterNot(x => isZeroLiteral(x.coef))
  @annotation.tailrec
  def pythagoras(ts: List[AlgTerm], changed: Boolean): (List[AlgTerm], Boolean) =
    val pair = (for
      (c, i) <- ts.zipWithIndex.iterator
      (cosArg, two) <- c.factors.iterator.collect { case (Cos(u), k) if asDouble(k) == 2.0 => (u, k) }
      rest = c.factors.filterNot(_._1 == Cos(cosArg))
      (s, j) <- ts.zipWithIndex.iterator
      if j != i && s.coef == c.coef && s.exponentOf(Sin(cosArg)) == 2.0
      if s.factors.filterNot(_._1 == Sin(cosArg)).map((b, k) => (b, asDouble(k))).toSet == rest.map((b, k) => (b, asDouble(k))).toSet
    yield (i, j, AlgTerm(c.coef, rest))).nextOption()
    pair match
      case None            => (ts, changed)
      case Some((i, j, r)) => pythagoras(ts.zipWithIndex.collect { case (x, k) if k != j => if k == i then r else x }, true)
  val (out, pyth) = pythagoras(grouped, false)
  Option.when(pyth || out.size != terms.size)(out)

private def sumOf(ts: List[AlgTerm])(using t: Tier): _Expression =
  ts.map(algExpr).reduceLeftOption[_Expression](Sum(_, _)).getOrElse(t.n(0))

private def flatSum(e: _Expression): List[_Expression] = e match
  case Sum(a, b) => flatSum(a) ++ flatSum(b)
  case other     => List(other)

/** `n/d` with the factors of `d` that every term of `n` carries cancelled; `None` when none is. */
private def cancelled(n: _Expression, d: _Expression)(using t: Tier): Option[_Expression] =
  def attempt(num: List[AlgTerm]): Option[_Expression] =
    val den = algTerm(d)
    val cuts = den.factors.flatMap { (b, k) =>
      val m = (asDouble(k) +: num.map(_.exponentOf(b))).min
      Option.when(m > 0 && m.isFinite)((b, m))
    }
    Option.when(cuts.nonEmpty) {
      def less(x: AlgTerm): AlgTerm = x.copy(factors = x.factors.map { (b, k) =>
        cuts.find(_._1 == b).fold((b, k)) { (_, m) =>
          val mv: _Value = if t.exact then _Rational.fromDouble(m).getOrElse(_Number(m)) else _Number(m)
          (b, addV(k, mulV(t.n(-1), mv)))
        }
      }.filterNot((_, k) => asDouble(k) == 0.0))
      val num2 = sumOf(num.map(less))
      val den2 = less(den)
      if den2.factors.isEmpty && isOneLiteral(den2.coef) then num2 else Ratio(num2, algExpr(den2))
    }
  val direct = flatSum(n).map(algTerm)
  attempt(direct).orElse {
    val terms = flatSum(expand(n)).map(algTerm)
    attempt(collected(terms).getOrElse(terms))
  }

/** The F_0292 pass: like terms collected, the Pythagorean pair, common factors cancelled.
 *  @param e the expression
 *  @return the expression with those reductions applied, unchanged where nothing combined */
private[leonardo] def simplifyAlgebraic(e: _Expression): _Expression =
  given Tier = Tier(_Rational.containsExact(e))
  val out = algebraic(e)
  if out == e then e else simplifyFullyInTier(out, _Rational.containsExact(e))

private def algebraic(e: _Expression)(using t: Tier): _Expression =
  val inner = if e.children.isEmpty then e else e.rebuild(e.children.map(algebraic))
  inner match
    case s: Sum   =>
      val terms = flatSum(s).map(algTerm)
      collected(terms).fold(inner)(sumOf)
    case Ratio(n, d) if dependsOnAny(d) => cancelled(n, d).getOrElse(inner)
    case other    => other

/** Whether `d` carries a factor worth cancelling: anything but a number. */
private def dependsOnAny(d: _Expression): Boolean = d match
  case _: _Number | _: _Rational => false
  case _                         => true
