package it.grypho.scala.leonardo
package scalar

import core.*


// Higher-order operators that take an expression (and a variable) and produce a new
// one: differentiation and integration. The algorithms live in their own files
// (Derive.scala, Integrate.scala); these classes are just the AST nodes.
abstract class _Functional extends _Expression


case class _Derivative(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"derive($e, $v)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    val derivative = it.grypho.scala.leonardo.scalar.derive(e, v)
    // derive returns this same _Derivative node when it cannot reduce further (e.g.
    // differentiating an integral w.r.t. an unrelated variable). Re-evaluating that
    // would call derive on the identical node forever, so stay symbolic instead.
    if derivative == this then Left(this)
    else derivative.eval(env)


case class _Integral(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"integral($e, $v)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    val antiderivative = it.grypho.scala.leonardo.scalar.integrate(e, v)
    // integrate returns this same _Integral node when no rule applies. Re-evaluating
    // it would call integrate on the identical node forever, so stay symbolic instead
    // (mirrors _Derivative.eval's termination guard).
    if antiderivative == this then Left(this)
    else antiderivative.eval(env)


case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional:
  override def toString: String = s"integral($e, $v, $low_limit, $up_limit)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (low_limit.eval(env), up_limit.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) =>
        val rawN = (env.precision * 200).max(100)
        val n    = if rawN % 2 == 0 then rawN else rawN + 1
        val h    = (b - a) / n

        @annotation.tailrec
        def loop(i: Int, acc: Double): Option[Double] =
          if i > n then Some(acc)
          else
            e.eval(env.withBinding(v.variable, _Number(a + i * h))) match
              case Right(_Number(y)) =>
                val coeff = if i == 0 || i == n then 1.0
                            else if i % 2 == 1   then 4.0
                            else                       2.0
                loop(i + 1, acc + coeff * y)
              case _ => None

        loop(0, 0.0) match
          case Some(s) => _Number(h / 3.0 * s).eval(env)
          case None    => Left(this)
      case _ => Left(this)
