package it.grypho.scala.leonardo
package scalar

import core.*


// Higher-order operators that take an expression (and a variable) and produce a new
// one: differentiation and integration. The algorithms live in their own files
// (Derive.scala, Integrate.scala, Compile.scala); these classes are just the AST nodes.
//
// children / rebuild: the binder variable (v) is excluded from children — it names
// the variable of differentiation/integration, not a use — so traversals (Substitute,
// Analysis) never recurse into it. _DefIntegral's children include lo and hi because
// they are regular expression positions subject to substitution and dependsOn checks.
abstract class _Functional extends _Expression


case class _Derivative(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"derive($e, $v)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Derivative(c.head, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val derivative = it.grypho.scala.leonardo.scalar.derive(e, v)
    // derive returns this same _Derivative node when it cannot reduce further (e.g.
    // differentiating an integral w.r.t. an unrelated variable). Re-evaluating that
    // would call derive on the identical node forever, so stay symbolic instead.
    if derivative == this then Left(this)
    else derivative.eval(env)


case class _Integral(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"integral($e, $v)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Integral(c.head, v)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val antiderivative = it.grypho.scala.leonardo.scalar.integrate(e, v)
    // integrate returns this same _Integral node when no rule applies. Re-evaluating
    // it would call integrate on the identical node forever, so stay symbolic instead
    // (mirrors _Derivative.eval's termination guard).
    if antiderivative == this then Left(this)
    else antiderivative.eval(env)


case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional:
  override def toString: String = s"integral($e, $v, $low_limit, $up_limit)"
  override def children: List[_Expression] = List(e, low_limit, up_limit)
  override def rebuild(c: List[_Expression]): _Expression = _DefIntegral(c.head, v, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (low_limit.eval(env), up_limit.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) =>
        val rawN = (env.precision.toLong * 200L).max(100L).min(Int.MaxValue.toLong).toInt
        val n    = if rawN % 2 == 0 then rawN else rawN + 1
        val h    = (b - a) / n

        compile(e, v, env) match
          case Some(f) =>
            // Fast path: compiled closure — no per-step tree traversal or env allocation.
            @annotation.tailrec
            def fastLoop(i: Int, acc: Double): Double =
              if i > n then acc
              else
                val coeff = if i == 0 || i == n then 1.0
                            else if i % 2 == 1   then 4.0
                            else                       2.0
                fastLoop(i + 1, acc + coeff * f(a + i * h))
            val s = h / 3.0 * fastLoop(0, 0.0)
            if s.isNaN || s.isInfinite then Left(this) else Right(_Number(s))

          case None =>
            // Fallback: tree-evaluation per step (handles symbolic sub-expressions).
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
              case Some(s) =>
                val result = h / 3.0 * s
                if result.isNaN || result.isInfinite then Left(this) else Right(_Number(result))
              case None    => Left(this)

      case _ => Left(this)
