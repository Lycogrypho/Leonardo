package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*


// Equation solver: solve(eq, v) returns the solutions of eq in v as a list of
// equations in the shape "v = expr" — a solution set does not fit eval's
// Either[_Expression, _Value], so like derive/integrate this is a package-level
// algorithm (the _Solve node presents its result at eval time).
//
// Tiers:
//   - polynomial via scalar.collect (like-term collection):
//       degree 1 → v = -c₀/c₁ (symbolic coefficients welcome)
//       degree 2 → discriminant; 0, 1 or 2 real roots for numeric coefficients,
//                  the two ±√Δ closed forms for symbolic ones
//       degree 0 → Nil (the equation does not constrain v)
//   - anything else (transcendental forms, degree ≥ 3) falls back to numeric
//     root-finding: compile(lhs − rhs, v, env) gives a Double => Double closure,
//     scanned for sign changes over [-100, 100] and refined by bisection — up to
//     MaxNumericRoots roots (periodic functions have infinitely many). If the
//     difference is not compilable (unbound symbols besides v), Nil.
//
// Each solution's right-hand side is folded through env, so bound coefficients
// produce numeric answers: with a := 2, solve(a·x = 4, x) yields x = 2.

private val SearchLo         = -100.0
private val SearchHi         = 100.0
private val SearchSamples    = 10000
private val MaxNumericRoots  = 8
private val BisectIterations = 200

def solve(eq: _Equation, v: _Variable, env: Environment = new Environment()): List[_Equation] =
  val difference = Sum(eq.lhs, Product(_Number(-1), eq.rhs))

  val roots: List[_Expression] = collect(difference, v) match
    case Some(cs) if cs.size == 1 => Nil   // constant in v: nothing to solve for
    case Some(cs) if cs.size == 2 =>
      List(simplifyFully(Ratio(Product(_Number(-1), cs(0)), cs(1))))
    case Some(cs) if cs.size == 3 => quadraticRoots(cs(0), cs(1), cs(2))
    case _                        => numericRoots(difference, v, env)

  roots.map(r => _Equation(v, r.eval(env).toExpression))

private def quadraticRoots(c0: _Expression, c1: _Expression, c2: _Expression): List[_Expression] =
  (c0, c1, c2) match
    case (_Number(a0), _Number(a1), _Number(a2)) =>
      val delta = a1 * a1 - 4.0 * a2 * a0
      if delta < 0.0 then Nil
      else if delta == 0.0 then List(_Number(-a1 / (2.0 * a2)))
      else
        val sq = math.sqrt(delta)
        List(_Number((-a1 - sq) / (2.0 * a2)), _Number((-a1 + sq) / (2.0 * a2)))
    case _ =>
      // Symbolic coefficients: both ±√Δ closed forms (the sign of Δ is unknown).
      val delta = Sum(Power(c1, _Number(2)), Product(_Number(-4), Product(c2, c0)))
      val sqrtD = Power(delta, _Number(0.5))
      val denom = Product(_Number(2), c2)
      List(
        simplifyFully(Ratio(Sum(Product(_Number(-1), c1), Product(_Number(-1), sqrtD)), denom)),
        simplifyFully(Ratio(Sum(Product(_Number(-1), c1), sqrtD), denom))
      )

private def numericRoots(f: _Expression, v: _Variable, env: Environment): List[_Expression] =
  compile(f, v, env) match
    case None     => Nil
    case Some(fn) =>
      val step  = (SearchHi - SearchLo) / SearchSamples
      val found = scala.collection.mutable.ListBuffer[Double]()
      var a  = SearchLo
      var fa = fn(a)
      var i  = 0
      while i < SearchSamples && found.size < MaxNumericRoots do
        val b  = a + step
        val fb = fn(b)
        if fa == 0.0 then found += a
        else if !fa.isNaN && !fb.isNaN && fa * fb < 0.0 then found += bisect(fn, a, b, fa)
        a = b; fa = fb; i += 1
      found.toList.map(_Number(_))

private def bisect(fn: Double => Double, lo0: Double, hi0: Double, flo0: Double): Double =
  var lo  = lo0
  var hi  = hi0
  var flo = flo0
  var i   = 0
  while i < BisectIterations do
    val mid  = (lo + hi) / 2.0
    val fmid = fn(mid)
    if fmid == 0.0 then return mid
    if flo * fmid < 0.0 then hi = mid
    else
      lo = mid
      flo = fmid
    i += 1
  (lo + hi) / 2.0
