package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.{pow, exp, log, sin, cos, tan, asin, acos, atan}


// Attempts to compile an expression into a raw Double => Double closure over v,
// resolving any other variables from env. Returns None for expressions that contain
// unbound symbolic nodes (_Derivative, _Integral, unresolvable variables, etc.).
//
// When Some(f) is returned, f(x) evaluates e with v bound to x at full Double
// precision, with no per-step allocation, no tree traversal, and no environment
// lookup — O(1) per sample after compilation. This is the basis of the fast path
// in _DefIntegral.eval (Simpson's rule) and future Monte-Carlo / matrix-fill loops.
def compile(e: _Expression, v: _Variable, env: Environment): Option[Double => Double] = e match
  case _Number(d)    => Some(_ => d)
  case _: _Complex   => None    // a complex value has no Double => Double closure
  case x: _Variable if x.variable == v.variable => Some(x => x)
  case _Variable(n)  => env.get(n).collect { case _Number(d) => _ => d }
  case Sum(a, b)     =>
    for fa <- compile(a, v, env); fb <- compile(b, v, env) yield (x: Double) => fa(x) + fb(x)
  case Product(a, b) =>
    for fa <- compile(a, v, env); fb <- compile(b, v, env) yield (x: Double) => fa(x) * fb(x)
  case Ratio(a, b)   =>
    for fa <- compile(a, v, env); fb <- compile(b, v, env) yield (x: Double) => fa(x) / fb(x)
  case Power(a, b)   =>
    for fa <- compile(a, v, env); fb <- compile(b, v, env) yield (x: Double) => pow(fa(x), fb(x))
  case Exp(a)        => compile(a, v, env).map(fa => (x: Double) => exp(fa(x)))
  case Ln(a)         => compile(a, v, env).map(fa => (x: Double) => log(fa(x)))
  case LogBase(a, b) =>
    for fa <- compile(a, v, env); fb <- compile(b, v, env)
    yield (x: Double) => log(fa(x)) / log(fb(x))
  case Sin(a)        => compile(a, v, env).map(fa => (x: Double) => sin(fa(x)))
  case Cos(a)        => compile(a, v, env).map(fa => (x: Double) => cos(fa(x)))
  case Tg(a)         => compile(a, v, env).map(fa => (x: Double) => tan(fa(x)))
  case Asin(a)       => compile(a, v, env).map(fa => (x: Double) => asin(fa(x)))
  case Acos(a)       => compile(a, v, env).map(fa => (x: Double) => acos(fa(x)))
  case Atan(a)       => compile(a, v, env).map(fa => (x: Double) => atan(fa(x)))
  case _             => None  // _Derivative, _Integral, _DefIntegral, unresolvable
