package it.grypho.scala.leonardo
package scalar

import core.*


// Evaluates an expression over a uniform grid of n points in [lo, hi], returning
// (x, f(x)) pairs in ascending x order, with non-finite results silently dropped.
//
// Fast path: if the expression compiles to a Double => Double closure over v
// (no unresolvable symbolic nodes), every point is evaluated with no allocation.
// Fallback: eval the tree with v bound per-step, keeping only _Number results.
def sample(
    e:   _Expression,
    v:   _Variable,
    lo:  Double,
    hi:  Double,
    n:   Int         = 200,
    env: Environment = new Environment()
): Vector[(Double, Double)] =
  val step = if n <= 1 then 0.0 else (hi - lo) / (n - 1)
  val xs   = Vector.tabulate(n)(i => lo + i * step)
  compile(e, v, env) match
    case Some(f) =>
      xs.flatMap { x =>
        val y = f(x)
        if y.isNaN || y.isInfinite then None else Some((x, y))
      }
    case None =>
      xs.flatMap { x =>
        e.eval(env.withBinding(v.variable, _Number(x))) match
          case Right(_Number(y)) if !y.isNaN && !y.isInfinite => Some((x, y))
          case _                                               => None
      }
