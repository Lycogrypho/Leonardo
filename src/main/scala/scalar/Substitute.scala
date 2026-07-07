package it.grypho.scala.leonardo
package scalar

import core.*


/**
 * Substitution of named definitions into an expression: every _Variable whose name
 * appears in the definitions map is replaced by that definition's body, recursively,
 * so chained definitions (g defined in terms of f) resolve in one call.
 *
 * Termination: a seen set breaks reference cycles — inside the expansion of f, any
 * further occurrence of f is left as a free variable, so self- and mutually-recursive
 * definitions cannot loop.
 *
 * Binder positions are never substituted: the variable of differentiation/integration
 * names the binder, not a use, so _Derivative(f, v) substitutes into f but keeps v.
 *
 * This is deliberately separate from Environment (values only) and from eval:
 * substitution is an explicit, always-terminating step the caller composes with
 * eval/derive/simplify, keeping those definition-blind and env-free.
 */
def substitute(e: _Expression, definitions: Map[String, _Expression]): _Expression =
  def loop(e: _Expression, seen: Set[String]): _Expression = e match
    case _Variable(n) if definitions.contains(n) && !seen.contains(n) =>
      loop(definitions(n), seen + n)
    case Sum(a, b)                  => Sum(loop(a, seen), loop(b, seen))
    case Product(a, b)              => Product(loop(a, seen), loop(b, seen))
    case Ratio(a, b)                => Ratio(loop(a, seen), loop(b, seen))
    case Power(a, b)                => Power(loop(a, seen), loop(b, seen))
    case Exp(a)                     => Exp(loop(a, seen))
    case Log(a)                     => Log(loop(a, seen))
    case Sin(a)                     => Sin(loop(a, seen))
    case Cos(a)                     => Cos(loop(a, seen))
    case Tg(a)                      => Tg(loop(a, seen))
    case _Derivative(f, v)          => _Derivative(loop(f, seen), v)
    case _Integral(f, v)            => _Integral(loop(f, seen), v)
    case _DefIntegral(f, v, lo, hi) => _DefIntegral(loop(f, seen), v, loop(lo, seen), loop(hi, seen))
    case other                      => other

  loop(e, Set())
