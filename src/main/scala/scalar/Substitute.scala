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
 * Because binders are excluded from children (see _Expression.children), _Functional
 * nodes must still be matched explicitly here to preserve the binder.
 *
 * For all other node types (operations, functions, leaf nodes) the generic
 * `other.rebuild(other.children.map(...))` handles them without per-type cases.
 */
def substitute(e: _Expression, definitions: Map[String, _Expression]): _Expression =
  def loop(e: _Expression, seen: Set[String]): _Expression = e match
    case _Variable(n) if definitions.contains(n) && !seen.contains(n) =>
      loop(definitions(n), seen + n)
    case _Derivative(f, v)          => _Derivative(loop(f, seen), v)
    case _Integral(f, v)            => _Integral(loop(f, seen), v)
    case _DefIntegral(f, v, lo, hi) => _DefIntegral(loop(f, seen), v, loop(lo, seen), loop(hi, seen))
    case other                      => other.rebuild(other.children.map(loop(_, seen)))

  loop(e, Set())
