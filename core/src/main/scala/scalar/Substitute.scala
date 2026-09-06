package it.grypho.scala.leonardo
package scalar

import core.*


/** Substitutes named definitions into an expression, replacing every `_Variable` whose
 *  name appears in `definitions` with that definition's body, recursively, so chained
 *  definitions (`g` defined in terms of `f`) resolve in a single call.
 *
 *  Termination: a `seen` set breaks reference cycles -- inside the expansion of `f`, any
 *  further occurrence of `f` is left as a free variable, so self- and mutually-recursive
 *  definitions cannot loop.
 *
 *  Binder positions are never substituted: the variable of differentiation / integration
 *  (and every other binder -- limit, integral transforms, `solveSystem`, `ode`) names the
 *  binder, not a use, so substitution must not touch it.  No special-casing is needed:
 *  binders are excluded from `children` and carried through by `rebuild`, so the generic
 *  `other.rebuild(other.children.map(...))` clause recurses into use positions while
 *  preserving every binder.  Only the `_Variable` case performs the actual replacement.
 *
 *  @param e           the expression to substitute into
 *  @param definitions map from variable name to its replacement body
 *  @return            `e` with every bound name replaced by its definition
 */
def substitute(e: _Expression, definitions: Map[String, _Expression]): _Expression =
  def loop(e: _Expression, seen: Set[String]): _Expression = e match
    case _Variable(n) if definitions.contains(n) && !seen.contains(n) =>
      loop(definitions(n), seen + n)
    case other =>
      other.rebuild(other.children.map(loop(_, seen)))

  loop(e, Set())


/** Picks a variable name that collides with nothing in `reserved`, capture-safe.
 *
 *  Pigeonhole: among the `|reserved| + 1` candidates `prefix0 … prefix|reserved|`
 *  at least one is free, so the search always succeeds and the `getOrElse` default is
 *  unreachable.  Shared by every tier that must invent an internal variable — the
 *  Laplace-to-Fourier frequency name and the u-substitution integrator (issue 3.10).
 *
 *  @param reserved names the new variable must avoid (typically some `freeVars` set)
 *  @param prefix   the name stem (default `__u`; the Fourier tier passes `__lts`)
 *  @return a fresh [[_Variable]] whose name is not in `reserved`
 */
private[leonardo] def freshVar(reserved: Set[String], prefix: String = "__u"): _Variable =
  val name = (0 to reserved.size).iterator.map(i => s"$prefix$i")
    .find(!reserved.contains(_))
    .getOrElse(s"$prefix${reserved.size + 1}")
  _Variable(name)

/** Replaces every occurrence of the sub-expression `target` in `e` with `replacement`.
 *
 *  Unlike [[substitute]], which replaces variables *by name*, this rewrites an arbitrary
 *  sub-term structurally (by value equality), so a compound argument such as `g(v)` can be
 *  swapped for a fresh `u` — the mechanism u-substitution (issue 3.10) needs.  Binder
 *  positions are excluded from `children`, so a binder is never rewritten.
 *
 *  @param e           the expression to rewrite
 *  @param target      the sub-expression to find
 *  @param replacement what to put in its place
 *  @return            `e` with every structural occurrence of `target` replaced
 */
private[scalar] def replaceSubexpr(e: _Expression, target: _Expression, replacement: _Expression): _Expression =
  if e == target then replacement
  else e.rebuild(e.children.map(replaceSubexpr(_, target, replacement)))
