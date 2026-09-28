package it.grypho.scala.leonardo
package scalar

import core.*


/** Returns `true` when expression `e` contains variable `v` as a free occurrence.
 *
 *  Uses the cached `freeVars` set on each node — computed once per node on first call,
 *  then O(1) — so repeated `dependsOn` calls on the same expression tree are effectively
 *  free after the first traversal.
 *
 *  @param e the expression to inspect
 *  @param v the variable to look for
 */
def dependsOn(e: _Expression, v: _Variable): Boolean =
  e.freeVars.contains(v.variable)


/** Returns `true` when `e` has a value only where `v` is a **whole number** (issue F_0044).
 *
 *  True exactly when some `sum`, `product` or `tabulate` inside `e` takes `v` in one of its
 *  **bounds**: those require a whole bound ([[indexOf]] accepts one only at
 *  `d == Math.floor(d)`), so at every other value of `v` the node stays symbolic and whatever
 *  contains it has no value at all.  A sampler that grids `v` continuously therefore misses
 *  such an expression almost everywhere — over `0 … 100` at 200 points, at exactly one sample.
 *
 *  **The test is structural, and deliberately so.**  Inferring it from *behaviour* — noticing
 *  that many samples came back empty — cannot tell this apart from a pole or a restricted
 *  domain, and would report an ordinary discontinuous function as a grid problem.  Reading the
 *  shape answers the question that was actually asked.
 *
 *  Only the bounds count.  A reduction whose *body* mentions `v` is an ordinary continuous
 *  function of it (`sum(v*k, k, 1, 5)` is a polynomial), and the binder itself is not `v`'s
 *  business — which is why this cannot be written in terms of `freeVars` alone.
 *
 *  @param e the expression to inspect
 *  @param v the variable a caller intends to vary
 *  @return whether varying `v` continuously would step outside where `e` is defined
 */
def wholeIndexedIn(e: _Expression, v: _Variable): Boolean =
  val boundsUse = e match
    case r: _Reduction => dependsOn(r.lo, v) || dependsOn(r.hi, v)
    case t: _Tabulate  => dependsOn(t.lo, v) || dependsOn(t.hi, v)
    case _             => false
  boundsUse || e.children.exists(wholeIndexedIn(_, v))
