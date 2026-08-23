package it.grypho.scala.leonardo

/** Probability distributions as first-class values, with expectation and variance as
 *  functionals over them (issue 4.P).
 *
 *  Imports `core` + `scalar`; nothing imports this except the parser and, later, the
 *  inference half of `statistics`.  In particular `probability` must never import
 *  `statistics` — the dependency runs the other way.
 *
 *  **A distribution is a `core._Value`.**  It is a concrete, fully-reduced object a user
 *  binds to a name, exactly like a matrix or a truth value.  That sounds expensive after
 *  issue 4.L, where adding `_Rational` meant a widening extractor and a hundred-odd touched
 *  sites — but the cases are not alike, and the difference is the general rule:
 *
 *  > Adding a `_Value` is cheap unless it has to be **readable as an existing one**.
 *
 *  ``_Rational` was costly precisely because every `case _Number(x)` site had a legitimate
 *  claim on it.  A distribution is not a number, so nothing needs to read it as one; nodes
 *  that do not understand it stay symbolic, which is the correct answer rather than a
 *  degradation.  `_MatrixValue` and `_Truth` are the precedents here, not `_Rational`.
 *
 *  **What is exact and what is not.**  The *moments* of these distributions are rational
 *  functions of their parameters — `Uniform(a,b).mean = (a+b)/2` — so they stay exact in the
 *  exact tier.  The pdf and cdf of the continuous ones are transcendental and degrade to
 *  `Double`, like every other transcendental in the library.
 */
package probability
