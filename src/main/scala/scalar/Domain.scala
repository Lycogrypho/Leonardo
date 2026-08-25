package it.grypho.scala.leonardo
package scalar

import core.*


/** Where an expression is *defined* and where it is *differentiable* — issue 3.3.
 *
 *  A **correctness** feature rather than a capability one: `solve`, `integrate` and `limit`
 *  all fall back to staying symbolic without saying why, and the answer is usually that some
 *  sub-expression left its domain.
 *
 *  **Two representations, because one cannot do both jobs.**  [[DomainSet.constraints]] is
 *  always present and is the honest general answer — it survives symbolic coefficients, so
 *  `ln(x - a)` yields `x - a > 0` with `a` still free.  [[DomainSet.intervals]] is `Some`
 *  only when every constraint resolved numerically.  `None` there means *"the constraints
 *  are known but could not be turned into intervals"*, which is a **different statement**
 *  from "unbounded" and must never be conflated with it.
 *
 *  **The analysis describes what the library computes, not what is mathematically true.**
 *  `Asin`/`Acos`/`Atan` stay symbolic on complex input (the documented "Asin convention"),
 *  so over [[DomainKind.Complex]] their domain here is reported **empty** even though the
 *  functions extend analytically.  Reporting the mathematical truth would make this promise
 *  results `eval` refuses to produce — the same failure mode 4.R slice B avoids when it
 *  declines `prob(2*X < 6)` rather than guessing.
 *
 *  This lives in `scalar` on purpose: `_Comparison` is an `equation` type and `equation`
 *  imports `scalar`, so an answer shaped as a predicate could never be consumed by
 *  `integrate` or `Limit`.  The neutral [[DomainSet]] is computed here and rendered into
 *  relation nodes above the `equation` line — the split `logic.simplifyLogic` already uses
 *  with its injected `simplifyLeaf`.
 */

/** What a node demands of one of its arguments. */
enum Requirement:
  /** `> 0` — `ln(g)`, and a logarithm base. */
  case Positive
  /** `!= 0` — a denominator, or a negative integer power. */
  case NonZero
  /** `>= 0` — a fractional power, i.e. an even root. */
  case NonNegative
  /** `-1 <= g <= 1` — `asin` / `acos`. */
  case InClosedUnit
  /** `g != (2k+1)·π/2` — `tan`.  Infinitely many exclusions, so never resolves to intervals. */
  case NotOddMultipleOfHalfPi
  /** `g` is not `0, -1, -2, …` — the `Gamma` / `fact` poles.  Also infinitely many. */
  case NotNonPositiveInteger
  /** Never satisfiable.
   *
   *  Not a mathematical requirement but a statement about *this library*: the argument sits
   *  in a position the evaluator refuses, so no value makes the expression computable.  It
   *  is what makes the Asin convention expressible over the complex plane.
   */
  case Never

/** Which number system the question is being asked in. */
enum DomainKind:
  case Real, Complex

/** One requirement placed on one sub-expression. */
case class Constraint(arg: _Expression, req: Requirement)

/** A closed/open real interval; endpoints may be infinite. */
case class Interval(lo: Double, loIncl: Boolean, hi: Double, hiIncl: Boolean):
  /** Whether `x` lies inside. */
  def contains(x: Double): Boolean =
    (if loIncl then x >= lo else x > lo) && (if hiIncl then x <= hi else x < hi)

  /** Whether the interval holds no points at all. */
  def isEmpty: Boolean = lo > hi || (lo == hi && !(loIncl && hiIncl))

object Interval:
  /** The whole real line. */
  val All: Interval = Interval(Double.NegativeInfinity, false, Double.PositiveInfinity, false)

/** The domain of an expression in one variable.
 *
 *  @param constraints every requirement the expression places, always populated
 *  @param intervals   the resolved real intervals, or `None` when they could not be derived
 */
case class DomainSet(constraints: Vector[Constraint], intervals: Option[Vector[Interval]]):
  /** Whether the analysis proved the domain empty. */
  def isEmpty: Boolean =
    constraints.exists(_.req == Requirement.Never) || intervals.exists(_.isEmpty)

  /** Whether the expression is defined everywhere this analysis can see. */
  def isUnrestricted: Boolean = constraints.isEmpty

object DomainSet:
  /** No requirement at all — defined everywhere. */
  val Unrestricted: DomainSet = DomainSet(Vector.empty, Some(Vector(Interval.All)))


/** The domain of `e` as a function of `v`.
 *
 *  @param e    the expression to analyse
 *  @param v    the variable the domain is expressed in
 *  @param kind real or complex
 *  @param env  bindings, so a bound coefficient participates
 *  @return the constraints, plus intervals when they could be resolved
 */
private[leonardo] def domainOf(e: _Expression, v: _Variable, kind: DomainKind,
                               env: Environment): DomainSet =
  val cs = collectConstraints(e, kind)
  DomainSet(cs, resolveIntervals(cs, v, env))

/** The domain on which `e` is differentiable in `v`.
 *
 *  The derivative's own domain intersected with the function's — `ln(x)` is differentiable
 *  exactly where it is defined, but `x^(1/2)` loses the endpoint its derivative divides by.
 *  `_Heaviside` is defined everywhere and differentiable nowhere at its step, so it
 *  contributes a `Never`.
 *
 *  Note 4.O added `digamma`, so `Gamma` / `fact` / `lgamma` *are* differentiable now; they
 *  contribute only their own pole constraints.
 */
private[leonardo] def differentiableDomainOf(e: _Expression, v: _Variable, kind: DomainKind,
                                             env: Environment): DomainSet =
  val stepPoints = heavisideArgs(e).map(a => Constraint(a, Requirement.Never))
  val cs         = (collectConstraints(e, kind) ++
                    collectConstraints(derive(e, v), kind) ++ stepPoints).distinct
  DomainSet(cs, resolveIntervals(cs, v, env))

/** The first requirement `e` violates when `v` takes the value `x`, if any — issue 3.3
 *  slice F.
 *
 *  This is the *diagnostic* half of the issue: `limit(ln(x), x, -1)` stays symbolic today
 *  and says nothing about why, when the reason is simply that `-1` is outside `ln`'s domain.
 *
 *  Deliberately a **query, not a guard**.  Wiring the domain analysis into `eval` as a
 *  precondition would change what existing expressions evaluate to, for no gain: the caller
 *  already knows it failed, and what it lacks is the reason.  Asking afterwards keeps every
 *  existing result byte-identical.
 *
 *  Only requirements that can be *tested at a point* participate; `NotOddMultipleOfHalfPi`
 *  and `NotNonPositiveInteger` are checked too, since at a concrete point they are decidable
 *  even though their full exclusion set is not enumerable.
 *
 *  @return the violated constraint, or `None` when `x` is inside the domain or undecidable
 */
private[leonardo] def violatedAt(e: _Expression, v: _Variable, x: Double,
                                 env: Environment): Option[Constraint] =
  domainOf(e, v, DomainKind.Real, env).constraints.find { c =>
    valueAt(c.arg, v, x, env).exists(d => !satisfies(c.req, d))
  }

/** Evaluates `arg` with `v` bound to `x`. */
private def valueAt(arg: _Expression, v: _Variable, x: Double,
                    env: Environment): Option[Double] =
  arg.eval(env.withBinding(v.variable, _Number(x))) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
    case _                                              => None

/** Whether a concrete value meets a requirement. */
private def satisfies(req: Requirement, d: Double): Boolean =
  import Requirement.*
  req match
    case Positive     => d > 0
    case NonNegative  => d >= 0
    case NonZero      => d != 0
    case InClosedUnit => d >= -1 && d <= 1
    case Never        => false
    case NotOddMultipleOfHalfPi =>
      val k = d / (math.Pi / 2)
      !(math.abs(k - math.round(k)) < 1e-9 && math.round(k) % 2 != 0)
    case NotNonPositiveInteger =>
      !(d <= 0 && math.abs(d - math.round(d)) < 1e-9)

/** A human-readable form of a requirement, for diagnostics. */
private[leonardo] def describe(req: Requirement): String =
  import Requirement.*
  req match
    case Positive                => "must be > 0"
    case NonNegative             => "must be >= 0"
    case NonZero                 => "must be != 0"
    case InClosedUnit            => "must lie in [-1, 1]"
    case NotOddMultipleOfHalfPi  => "must not be an odd multiple of pi/2"
    case NotNonPositiveInteger   => "must not be zero or a negative integer"
    case Never                   => "is not computable here"

/** Arguments of every `_Heaviside` in `e` — the points where a step is not differentiable. */
private def heavisideArgs(e: _Expression): Vector[_Expression] = e match
  case _Heaviside(a) => Vector(a) ++ e.children.toVector.flatMap(heavisideArgs)
  case _             => e.children.toVector.flatMap(heavisideArgs)

/** Walks the tree and unions the requirements each node places on its arguments. */
private def collectConstraints(e: _Expression, kind: DomainKind): Vector[Constraint] =
  (own(e, kind) ++ e.children.toVector.flatMap(collectConstraints(_, kind))).distinct

/** The requirements a single node places, ignoring its children. */
private def own(e: _Expression, kind: DomainKind): Vector[Constraint] =
  import Requirement.*
  val real = kind == DomainKind.Real
  e match
    // Over the complex plane a logarithm needs only a non-zero argument; the branch cut is a
    // choice of value, not an absence of one, and `_Complex.logc` makes that choice.
    case Ln(g)         => Vector(Constraint(g, if real then Positive else NonZero))
    case LogBase(g, b) => Vector(Constraint(g, if real then Positive else NonZero),
                                 Constraint(b, if real then Positive else NonZero))

    case Ratio(_, d)   => Vector(Constraint(d, NonZero))

    // A fractional exponent is an even root over the reals; over the complex plane the
    // principal value exists for every base, so only the negative-integer case survives.
    case Power(b, _Number(n)) if n != n.toInt =>
      if real then Vector(Constraint(b, NonNegative)) else Vector.empty
    case Power(b, _Number(n)) if n < 0        => Vector(Constraint(b, NonZero))

    // The Asin convention: these stay symbolic on complex input, so over `Complex` the
    // library computes nothing at all and the honest report is an empty domain.
    // (Scala 3 forbids binding a variable inside a pattern alternative, so these are
    // three separate arms rather than one.)
    case Asin(g) => if real then Vector(Constraint(g, InClosedUnit)) else Vector(Constraint(g, Never))
    case Acos(g) => if real then Vector(Constraint(g, InClosedUnit)) else Vector(Constraint(g, Never))
    case Atan(g) => if real then Vector.empty else Vector(Constraint(g, Never))

    case Tg(g) => Vector(Constraint(g, NotOddMultipleOfHalfPi))

    case Gamma(g)      => Vector(Constraint(g, NotNonPositiveInteger))
    case Factorial(g)  => Vector(Constraint(g, NotNonPositiveInteger))
    case LogGamma(g)   => Vector(Constraint(g, NotNonPositiveInteger))

    case _ => Vector.empty


// ── Interval resolution ───────────────────────────────────────────────────────
//
// Only attempted when every constraint argument is a numeric polynomial in `v`. Two
// requirements are deliberately unresolvable: `tan`'s exclusions and the `Gamma` poles are
// both infinite sets, so a finite interval list cannot represent them and the honest answer
// is `None` rather than a truncated approximation.

/** Intersects the interval form of every constraint, or `None` if any cannot be resolved. */
private def resolveIntervals(cs: Vector[Constraint], v: _Variable,
                             env: Environment): Option[Vector[Interval]] =
  if cs.isEmpty then Some(Vector(Interval.All))
  else
    val each = cs.map(c => intervalsFor(c, v, env))
    if each.exists(_.isEmpty) then None
    else Some(each.flatten.foldLeft(Vector(Interval.All))(intersectSets))

/** The real intervals on which one constraint holds. */
private def intervalsFor(c: Constraint, v: _Variable,
                         env: Environment): Option[Vector[Interval]] =
  import Requirement.*
  c.req match
    case Never                                     => Some(Vector.empty)
    case NotOddMultipleOfHalfPi | NotNonPositiveInteger => None   // infinite exclusion set
    case Positive    => polySatisfying(c.arg, v, env, Vector(0.0), _ > 0)
    case NonNegative => polySatisfying(c.arg, v, env, Vector(0.0), _ >= 0)
    case NonZero     => polySatisfying(c.arg, v, env, Vector(0.0), _ != 0)
    case InClosedUnit =>
      polySatisfying(c.arg, v, env, Vector(-1.0, 1.0), x => x >= -1 && x <= 1)

/** Intervals where a numeric polynomial in `v` satisfies `holds`, via a sign chart.
 *
 *  **The cut points are where the polynomial crosses each `threshold`, not where it is
 *  zero.**  Those coincide for `> 0` and friends, which is why the distinction is easy to
 *  miss, but `-1 <= g <= 1` changes truth value where `g = ±1`; cutting at the roots of `g`
 *  instead reports `asin(x)` as defined on the whole line.
 */
private def polySatisfying(arg: _Expression, v: _Variable, env: Environment,
                           thresholds: Vector[Double],
                           holds: Double => Boolean): Option[Vector[Interval]] =
  numericCoeffs(arg, v, env).map { cs =>
    val cuts = thresholds.flatMap { t =>
      val shifted = shiftBy(cs, -t)                       // roots of p(v) - t
      if polyDegree(shifted) < 1 then Vector.empty[Double]
      else polyRoots(shifted).fold(Vector.empty[Double])(
             _.collect { case _Number(d) if !d.isNaN => d })
    }.sorted.distinct

    if cuts.isEmpty then
      // No crossing anywhere: one sample decides the whole line.
      if holds(polyValue(cs, 0.0)) then Vector(Interval.All) else Vector.empty
    else
      val samples = (cuts.head - 1.0) +:
                    cuts.sliding(2).collect { case Vector(a, b) => (a + b) / 2 }.toVector :+
                    (cuts.last + 1.0)
      val pieces  = samples.zipWithIndex.filter((s, _) => holds(polyValue(cs, s))).map { (_, i) =>
        val lo = if i == 0 then Double.NegativeInfinity else cuts(i - 1)
        val hi = if i == cuts.size then Double.PositiveInfinity else cuts(i)
        Interval(lo, false, hi, false)
      }
      // A crossing point itself belongs when the relation admits equality there.
      val atCuts = cuts.filter(r => holds(polyValue(cs, r))).map(r => Interval(r, true, r, true))
      merge(pieces ++ atCuts)
  }

/** `cs` with `delta` added to its constant term, i.e. the coefficients of `p(v) + delta`. */
private def shiftBy(cs: Vector[Double], delta: Double): Vector[Double] =
  if cs.isEmpty then Vector(delta) else cs.updated(0, cs(0) + delta)

/** Horner evaluation. */
private def polyValue(cs: Vector[Double], x: Double): Double =
  cs.foldRight(0.0)((c, acc) => acc * x + c)

/** `collect` plus a fold of every coefficient to a number; `None` if any stays symbolic. */
private def numericCoeffs(e: _Expression, v: _Variable, env: Environment): Option[Vector[Double]] =
  collect(e, v).flatMap { cs =>
    cs.foldRight(Option(Vector.empty[Double])) { (c, acc) =>
      for tail <- acc; d <- (c.eval(env) match
                               case Right(_Number(x)) if !x.isNaN && !x.isInfinite => Some(x)
                               case _                                              => None)
      yield d +: tail
    }
  }

/** Intersects two interval sets: every pairwise overlap, coalesced. */
private def intersectSets(a: Vector[Interval], b: Vector[Interval]): Vector[Interval] =
  merge(b.foldLeft(Vector.empty[Interval])((acc, i) => acc ++ intersect(a, i)))

/** Intersects an interval list with one more interval. */
private def intersect(acc: Vector[Interval], i: Interval): Vector[Interval] =
  acc.map { a =>
    val (lo, loIncl) =
      if i.lo > a.lo then (i.lo, i.loIncl)
      else if i.lo < a.lo then (a.lo, a.loIncl)
      else (a.lo, a.loIncl && i.loIncl)
    val (hi, hiIncl) =
      if i.hi < a.hi then (i.hi, i.hiIncl)
      else if i.hi > a.hi then (a.hi, a.hiIncl)
      else (a.hi, a.hiIncl && i.hiIncl)
    Interval(lo, loIncl, hi, hiIncl)
  }.filterNot(_.isEmpty)

/** Sorts and coalesces intervals that touch or overlap. */
private def merge(is: Vector[Interval]): Vector[Interval] =
  val sorted = is.filterNot(_.isEmpty).sortBy(i => (i.lo, !i.loIncl))
  sorted.foldLeft(Vector.empty[Interval]) { (acc, i) =>
    acc.lastOption match
      case Some(p) if i.lo < p.hi || (i.lo == p.hi && (i.loIncl || p.hiIncl)) =>
        val hiIncl = if i.hi > p.hi then i.hiIncl else if i.hi < p.hi then p.hiIncl else p.hiIncl || i.hiIncl
        acc.init :+ Interval(p.lo, p.loIncl, math.max(p.hi, i.hi), hiIncl)
      case _ => acc :+ i
  }
