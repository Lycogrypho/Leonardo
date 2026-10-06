package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*


/** Upper bound on the recursive steps one [[eliminate]] call may take.
 *
 *  Each step is one elimination (a substitution or a branch on roots); the bound caps the
 *  branching, which is what can grow.  **Measured, not round** (F_0010, JVM): the acceptance
 *  systems take 3 to 8 steps.  A system whose stationary points double with every variable
 *  (`Σ xᵢ³ − 3xᵢ`) takes `2ⁿ⁺¹ − 1` steps — 2047 steps and 0.13 s for its 1024 points at
 *  `n = 10` — and at `n = 11` (2048 points) passes the cap and declines after 0.16 s.  So the
 *  cap bounds an answer near two thousand points and its cost well under a second.
 */
val MaxEliminationSteps: Int = 4000

/** A solution of a system: each unknown's name mapped to its value.
 *
 *  A value is a number when the system had numeric coefficients, and an expression in the
 *  free parameters when it did not.
 */
type SolutionPoint = Map[String, _Expression]


/** Every real solution of `eqs = 0` in `unknowns`, or `None` when that set cannot be proved
 *  complete (F_0010, Decision B: elimination with case splits).
 *
 *  `equation.solveSystem` is linear-only, and the conditions an optimum must satisfy are not
 *  linear in general — a nonlinear constraint makes `λ·∇g` bilinear, and even the decoupled
 *  `x³ − 3x + y²` has a quadratic gradient.  This solver reaches them by elimination:
 *
 *  1. **An equation linear in some unknown with a constant coefficient** is solved for it and
 *     substituted — no branching, and exact on exact input, so a linear system comes out in
 *     the exact tier.
 *  2. **An equation in a single unknown** is solved for *every* real root (square-free
 *     factorisation, then the roots of each part — the 2.5 rule, since QR can fail on a repeated
 *     root) and the search branches on each.
 *  3. **An equation linear in an unknown whose coefficient `c` depends on the others** is solved
 *     for it on one branch, and on a second branch replaced by `c = 0` and the constant term
 *     `= 0`.  **The second branch is the point**: dividing by `c` silently drops every solution
 *     where `c` vanishes, and a stationary-point list is read as *all* of them.
 *
 *  It **declines** — `None` — when no step applies, when a branch is left with free unknowns
 *  and no equations (a continuum of solutions, which no list can hold), when a univariate
 *  equation's roots cannot be enumerated (a transcendental equation, or symbolic coefficients
 *  above degree one), and past [[MaxEliminationSteps]].  It never returns what it found so far
 *  as though it were everything.
 *
 *  **Every candidate is verified** against the original equations under the tolerance `=` uses
 *  (`0.5·10^-precision`).  A candidate at which an equation is *undefined* is dropped — that is
 *  the artefact of a step-3 division where the coefficient vanished, which the second branch
 *  covers.  A candidate that is defined but misses the tolerance makes the whole answer
 *  decline, since that is not an artefact the algorithm can explain.  A candidate carrying free
 *  parameters is verified numerically at fixed sample values of those parameters.
 *
 *  Symbolic coefficients are allowed where step 1 or 3 handles them, and are assumed non-zero
 *  there — the convention `solveSystem`'s symbolic path already follows.
 *
 *  @param eqs      the left-hand sides of `eq = 0`
 *  @param unknowns the unknowns, in the order the answer reports them
 *  @param env      bindings and precision; a bound parameter is substituted, a bound unknown
 *                  is ignored (it is being solved for)
 *  @return every real solution, deduplicated; `Some(Nil)` when there provably are none
 */
def eliminate(eqs: List[_Expression], unknowns: List[_Variable],
              env: Environment): Option[List[SolutionPoint]] =
  val names     = unknowns.map(_.variable).toSet
  val params    = boundParameters(eqs, names, env)
  val prepared  = eqs.map(e => simplifyFully(substitute(e, params)))
  val tolerance = 0.5 * math.pow(10, -env.precision)
  var steps     = 0

  // `None` = decline the whole problem; `Some(list)` = every solution of this branch.
  def go(eqs: List[_Expression], left: List[_Variable],
         assigned: Map[String, _Expression]): Option[List[SolutionPoint]] =
    steps += 1
    if steps > MaxEliminationSteps then None
    else
      val leftNames = left.map(_.variable).toSet
      reduceConstantEquations(eqs, leftNames, env, tolerance) match
        case Decided.Undecidable  => None
        case Decided.Inconsistent => Some(Nil)
        case Decided.Remaining(rest) =>
          if left.isEmpty then Some(List(assigned))
          else if rest.isEmpty then None          // free unknowns, no equations: a continuum
          else
            // The equation `used` is the one `u` was solved from, so it is satisfied by
            // construction and is dropped rather than substituted back: for a value in the
            // parameters (`x = a/2`) the back-substituted residue is a cancellation
            // `simplify` does not perform, and would read as undecidable.
            def assign(u: _Variable, value: _Expression, used: _Expression): Option[List[SolutionPoint]] =
              val bind     = Map(u.variable -> value)
              val nextEqs  = rest.filterNot(_ eq used).map(e => simplifyFully(substitute(e, bind)))
              val nextAsg  = assigned.view.mapValues(v => simplifyFully(substitute(v, bind))).toMap +
                             (u.variable -> value)
              go(nextEqs, left.filterNot(_ == u), nextAsg)

            linearConstantStep(rest, left).map((e, u, value) => assign(u, value, e))
              .orElse(univariateStep(rest, left).map { (e, u, roots) =>
                sequence(roots.toList.map(r => assign(u, _Number(r), e))).map(_.flatten)
              })
              .orElse(linearBranchStep(rest, left).map { (e, u, c0, c1) =>
                val divided = assign(u, solveLinear(c0, c1), e)
                val vanish  = go(c1 :: c0 :: rest.filterNot(_ == e), left, assigned)
                for a <- divided; b <- vanish yield a ++ b
              })
              .getOrElse(None)

  go(prepared, unknowns, Map.empty).flatMap { candidates =>
    verified(candidates, prepared, unknowns, env, tolerance)
  }


/** Values of parameters that are bound in `env`, so the solver sees numbers rather than names.
 *
 *  An unknown is never substituted, even when bound: it is what is being solved for. */
private def boundParameters(eqs: List[_Expression], unknowns: Set[String],
                            env: Environment): Map[String, _Expression] =
  eqs.flatMap(_.freeVars).distinct.filterNot(unknowns.contains)
    .flatMap(n => env.get(n).collect { case v @ _Number(_) => n -> (v: _Expression) }).toMap

/** The outcome of checking the equations that no longer mention any unknown. */
private enum Decided:
  /** One of them is a non-zero constant: this branch has no solutions. */
  case Inconsistent
  /** One of them could not be decided (a residue in free parameters that does not vanish). */
  case Undecidable
  /** They all hold; these are the equations still to solve. */
  case Remaining(eqs: List[_Expression])

/** Drops the equations free of every remaining unknown, deciding each one on the way. */
private def reduceConstantEquations(eqs: List[_Expression], left: Set[String],
                                    env: Environment, tolerance: Double): Decided =
  val (constant, rest) = eqs.partition(e => e.freeVars.intersect(left).isEmpty)
  val verdicts = constant.map(e => constantVerdict(e, env, tolerance))
  if verdicts.contains(Some(false)) then Decided.Inconsistent
  else if verdicts.contains(None) then Decided.Undecidable
  else Decided.Remaining(rest)

/** Whether a constant equation `e = 0` holds: `Some(true)`, `Some(false)`, or `None` when
 *  it cannot be decided.
 *
 *  An *undefined* constant (a `1/0` left by a division whose coefficient vanished) is
 *  `Some(false)`: that branch has no solution, and the division's companion branch is the
 *  one that covers the case. */
private def constantVerdict(e: _Expression, env: Environment, tolerance: Double): Option[Boolean] =
  e.eval(env) match
    case Right(r: _Rational)  => Some(r.isZero)
    case Right(_Number(d))    => Some(!d.isNaN && math.abs(d) <= tolerance)
    case Right(_: _Value)     => Some(false)           // complex, matrix, truth value
    case Left(residue)        =>
      if residue.freeVars.isEmpty then Some(false)     // undefined, not undecided
      else if isZero(simplifyFully(residue)) || vanishesAtSamples(residue, env, tolerance) then Some(true)
      // Non-zero for some parameter values: it may still hold for others (`a = 0`), so
      // the branch's validity depends on the parameters and cannot be decided here.
      else None

/** Step 1: an equation linear in an unknown whose coefficient is free of every unknown.
 *
 *  Such a coefficient is a number or an expression in the parameters, and is assumed
 *  non-zero — `solveSystem`'s convention.  A coefficient that is *provably* zero is not
 *  linear at all and `collect` would not report it. */
private def linearConstantStep(eqs: List[_Expression],
                               left: List[_Variable]): Option[(_Expression, _Variable, _Expression)] =
  val leftNames = left.map(_.variable).toSet
  (for
    e <- eqs.iterator
    u <- left.iterator
    cs <- collect(e, u).iterator
    if cs.sizeIs == 2 && cs(1).freeVars.intersect(leftNames).isEmpty && !isZero(cs(1))
  yield (e, u, solveLinear(cs(0), cs(1)))).nextOption()

/** Step 2: an equation in exactly one unknown, with every real root enumerated. */
private def univariateStep(eqs: List[_Expression],
                           left: List[_Variable]): Option[(_Expression, _Variable, Vector[Double])] =
  (for
    e <- eqs.iterator
    u <- left.iterator
    if e.freeVars == Set(u.variable)
    roots <- realRoots(e, u).iterator
  yield (e, u, roots)).nextOption()

/** Step 3: an equation linear in an unknown whose coefficient depends on the others.
 *
 *  @return the equation, the unknown, and the constant and linear coefficients */
private def linearBranchStep(eqs: List[_Expression], left: List[_Variable])
    : Option[(_Expression, _Variable, _Expression, _Expression)] =
  (for
    e <- eqs.iterator
    u <- left.iterator
    cs <- collect(e, u).iterator
    if cs.sizeIs == 2 && !isZero(cs(1))
  yield (e, u, cs(0), cs(1))).nextOption()

/** `u = -c0 / c1`, with the `-1` built in the tier of the coefficients (the numeric tier rule). */
private def solveLinear(c0: _Expression, c1: _Expression): _Expression =
  simplifyFully(Ratio(Product(_Rational.literalLike(-1, Sum(c0, c1)), c0), c1))

/** Whether a coefficient is the literal zero, in either tier. */
private def isZero(e: _Expression): Boolean = e match
  case r: _Rational => r.isZero
  case _Number(d)   => d == 0.0
  case _            => false

/** Every real root of `e = 0` in `u`, or `None` when they cannot all be enumerated.
 *
 *  The equation is folded into one polynomial fraction; the roots are those of the
 *  numerator at which the denominator does not vanish.  An identically-zero numerator is
 *  reported as no constraint at all would be — it is not, so it declines instead: a vacuous
 *  equation in the last unknown leaves a continuum. */
private def realRoots(e: _Expression, u: _Variable): Option[Vector[Double]] =
  rationalCoeffs(e, u).flatMap { (num, den) =>
    val n = polyTrim(num)
    polyDegree(n) match
      case d if d < 0 => None
      case 0          => Some(Vector.empty)
      case _ =>
        sequence(squareFreeFactors(n).map((p, _) => rootsOfSquareFree(p)).toList).map { parts =>
          val denScale = den.map(math.abs).sum
          parts.flatten
            // A real root found through the eigenvalues can carry a vanishing imaginary part.
            .collect { case (re, im) if math.abs(im) <= 1e-9 * math.max(1.0, math.abs(re)) => re }
            .filter(r => math.abs(evalCoeffsReal(den, r)) > 1e-12 * math.max(1.0, denScale))
            .distinct.toVector
        }
    }

/** `Some` of every value when all are `Some`, else `None`. */
private[optimize] def sequence[A](xs: List[Option[A]]): Option[List[A]] =
  xs.foldRight(Option(List.empty[A]))((x, acc) => for a <- x; as <- acc yield a :: as)


/** Sample values given to free parameters when a symbolic candidate is verified numerically.
 *
 *  Irrational-looking and of mixed sign, so a residue that vanishes at all three is very
 *  unlikely to vanish by coincidence. */
private val ParameterSamples: Vector[Double] = Vector(0.7316, 1.3719, -0.4417)

/** The candidates that satisfy every original equation, deduplicated and ordered; `None`
 *  when a defined candidate misses the tolerance (see [[eliminate]]). */
private def verified(candidates: List[SolutionPoint], eqs: List[_Expression],
                     unknowns: List[_Variable], env: Environment,
                     tolerance: Double): Option[List[SolutionPoint]] =
  val evaluated = candidates.map(_.view.mapValues(v => v.eval(env).toExpression).toMap)
  val checks    = evaluated.map(p => (p, satisfies(p, eqs, env, tolerance)))
  if checks.exists(_._2.isEmpty) then None
  else
    val kept = checks.collect { case (p, Some(true)) if isRealPoint(p) => p }
    Some(dedupe(kept, unknowns, tolerance).sortBy(p => sortKey(p, unknowns)))

/** `Some(true)` when the point satisfies every equation, `Some(false)` when one is undefined
 *  there, `None` when one is defined and fails. */
private def satisfies(point: SolutionPoint, eqs: List[_Expression], env: Environment,
                      tolerance: Double): Option[Boolean] =
  val residues = eqs.map(e => substitute(e, point))
  val verdicts = residues.map(r => residueVerdict(r, env, tolerance))
  if verdicts.contains(None) then None
  else Some(verdicts.forall(_.contains(true)))

/** `Some(true)` holds, `Some(false)` undefined, `None` defined but failing. */
private def residueVerdict(r: _Expression, env: Environment, tolerance: Double): Option[Boolean] =
  if r.freeVars.isEmpty then
    r.eval(env) match
      case Right(q: _Rational)                    => if q.isZero then Some(true) else None
      case Right(_Number(d)) if d.isNaN || d.isInfinite => Some(false)
      case Right(_Number(d))                      => if math.abs(d) <= tolerance then Some(true) else None
      case Right(_)                               => Some(false)
      case Left(_)                                => Some(false)
  else
    val values = sampled(r, env)
    if values.isEmpty then Some(false)
    else if values.forall(d => math.abs(d) <= tolerance) then Some(true)
    else None

/** The finite values of `r` with its free parameters bound to [[ParameterSamples]], each
 *  parameter to a different sample so no two are tested only at equal values. */
private def sampled(r: _Expression, env: Environment): Vector[Double] =
  val names = r.freeVars.toList.sorted
  ParameterSamples.indices.toVector.flatMap { k =>
    val bound = names.zipWithIndex.foldLeft(env) { case (acc, (n, i)) =>
      acc.withBinding(n, _Number(ParameterSamples((k + i) % ParameterSamples.size)))
    }
    r.eval(bound) match
      case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
      case _                                               => None
  }

/** Whether a residue in the parameters vanishes at every sample where it is defined. */
private def vanishesAtSamples(r: _Expression, env: Environment, tolerance: Double): Boolean =
  val values = sampled(r, env)
  values.nonEmpty && values.forall(d => math.abs(d) <= tolerance)

/** Whether no coordinate of the point is a non-real number. */
private def isRealPoint(p: SolutionPoint): Boolean =
  p.values.forall { case _: _Complex => false; case _ => true }

/** Drops points equal, coordinate by coordinate, to one kept earlier. */
private def dedupe(points: List[SolutionPoint], unknowns: List[_Variable],
                   tolerance: Double): List[SolutionPoint] =
  def same(a: SolutionPoint, b: SolutionPoint): Boolean = unknowns.forall { u =>
    (a.get(u.variable), b.get(u.variable)) match
      case (Some(_Number(x)), Some(_Number(y))) => math.abs(x - y) <= tolerance
      case (x, y)                               => x == y
  }
  points.foldLeft(List.empty[SolutionPoint])((acc, p) => if acc.exists(same(_, p)) then acc else acc :+ p)

/** Numeric points first, in lexicographic order of their coordinates. */
private def sortKey(p: SolutionPoint, unknowns: List[_Variable]): (Int, List[Double]) =
  val coords = unknowns.map(u => p.get(u.variable).collect { case _Number(d) => d })
  if coords.forall(_.isDefined) then (0, coords.flatten) else (1, Nil)

private given Ordering[List[Double]] = Ordering.Implicits.seqOrdering[List, Double]
