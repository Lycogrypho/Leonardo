package it.grypho.scala.leonardo
package logic

import core.*

import scala.collection.mutable
import scala.util.control.ControlThrowable


/** Why [[minimalForm]] declined (F_0325 step 2).  The node stays unevaluated and the REPL appends why. */
enum MinimiseFailure:
  /** A graded truth value occurs: a two-level form is a two-valued notion, like the normal forms. */
  case Graded
  /** More atoms than `Limit.MaxMinimiseVars` (the table has `2ⁿ` rows); `limit` is the cap in force. */
  case TooManyAtoms(atoms: Int, limit: Int)
  /** The search passed `Limit.MaxMinimiseWork`: choosing a minimum cover is NP-hard, and this is what stops
   *  a cyclic core. */
  case OverBudget

/** A minimal two-level form, and how many there are.
 *
 *  @param expr  the form: a sum of products or a product of sums, or a constant
 *  @param count how many forms of the same, least cost exist (at least 1); `expr` is the first of them in
 *               the canonical order of [[minimalForm]] */
final case class MinimalForm(expr: _Expression, count: Long)

/** Which minimal form a [[_MinimalForm]] asks for, with its grammar keyword. */
enum MinimalKind(val word: String) extends Keyworded:
  /** The grammar keyword. */
  def keyword: String = word
  /** `mindnf(e)` -- a minimal sum of products. */
  case MinDNF extends MinimalKind("mindnf")
  /** `mincnf(e)` -- a minimal product of sums. */
  case MinCNF extends MinimalKind("mincnf")


/** A minimal two-level form of `e` (F_0325 step 2, LT L_0081): the Karnaugh-map exercise, by
 *  Quine–McCluskey and an exact minimum cover.
 *
 *  **What "minimal" means** (Decision A): the fewest terms, then the fewest literals -- the reading of a
 *  Karnaugh map, fewest groups each as large as it can be, and Espresso's order.  **Several minimal forms**
 *  (Decision B): a cyclic cover has two or more of equal cost; one is answered, **the first in a fixed
 *  canonical order**, identical on the JVM and Node, and [[MinimalForm.count]] says how many there are.
 *  **Atoms** (Decision C): as for `cnf` and `dnf`, any subexpression that is not a connective is an opaque
 *  atom, so `x > 0 and y` has two; the atoms are ordered by their printed form, a name by itself.  The
 *  canonical order of the terms is lexicographic over their literals in that atom order, a positive
 *  literal before a negative one, a prefix before its extension.
 *
 *  The formula is simplified first (the `cnf` pre-pass), the truth vector read off over its atoms, and
 *  the prime implicants found; `mincnf` is the dual, the minimal sum of products of `not e` with every
 *  literal negated and the connectives swapped.  Crisp only: a graded truth value is declined.
 *
 *  @param e           the formula
 *  @param conjunctive `false` for a sum of products (`mindnf`), `true` for a product of sums (`mincnf`)
 *  @param symmetric   whether the symmetric ternary digits are in scope
 *  @param limits      the budgets: atoms (`MaxMinimiseVars`, at most 15) and work (`MaxMinimiseWork`)
 *  @return the form and the number of minimal forms, or why there is none
 */
def minimalForm(e: _Expression, conjunctive: Boolean, symmetric: Boolean = false,
                limits: Limits = Limits.Default): Either[MinimiseFailure, MinimalForm] =
  if !isCrisp(e, symmetric) then Left(MinimiseFailure.Graded)
  else
    val simplified = simplifyLogicFully(e, identity, symmetric)
    val atoms      = atomsOf(simplified)
    val cap        = math.min(limits(Limit.MaxMinimiseVars), MaxMinimiseAtoms.toLong).toInt
    if atoms.size > cap then Left(MinimiseFailure.TooManyAtoms(atoms.size, cap))
    else
      val holds = evaluator(simplified, atoms)
      val rows  = 1 << atoms.size
      val work  = limits(Limit.MaxMinimiseWork)
      // reading the truth vector is one pass over the formula per row, and a wide formula over 2¹⁵ rows is
      // not free: it is charged to the same budget, before it is spent
      val reading = rows.toLong * sizeOf(simplified)
      if reading > work then Left(MinimiseFailure.OverBudget)
      else
        // the rows the form must cover: where the formula is true for a sum of products, false for a product
        // of sums (the minimal sum of products of the negation, read back by duality)
        val ones = (0 until rows).filter(m => holds(m) != conjunctive).toArray
        if ones.isEmpty then Right(MinimalForm(_Bool(conjunctive), 1))
        else if ones.length == rows then Right(MinimalForm(_Bool(!conjunctive), 1))
        else
          minimise(atoms.size, ones, work - reading) match
            case None        => Left(MinimiseFailure.OverBudget)
            case Some(cover) => Right(MinimalForm(rebuilt(atoms, cover, conjunctive), cover.count))


/** `mindnf(e)` / `mincnf(e)` -- a minimal two-level form at the prompt (F_0325 step 2).
 *
 *  The `cnf`/`dnf` node's shape (F_0133): the operand is evaluated first, so a bound name takes its value
 *  and a concrete truth value is its own minimal form, and **the node stays unevaluated on a refusal**
 *  (a graded truth value, too many atoms, a search past its budget) rather than answering the input as
 *  though it were minimal.  [[detailed]] carries the reason and the count, for the REPL's note.
 *
 *  @param kind which form
 *  @param e    the logical expression
 */
case class _MinimalForm(kind: MinimalKind, e: _Expression) extends _Expression:
  override def toString: String = s"${kind.word}($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _MinimalForm(kind, c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env) match
      case Right(MinimalForm(v: _Value, _)) => Right(v)
      case Right(MinimalForm(x, _))         => Left(x)
      case Left(_)                          => Left(_MinimalForm(kind, e.eval(env).toExpression))

  private val answers = LastAnswer[Either[MinimiseFailure, MinimalForm]]()

  /** The form and its count, or why there is none; computed once per environment, so the REPL's note
   *  reads the search `eval` ran (F_0212).
   *
   *  @param env bindings, the symmetric encoding and the budgets
   *  @return the minimal form, or the reason the node stays unevaluated */
  def detailed(env: Environment): Either[MinimiseFailure, MinimalForm] = answers(env) {
    e.eval(env) match
      case Right(v) => Right(MinimalForm(v, 1))
      case Left(x)  => minimalForm(x, kind == MinimalKind.MinCNF, env.symmetricLogic, env.limits)
  }


// ── the formula, as a truth vector over its atoms ──────────────────────────────────────────────────

/** The atoms of `e`: its non-connective subexpressions other than the boolean constants, each once, ordered
 *  by printed form -- a name by itself, so `p` before `q` and a relation, which opens with a bracket,
 *  before both. */
private def atomsOf(e: _Expression): Vector[_Expression] =
  def go(x: _Expression): List[_Expression] = x match
    case And(a, b)     => go(a) ++ go(b)
    case Or(a, b)      => go(a) ++ go(b)
    case Xor(a, b)     => go(a) ++ go(b)
    case Implies(a, b) => go(a) ++ go(b)
    case Iff(a, b)     => go(a) ++ go(b)
    case Not(a)        => go(a)
    case _Bool(_)      => Nil
    case atom          => List(atom)
  go(e).distinct.sortBy(_.toString).toVector

/** The number of nodes of `e`: what one evaluation of the formula costs. */
private def sizeOf(e: _Expression): Int = 1 + e.children.map(sizeOf).sum

/** The truth value of the connective skeleton of `e` at row `m`, bit `i` of `m` being atom `i`. */
private def evaluator(e: _Expression, atoms: Vector[_Expression]): Int => Boolean =
  val index = atoms.zipWithIndex.toMap
  def build(x: _Expression): Int => Boolean = x match
    case And(a, b)     => val (f, g) = (build(a), build(b)); m => f(m) && g(m)
    case Or(a, b)      => val (f, g) = (build(a), build(b)); m => f(m) || g(m)
    case Xor(a, b)     => val (f, g) = (build(a), build(b)); m => f(m) != g(m)
    case Implies(a, b) => val (f, g) = (build(a), build(b)); m => !f(m) || g(m)
    case Iff(a, b)     => val (f, g) = (build(a), build(b)); m => f(m) == g(m)
    case Not(a)        => val f = build(a); m => !f(m)
    case _Bool(b)      => _ => b
    case atom          => val i = index(atom); m => ((m >> i) & 1) == 1
  build(e)

/** The answer as a formula: each term `And`ed in atom order, the terms `Or`ed in canonical order -- for a
 *  product of sums, each term of the negation's cover turned into a clause by negating its literals. */
private def rebuilt(atoms: Vector[_Expression], cover: Cover, conjunctive: Boolean): _Expression =
  def literal(code: Int): _Expression = if (code & 1) == 1 then Not(atoms(code / 2)) else atoms(code / 2)
  val groups   = if conjunctive then cover.terms.map(_.map(_ ^ 1)).sortWith(lexLess) else cover.terms
  val inner: (_Expression, _Expression) => _Expression = if conjunctive then Or.apply else And.apply
  val outer: (_Expression, _Expression) => _Expression = if conjunctive then And.apply else Or.apply
  groups.map(_.map(literal).reduceLeft(inner)).reduceLeft(outer)


// ── Quine–McCluskey and the minimum cover ───────────────────────────────────────────────────────────

/** A minimum cover of the rows `ones` of an `n`-variable function by its prime implicants.
 *
 *  @param terms the terms, each as sorted literal codes `atom * 2 + (1 if negated)`, in canonical order
 *  @param count how many covers of the least cost exist */
private[logic] final case class Cover(terms: Vector[Vector[Int]], count: Long)

/** Thrown, and caught in [[minimise]], when the work budget is spent: one private control throwable rather
 *  than an `Option` through every loop, the `ExactBudget` arrangement. */
private object BudgetSpent extends ControlThrowable

private final class Budget(private var left: Long):
  def spend(units: Long): Unit =
    left -= units
    if left < 0 then throw BudgetSpent

/** The cost of a term: a unit large enough that no number of literals outweighs a term, so costs compare
 *  by terms first and literals second (`2²⁴` exceeds `20` literals in `65 536` terms). */
private val TermUnit: Long = 1L << 24

/** The most atoms a minimal form is searched over, whatever `Limit.MaxMinimiseVars` says: a cube is its
 *  fixed bits (`value`) and its free ones (`mask`), packed as `mask << n | value` into one `Int`, which
 *  holds up to 15 atoms -- and one `Int` is what keeps the prime generation free of boxing, a `Long` being
 *  an object under Scala.js as well. */
private val MaxMinimiseAtoms = 15

/** The literal codes of a cube over `n` atoms, ascending: `atom * 2`, plus 1 where the atom must be false. */
private def termCodes(n: Int, cube: Int): Vector[Int] =
  val (v, m) = (cube & ((1 << n) - 1), cube >>> n)
  (0 until n).filter(i => ((m >> i) & 1) == 0).map(i => i * 2 + (1 - ((v >> i) & 1))).toVector

/** A set of cubes (non-negative `Int`s) as an open-addressing table of primitives.  A `HashSet[Long]` boxed
 *  every cube, which made a unit of prime-generation work about ten times dearer than one of the search
 *  (a 14-atom majority function: 7.8 s for under ten million units). */
private final class CubeSet(expected: Int):
  private var table = Array.fill(CubeSet.capacityFor(expected))(-1)
  private var used  = 0

  def size: Int = used
  def capacity: Int = table.length
  def keyAt(slot: Int): Int = table(slot)

  private def home(key: Int, mask: Int): Int =
    val h = key * 0x9E3779B1
    (h ^ (h >>> 15)) & mask

  /** The slot holding `key`, or -1. */
  def find(key: Int): Int =
    val mask = table.length - 1
    var s = home(key, mask)
    while table(s) != -1 && table(s) != key do s = (s + 1) & mask
    if table(s) == key then s else -1

  def add(key: Int): Unit =
    if (used + 1) * 2 > table.length then grow()
    insert(key)

  private def insert(key: Int): Unit =
    val mask = table.length - 1
    var s = home(key, mask)
    while table(s) != -1 && table(s) != key do s = (s + 1) & mask
    if table(s) == -1 then
      table(s) = key
      used += 1

  private def grow(): Unit =
    val old = table
    table = Array.fill(old.length * 2)(-1)
    used = 0
    for key <- old if key != -1 do insert(key)

private object CubeSet:
  def capacityFor(expected: Int): Int =
    var c = 16
    while c < expected * 2 + 2 do c <<= 1
    c

/** Lexicographic order over literal codes, a prefix before its extension. */
private def lexLess(a: Vector[Int], b: Vector[Int]): Boolean =
  val common = math.min(a.length, b.length)
  var i = 0
  while i < common && a(i) == b(i) do i += 1
  if i < common then a(i) < b(i) else a.length < b.length

/** The minimum cover of `ones` (the rows where an `n`-variable function holds, neither none nor all), or
 *  `None` when the search outruns `work`.
 *
 *  @param n    the number of atoms, at most 15
 *  @param ones the rows to cover, ascending, bit `i` of a row being atom `i`
 *  @param work the budget, in the units of [[Limit.MaxMinimiseWork]] */
private[logic] def minimise(n: Int, ones: Array[Int], work: Long): Option[Cover] =
  val budget = Budget(work)
  try
    val found = primeImplicants(n, ones, budget)
    budget.spend(found.length.toLong * 20)                       // the canonical sort, about log2 of a million compares each
    val primes = found
      .map(cube => (termCodes(n, cube), cube))
      .sortWith((x, y) => lexLess(x._1, y._1))
      .toVector
    Some(cheapestCover(n, ones, primes, budget))
  catch case BudgetSpent => None

/** Every prime implicant of the function that holds on `ones`, by repeated merging: two cubes with the same
 *  free bits that differ in one fixed bit merge into one with that bit free, and a cube that merges with
 *  nothing is prime.  The partner is found by a hash lookup, so a level costs `cubes · n` lookups.
 *
 *  Work: `n` per cube per level. */
private def primeImplicants(n: Int, ones: Array[Int], budget: Budget): Array[Int] =
  val low    = (1 << n) - 1
  var level  = CubeSet(ones.length)
  ones.foreach(level.add)                                       // a minterm is a cube with nothing free
  val primes = mutable.ArrayBuffer.empty[Int]
  while level.size > 0 do
    val next   = CubeSet(level.size)
    val merged = new Array[Boolean](level.capacity)             // by slot: this cube merged with a partner
    var s = 0
    while s < level.capacity do
      val cube = level.keyAt(s)
      if cube != -1 then
        budget.spend(n)
        val (v, m) = (cube & low, cube >>> n)
        var i = 0
        while i < n do
          val bit = 1 << i
          if (m & bit) == 0 && (v & bit) == 0 then
            val partner = level.find((m << n) | (v | bit))
            if partner >= 0 then
              next.add(((m | bit) << n) | v)
              merged(s) = true
              merged(partner) = true
          i += 1
      s += 1
    s = 0
    while s < level.capacity do
      if level.keyAt(s) != -1 && !merged(s) then primes += level.keyAt(s)
      s += 1
    level = next
  primes.toArray

/** The cheapest cover of `ones` by `primes`, in the order of Decision A, with the number of covers of that
 *  cost: the essential primes (a row only one covers), then a branch and bound over the rest.
 *
 *  **Every minimal cover is counted, so nothing that could drop one is used**: a dominated column is kept
 *  (an equal-cost prime is an equal-cost cover), and the bound only prunes what is strictly dearer.  Each
 *  cover is reached once: branching on the row with the fewest candidates, the `i`-th candidate's branch
 *  forbids the `i - 1` before it.  The explicit stack keeps the depth off the call stack, the shallowest
 *  platform's limit (Node's) being the one that matters.
 *
 *  Work: the cells of the cover table, then per node one plus its uncovered rows and the candidates scanned. */
private def cheapestCover(n: Int, ones: Array[Int], primes: Vector[(Vector[Int], Int)], budget: Budget): Cover =
  val k = ones.length
  val p = primes.length
  val position = new Array[Int](1 << n)
  for i <- 0 until k do position(ones(i)) = i

  val cost     = Array.tabulate(p)(i => TermUnit + primes(i)._1.length)
  val covers   = new Array[Array[Int]](p)                       // prime -> the rows it covers
  val coveredBy = Array.fill(k)(mutable.ArrayBuffer.empty[Int]) // row -> the primes covering it, ascending
  for pi <- 0 until p do
    val (v, m) = (primes(pi)._2 & ((1 << n) - 1), primes(pi)._2 >>> n)
    val cells = mutable.ArrayBuffer.empty[Int]
    var sub   = m
    var more  = true
    while more do                                               // every row of the cube: the submasks of m
      val row = position(v | sub)
      cells += row
      coveredBy(row) += pi
      if sub == 0 then more = false else sub = (sub - 1) & m
    budget.spend(cells.length)
    covers(pi) = cells.toArray
  val maxGain = covers.map(_.length).max
  val minLits = primes.map(_._1.length).min

  val essential = mutable.TreeSet.empty[Int]
  for row <- 0 until k if coveredBy(row).length == 1 do essential += coveredBy(row)(0)
  val uncovered = mutable.BitSet.empty
  uncovered ++= (0 until k)
  for e <- essential do covers(e).foreach(uncovered -= _)
  val baseCost = essential.toList.map(cost(_)).sum

  // a greedy cover seeds the bound, so the search prunes from the first node; it is found again by the
  // search, which keeps every cover of cost <= the bound, so the count is unaffected
  val (greedyCost, greedyChosen) =
    val unc = uncovered.clone()
    var total  = baseCost
    var chosen = List.empty[Int]
    while unc.nonEmpty do
      var pick = -1
      var gain = -1
      for pi <- coveredBy(unc.head) do
        budget.spend(covers(pi).length)
        val g = covers(pi).count(unc.contains)
        if g > gain then { gain = g; pick = pi }
      covers(pick).foreach(unc -= _)
      total += cost(pick)
      chosen ::= pick
    (total, chosen)

  var best      = greedyCost
  var bestCover = Option(greedyChosen.sorted.toVector)
  var count     = 0L

  final class Frame(val unc: mutable.BitSet, val forbidden: mutable.BitSet, val soFar: Long,
                    val chosen: List[Int], val candidates: Array[Int]):
    var next = 0

  /** A complete cover.  Only one no dearer than the best is looked at, and it is charged for the sort and
   *  the comparison it costs: a unit apiece let a search over covers of a hundred primes run for minutes
   *  inside a budget of ten million. */
  def leaf(total: Long, chosen: List[Int]): Unit =
    if total <= best then
      budget.spend(1L + 8L * chosen.length)
      val cover = chosen.sorted.toVector
      if total < best then
        best = total; count = 1; bestCover = Some(cover)
      else
        count += 1
        if bestCover.forall(lexLess(cover, _)) then bestCover = Some(cover)

  /** A node: a leaf when nothing is uncovered, nothing when the bound or a row with no candidate rules it
   *  out, else the frame over the candidates of the row with the fewest. */
  def open(unc: mutable.BitSet, forbidden: mutable.BitSet, soFar: Long, chosen: List[Int]): Option[Frame] =
    val remaining = unc.size
    budget.spend(1L + remaining)
    if remaining == 0 then
      leaf(soFar, chosen)
      None
    else
      // each further term covers at most maxGain rows and holds at least minLits literals
      val byGain = ((remaining + maxGain - 1) / maxGain).toLong
      if soFar + byGain * (TermUnit + minLits) > best then None
      else
        var pick    = -1
        var fewest  = Int.MaxValue
        var scanned = 0L
        // a packing: rows no two of which share an allowed prime each need a term of their own
        val used    = mutable.BitSet.empty
        var packed  = 0L
        for row <- unc do
          var allowed = 0
          var apart   = true
          for pi <- coveredBy(row) if !forbidden.contains(pi) do
            allowed += 1
            if used.contains(pi) then apart = false
          scanned += coveredBy(row).length
          if allowed < fewest then { fewest = allowed; pick = row }
          if apart && allowed > 0 then
            packed += 1
            for pi <- coveredBy(row) if !forbidden.contains(pi) do used += pi
        budget.spend(scanned)
        if fewest == 0 || soFar + math.max(byGain, packed) * (TermUnit + minLits) > best then None
        else
          // the candidates covering most of what is still uncovered first: the search finds its best early
          val allowed = coveredBy(pick).filterNot(forbidden.contains)
          budget.spend(allowed.map(covers(_).length).sum.toLong)
          // the gain is computed once per candidate: `sortBy` would recompute it at every comparison, a scan of
          // the whole cover each time and none of it counted -- a 10⁷-unit budget once took 199 s
          val ordered = allowed.map(pi => (-covers(pi).count(unc.contains), pi)).sorted.map(_._2).toArray
          Some(Frame(unc, forbidden, soFar, chosen, ordered))

  val stack = mutable.ArrayBuffer.empty[Frame]
  open(uncovered.clone(), mutable.BitSet.empty, baseCost, Nil).foreach(stack += _)
  while stack.nonEmpty do
    val top = stack.last
    if top.next >= top.candidates.length then stack.remove(stack.length - 1)
    else
      val pick      = top.candidates(top.next)
      val forbidden = top.forbidden.clone()
      var j = 0
      while j < top.next do { forbidden += top.candidates(j); j += 1 }
      top.next += 1
      val unc = top.unc.clone()
      covers(pick).foreach(unc -= _)
      open(unc, forbidden, top.soFar + cost(pick), pick :: top.chosen).foreach(stack += _)

  val all = (essential.toVector ++ bestCover.getOrElse(Vector.empty)).sorted
  Cover(all.map(primes(_)._1), math.max(count, 1L))
