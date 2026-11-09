package it.grypho.scala.leonardo
package logic

import core.*


/** What [[_Equivalent]] found (F_0322, LT L_0079). */
enum EquivalenceVerdict:
  /** The two formulas agree on every row of the table. */
  case Same
  /** The first row, in [[truthTable]]'s order, on which they differ: the assignment, and the value of
   *  the left and of the right formula there. */
  case Differ(assignment: Map[String, Boolean], left: Boolean, right: Boolean)

/** Why [[_Equivalent]] declined (F_0322): it stays unevaluated and the REPL appends the reason. */
enum EquivalenceFailure:
  /** More free names than the table's cap, `Limit.MaxTruthTableVars` (at most 30). */
  case TooManyNames(names: Int, limit: Int)
  /** A row where a side is not `true` or `false` -- a graded value, or a relation or arithmetic node
   *  that no assignment of truth values to its names can reduce -- and no row that differs. */
  case NotBoolean


/** `equivalent(f, g)` -- whether two formulas agree on every assignment of their free names as
 *  booleans (F_0322, LT L_0079).
 *
 *  **Logical equivalence is not `==`.**  `==` compares values and stays what it was (`f == g` between
 *  two formulas echoes itself, and a connective is not even an operand of `==`, L_0066); equivalence
 *  asks about every row of the table, which is what the BL lessons built by hand as
 *  `truth f xor g` reading `false` throughout.
 *
 *  Both sides are evaluated first, so a name bound in the session is read as its value and is not
 *  enumerated; the names left over -- the free names of the two reduced sides together -- are
 *  enumerated as `truth` does, the first name the most significant.  `true` when every row agrees,
 *  `false` as soon as one differs (the first such row is in [[detailed]], for the REPL's note), and
 *  **unevaluated** past the table's cap or when a row is no boolean: crisp only, the `cnf` / `dnf`
 *  convention (F_0133), since a graded value has no row to be equal on.
 *
 *  @param a the left formula
 *  @param b the right formula
 */
case class _Equivalent(a: _Expression, b: _Expression) extends _Expression:
  override def toString: String = s"equivalent($a, $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = _Equivalent(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env) match
      case Right(EquivalenceVerdict.Same)      => Right(_Bool(true))
      case Right(_: EquivalenceVerdict.Differ) => Right(_Bool(false))
      case Left(_)                             => Left(_Equivalent(a.eval(env).toExpression, b.eval(env).toExpression))

  private val answers = LastAnswer[Either[EquivalenceFailure, EquivalenceVerdict]]()

  /** The verdict, or why there is none; what the REPL queries -- computed once per environment, so
   *  its note reads the table `eval` built (F_0212).
   *
   *  @param env bindings, and the table's cap
   *  @return the verdict, or the reason the node stays unevaluated
   */
  def detailed(env: Environment): Either[EquivalenceFailure, EquivalenceVerdict] = answers(env) {
    val (ea, eb) = (a.eval(env).toExpression, b.eval(env).toExpression)
    val names    = (ea.freeVars ++ eb.freeVars).toList.sorted.map(_Variable.apply)
    val cap      = math.min(env.limits(Limit.MaxTruthTableVars), 30L)
    if names.size > cap then Left(EquivalenceFailure.TooManyNames(names.size, cap.toInt))
    else
      val rows = truthTable(ea, names, env).zip(truthTable(eb, names, env))
      rows.collectFirst { case ((assignment, Some(x)), (_, Some(y))) if x != y =>
        EquivalenceVerdict.Differ(assignment, x, y) } match
        case Some(differ) => Right(differ)
        case None =>
          if rows.forall { case ((_, x), (_, y)) => x.isDefined && y.isDefined } then Right(EquivalenceVerdict.Same)
          else Left(EquivalenceFailure.NotBoolean)
  }
