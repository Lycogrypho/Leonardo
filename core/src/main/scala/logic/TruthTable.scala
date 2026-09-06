package it.grypho.scala.leonardo
package logic

import core.*


/** Maximum number of variables [[truthTable]] enumerates (2^16 = 65536 rows).
 *  Beyond this the table is rejected (empty result) rather than attempted.
 */
val MaxTruthTableVars: Int = 16


/** Maximum number of variables [[kleeneTable]] enumerates (3^10 = 59049 rows) —
 *  the three-valued counterpart of [[MaxTruthTableVars]], set to the same row budget.
 */
val MaxKleeneTableVars: Int = 10


/** The three Kleene truth values in ascending order: `false`, `unknown`, `true`.
 *  The enumeration alphabet of [[kleeneTable]].
 */
val KleeneValues: Vector[_Value] = Vector(_Bool(false), _Truth.Unknown, _Bool(true))


/** Enumerates every boolean assignment of `vars` and evaluates `e` under each.
 *
 *  Rows are ordered with the first variable as the most significant bit, i.e. from
 *  all-`false` to all-`true`.  The result column is `Some(b)` when the expression fully
 *  reduces to `_Bool(b)` under that assignment and `None` otherwise (free variables
 *  outside `vars`, non-boolean operands, or a row that reduces to a graded `_Truth`
 *  degree — use [[kleeneTable]] for those).
 *
 *  Crisp-only (the `Asin` convention): assignments range over `_Bool` values only.
 *
 *  @param e    the expression to tabulate
 *  @param vars the variables to enumerate (each bound to `_Bool` per row); an empty list
 *              yields a single row with the empty assignment
 *  @param env  base environment for each row's evaluation (row bindings shadow it)
 *  @return one `(assignment, result)` pair per row, or an empty vector when `vars`
 *          exceeds [[MaxTruthTableVars]]
 */
def truthTable(e: _Expression, vars: List[_Variable], env: Environment = new Environment())
    : Vector[(Map[String, Boolean], Option[Boolean])] =
  if vars.size > MaxTruthTableVars then Vector.empty
  else
    val names = vars.map(_.variable)
    Vector.tabulate(1 << names.size) { i =>
      val assignment = names.zipWithIndex
        .map((n, j) => n -> (((i >> (names.size - 1 - j)) & 1) == 1)).toMap
      val bound = assignment.foldLeft(env)((acc, kv) => acc.withBinding(kv._1, _Bool(kv._2)))
      val result = e.eval(bound) match
        case Right(_Bool(b)) => Some(b)
        case _               => None
      (assignment, result)
    }


/** Enumerates every three-valued (Kleene) assignment of `vars` and evaluates `e` under each.
 *
 *  The three-valued counterpart of [[truthTable]]: each variable ranges over
 *  [[KleeneValues]] (`false`, `unknown`, `true`) for `3^n` rows, ordered with the first
 *  variable as the most significant digit.  The result column is `Some(v)` when the row
 *  reduces to a truth-valued result (`_Bool` or `_Truth`) and `None` otherwise.
 *
 *  @param e    the expression to tabulate
 *  @param vars the variables to enumerate; an empty list yields a single row with the
 *              empty assignment
 *  @param env  base environment for each row's evaluation (row bindings shadow it)
 *  @return one `(assignment, result)` pair per row, or an empty vector when `vars`
 *          exceeds [[MaxKleeneTableVars]]
 */
def kleeneTable(e: _Expression, vars: List[_Variable], env: Environment = new Environment())
    : Vector[(Map[String, _Value], Option[_Value])] =
  if vars.size > MaxKleeneTableVars then Vector.empty
  else
    val names  = vars.map(_.variable)
    val radix  = KleeneValues.size
    val rows   = math.pow(radix, names.size).toInt
    // digit j of row i, first variable most significant
    val weight = (j: Int) => math.pow(radix, names.size - 1 - j).toInt
    Vector.tabulate(rows) { i =>
      val assignment = names.zipWithIndex
        .map((n, j) => n -> KleeneValues((i / weight(j)) % radix)).toMap
      val bound = assignment.foldLeft(env)((acc, kv) => acc.withBinding(kv._1, kv._2))
      val result = e.eval(bound) match
        case Right(v) if asTruth(v, env.symmetricLogic).isDefined => Some(v)
        case _                                                    => None
      (assignment, result)
    }
