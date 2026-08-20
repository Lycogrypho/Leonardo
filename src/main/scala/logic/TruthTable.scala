package it.grypho.scala.leonardo
package logic

import core.*


/** Maximum number of variables [[truthTable]] enumerates (2^16 = 65536 rows).
 *  Beyond this the table is rejected (empty result) rather than attempted.
 */
val MaxTruthTableVars: Int = 16


/** Enumerates every boolean assignment of `vars` and evaluates `e` under each.
 *
 *  Rows are ordered with the first variable as the most significant bit, i.e. from
 *  all-`false` to all-`true`.  The result column is `Some(b)` when the expression fully
 *  reduces to `_Bool(b)` under that assignment and `None` otherwise (free variables
 *  outside `vars`, non-boolean operands, ...).
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
