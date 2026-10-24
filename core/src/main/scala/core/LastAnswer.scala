package it.grypho.scala.leonardo
package core

/** The last answer a node computed, and the environment it computed it in (F_0212).
 *
 *  The REPL evaluates a heavy node (`lp`, `qp`, `nlp`, `lsqfit`, `minimize`) and then asks the
 *  **same instance** for its `detailed` answer to write the note — which solved the problem a
 *  second time, doubling the browser's time on its heaviest commands (`qp` at 60 variables: 2.5 s
 *  on Node).  Holding the last answer lets the second call read the first.
 *
 *  **Keyed on the environment's identity**, never its contents: an `Environment` is immutable, so
 *  the same instance always gives the same answer, while comparing two by value would compare
 *  every binding.  A different environment simply computes again.  The node is the parse of one
 *  line, so the memo lives no longer than the expression holding it.  **One immutable pair behind
 *  one field**: a race between two threads can only compute twice, never return an answer for
 *  another environment.
 *
 *  @tparam A the answer
 */
private[leonardo] final class LastAnswer[A]:
  @volatile private var last: Option[(Environment, A)] = None

  /** The answer for `env`: the remembered one when `env` is the environment it was computed in,
   *  else `compute`, remembered.
   *  @param env     the environment
   *  @param compute the computation, pure in `env`
   *  @return its answer */
  def apply(env: Environment)(compute: => A): A =
    last match
      case Some((e, a)) if e eq env => a
      case _ =>
        val a = compute
        last = Some((env, a))
        a
