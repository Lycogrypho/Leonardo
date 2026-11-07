package it.grypho.scala.leonardo
package mcp

import cli.Session

import java.util.concurrent.{Callable, ExecutionException, ExecutorService, Executors, ThreadFactory, TimeUnit, TimeoutException}
import scala.concurrent.duration.FiniteDuration


/** One REPL session behind a wall-clock timeout (F_0126).
 *
 *  Every request runs on a single worker thread, so the session sees one request at a time.  A
 *  request that passes `timeout` is answered at once with an error; its worker is **abandoned** — a
 *  computation cannot be interrupted, and its work budgets (`Environment.limits`) eventually stop it
 *  — and the session continues on a fresh worker whose `Session` is restored from the `:save` script
 *  taken before the request, so the state the client built survives and the half-run request leaves
 *  no trace.  A throwable escaping the session (the REPL lets `StackOverflowError` and
 *  `OutOfMemoryError` through) is handled the same way.  The worker threads are daemons, so an
 *  abandoned one never keeps the process alive.
 *
 *  **The abandoned workers are bounded** (F_0312, the owner's choice): while `maxAbandoned` of them
 *  are still computing, a new request is refused at once with the reason, and accepted again as soon
 *  as one finishes — a client repeating a slow line cannot stack runaway threads.  **The restore runs
 *  under the worker's timeout too**: if replaying the script does not finish in time, that worker is
 *  abandoned as well and the session starts empty, which the error says.
 *
 *  @param timeout      the wall-clock limit of one request
 *  @param init         the commands run on every fresh session, after its state is restored
 *  @param maxAbandoned how many abandoned computations may still run before requests are refused
 */
final class SessionWorker(timeout: FiniteDuration, init: Seq[String] = SessionWorker.DefaultInit,
                          maxAbandoned: Int = SessionWorker.DefaultMaxAbandoned):
  private var executor: ExecutorService = SessionWorker.newExecutor()
  private var session: Session          = SessionWorker.fresh(None, init)
  private var abandoned: Vector[ExecutorService] = Vector.empty
  /** Run on the worker before a restore replays the script; a seam for the tests, which make the
   *  restore slow deterministically. */
  private[mcp] var beforeRestore: () => Unit = () => ()

  /** How many abandoned computations are still running. */
  private[mcp] def abandonedRunning: Int = synchronized {
    abandoned = abandoned.filterNot(_.isTerminated)
    abandoned.size
  }

  /** Abandons the current worker, keeping it counted until its thread ends. */
  private def abandon(): Unit =
    executor.shutdownNow()
    abandoned = abandoned :+ executor
    executor = SessionWorker.newExecutor()

  /** A fresh session restored from `saved`, within `within`, or an empty one when that times out.
   *  @return the session and whether it was restored */
  private def restored(saved: String, within: FiniteDuration): (Session, Boolean) =
    val future = executor.submit(new Callable[Session] {
      def call(): Session =
        beforeRestore()
        SessionWorker.fresh(Some(saved), init)
    })
    try (future.get(within.toMillis, TimeUnit.MILLISECONDS), true)
    catch
      case _: TimeoutException | _: ExecutionException =>
        future.cancel(true)
        abandon()
        // the default settings the caller asked for, which are cheap; any other init line may be the slow one
        (SessionWorker.fresh(None, SessionWorker.DefaultInit.filter(init.contains)), false)

  /** Runs `f` on the session within the timeout.
   *  @param f      the work, given the session
   *  @param within the limit of this call, the worker's own by default
   *  @tparam A its result
   *  @return the result, or the message saying why there is none */
  def run[A](f: Session => A, within: FiniteDuration = timeout): Either[String, A] = synchronized {
    abandoned = abandoned.filterNot(_.isTerminated)
    if abandoned.size >= maxAbandoned then
      Left(s"${abandoned.size} abandoned computations are still running, from requests that timed out; " +
           "retry shortly, once they finish")
    else
      val current = session
      val saved   = current.script
      val future  = executor.submit(new Callable[A] { def call(): A = f(current) })
      def restore(why: String): Left[String, A] =
        future.cancel(true)
        abandon()
        // the worker's own limit, not this call's: a call may ask for less than a restore needs
        val (fresh, ok) = restored(saved, timeout)
        session = fresh
        Left(why + (if ok then "; the session was restored to its state before it"
                    else "; restoring the session timed out as well, so the session starts empty"))
      try Right(future.get(within.toMillis, TimeUnit.MILLISECONDS))
      catch
        case _: TimeoutException =>
          restore(s"timed out after ${within.toMillis} ms; the request was abandoned")
        case e: ExecutionException =>
          restore(s"the request failed (${Option(e.getCause).fold("unknown")(_.getClass.getSimpleName)})")
  }

object SessionWorker:
  /** What every server session starts with: unknown calls reported, LaTeX offered. */
  val DefaultInit: Seq[String] = Seq("names on", "latex on")

  /** How many abandoned computations may still run before new requests are refused (F_0312). */
  val DefaultMaxAbandoned: Int = 4

  private def fresh(saved: Option[String], init: Seq[String]): Session =
    val s = Session()
    saved.foreach(s.load)
    init.foreach(s.execute)
    s

  private def newExecutor(): ExecutorService =
    Executors.newSingleThreadExecutor(new ThreadFactory:
      def newThread(r: Runnable): Thread =
        val t = Thread(r, "leonardo-mcp-worker")
        t.setDaemon(true)
        t)
