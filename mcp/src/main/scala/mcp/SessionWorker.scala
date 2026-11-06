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
 *  @param timeout the wall-clock limit of one request
 *  @param init    the commands run on every fresh session, after its state is restored
 */
final class SessionWorker(timeout: FiniteDuration, init: Seq[String] = SessionWorker.DefaultInit):
  private var executor: ExecutorService = SessionWorker.newExecutor()
  private var session: Session          = SessionWorker.fresh(None, init)

  /** Runs `f` on the session within the timeout.
   *  @param f      the work, given the session
   *  @param within the limit of this call, the worker's own by default
   *  @tparam A its result
   *  @return the result, or the message saying why there is none */
  def run[A](f: Session => A, within: FiniteDuration = timeout): Either[String, A] = synchronized {
    val current = session
    val saved   = current.script
    val future  = executor.submit(new Callable[A] { def call(): A = f(current) })
    def restore(): Unit =
      future.cancel(true)
      executor.shutdownNow()
      executor = SessionWorker.newExecutor()
      session  = SessionWorker.fresh(Some(saved), init)
    try Right(future.get(within.toMillis, TimeUnit.MILLISECONDS))
    catch
      case _: TimeoutException =>
        restore()
        Left(s"timed out after ${within.toMillis} ms; the request was abandoned " +
             "and the session restored to its state before it")
      case e: ExecutionException =>
        restore()
        Left(s"the request failed (${Option(e.getCause).fold("unknown")(_.getClass.getSimpleName)}); " +
             "the session was restored to its state before it")
  }

object SessionWorker:
  /** What every server session starts with: unknown calls reported, LaTeX offered. */
  val DefaultInit: Seq[String] = Seq("names on", "latex on")

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
