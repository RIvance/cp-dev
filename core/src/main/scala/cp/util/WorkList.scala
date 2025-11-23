package cp.util

trait Workable[Task[_]] {

  /**
   * Returns Some(result) if the task is in a completed/final state.
   * Returns None if it must take a step.
   */
  def completed[A](task: Task[A]): Option[A]

  /**
   * Executes one computation step for the task,
   * producing the next WorkList node to evaluate.
   */
  def step[A](task: Task[A]): WorkList[Task, A]
}

sealed trait WorkList[Task[_], +A] { self =>

  final def flatMap[B](f: A => WorkList[Task, B]): WorkList[Task, B] = WorkList.Sequence(this, f)

  final def map[B](f: A => B): WorkList[Task, B] = flatMap(a => WorkList.Completed(f(a)))

  final def withFilter(predicate: A => Boolean): WorkList[Task, A] = flatMap { a =>
    if (predicate(a)) WorkList.Completed(a)
    else WorkList.Failed(Some(RuntimeException("withFilter predicate failed")))
  }

  /**
   * Deterministic evaluator for the computation structure.
   */
  final def run(using work: Workable[Task]): A = {
    import WorkList._
    import work._

    @scala.annotation.tailrec
    def loop(current: WorkList[Task, A]): A = current match {

      case Completed(value) => value

      case TaskNode(task) => completed(task) match {
        case Some(result) => result
        case None         => loop(step(task))
      }

      case Sequence(inner, cont) => inner match {
        case Completed(value) => loop(cont(value))
        case TaskNode(task) => completed(task) match {
          case Some(result) => loop(cont(result))
          case None         => loop(step(task).flatMap(cont))
        }
        // Reassociate nested binds for tail-recursion
        case Sequence(first, next) => loop(first.flatMap(a => next(a).flatMap(cont)))
        case failure @ Failed(_)   => failure.throwError()
      }

      case failure @ Failed(_) => failure.throwError()
    }

    loop(this)
  }
}

object WorkList {

  final case class Completed[Task[_], A](
    value: A
  ) extends WorkList[Task, A]

  final case class Sequence[Task[_], A, B](
    step: WorkList[Task, A],
    cont: A => WorkList[Task, B]
  ) extends WorkList[Task, B]

  final case class TaskNode[Task[_], A](
    task: Task[A]
  ) extends WorkList[Task, A]

  final case class Failed[Task[_], A](
    err: Option[Throwable] = None
  ) extends WorkList[Task, A] {
    def throwError(): Nothing = err match {
      case Some(e) => throw e
      case None    => throw new RuntimeException("WorkList failed")
    }
  }

  infix def >>=[Task[_], A, B](
    workList: WorkList[Task, A],
    cont: A => WorkList[Task, B],
  ): WorkList[Task, B] = workList.flatMap(cont)
}
