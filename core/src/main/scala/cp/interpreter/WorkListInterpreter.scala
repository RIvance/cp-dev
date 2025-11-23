package cp.interpreter

import cp.core.{Module, Term, Type, Value}
import cp.util.{WorkList, Workable}

class WorkListInterpreter(initialModules: Module*) extends Interpreter(initialModules*) {

  sealed trait BigStepTask[T] {
    def step[A]: WorkList[BigStepTask, A]
  }

  given Workable[BigStepTask] with {
    // We don't have to implement `completed` for big-step evaluation
    //  because the evaluation always finishes in one step.
    // For small-step evaluation, we would need to implement this method.
    def completed[A](task: BigStepTask[A]): Option[A] = None
    def step[A](task: BigStepTask[A]): WorkList[BigStepTask, A] = task.step
  }

  override def eval(term: Term)(using env: Env): Value = WorkList.TaskNode(Eval(term, env)).run

  case class Eval(term: Term, env: Env) extends BigStepTask[Value] {
    override def step[A]: WorkList[BigStepTask, A] = ???
  }

  // TODO: For now, we just use the direct `infer/check` method in `Term` class.
  //  In the future, we can implement these as separate tasks to fit into the WorkList framework.
  //
  // case class Infer(term: Term, env: Env) extends BigStepTask[Type] {
  //   override def step[A]: WorkList[BigStepTask, A] = ???
  // }
  //
  // case class Check(term: Term, expectedType: Type, env: Env) extends BigStepTask[Unit] {
  //   override def step[A]: WorkList[BigStepTask, A] = ???
  // }
}
