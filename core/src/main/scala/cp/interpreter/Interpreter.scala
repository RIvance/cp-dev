package cp.interpreter

import cp.core.{Environment, Term, Type}

import scala.util.control.TailCalls.TailRec

object Interpreter {

  private type Env = Environment[String, Type, Value]

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value = term.evalTramp.result

  extension (term: Term) {
    private def evalTramp(using env: Env): TailRec[Value] = ???
  }
}
