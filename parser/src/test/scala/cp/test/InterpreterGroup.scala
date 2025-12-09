package cp.test

import cp.common.Environment
import cp.core.{Module, Term, Type, Value}
import cp.interpreter.Interpreter

class InterpreterGroup(val interpreters: Map[String, Interpreter]) {
  
  def check(term: Term, expectedType: Type): Boolean = {
    interpreters.values.forall(_.check(term, expectedType))
  }

  def loadModule(module: Module): Unit = {
    interpreters.values.foreach(_.loadModule(module))
  }

  def evalAll(term: Term): Map[String, Term] = {
    interpreters.map { (name, interpreter) =>
      name -> interpreter.eval(term)(using Environment.empty).toTerm
    }
  }

  def foreachInterpreter(f: (String, Interpreter) => Unit): Unit = {
    interpreters.foreach { case (name, interpreter) => f(name, interpreter) }
  }
}

object InterpreterGroup {
  def apply(interpreters: (String, Interpreter)*): InterpreterGroup = {
    new InterpreterGroup(interpreters.toMap)
  }
}
