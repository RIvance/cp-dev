package cp.interpreter

import cp.common.Environment
import cp.core.{Module, Namespace, Term, Type, Value}

abstract class Interpreter(initialModules: Module*) {

  protected type Env = Environment[String, Type, Value]

  protected var modules: Map[Namespace, Module] = initialModules.map { mod => mod.namespace -> mod }.toMap

  def check(term: Term, expectedType: Type): Boolean = term.check(expectedType)

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value

  def loadModule(module: Module): Unit = {
    modules += (module.namespace -> module)
  }

  implicit def globalEnvironment: Environment[String, Type, Term] = {
    modules.values.foldLeft(Environment.empty[String, Type, Term]) { (envAcc, module) =>
      envAcc.merge(module.importEnvironment)
    }
  }
}
