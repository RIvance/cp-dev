package cp.interpreter

import cp.common.Environment
import cp.core.{Module, Namespace, Term, Type, Value}

abstract class Interpreter(initialModules: Module*) {

  protected type Env = Environment[String, Type, Value]

  protected var modules: Map[Namespace, Module] = initialModules.map { mod => mod.namespace -> mod }.toMap

  protected case class FixProjection(fixpoint: Value.FixThunk, field: String)

  protected case class UnfoldingSuppressor(fixpoints: Set[FixProjection]) {
    def isSuppressed(fixpoint: FixProjection): Boolean = fixpoints.contains(fixpoint)
    def isSuppressed(fixpoint: (Value.FixThunk, String)): Boolean = {
      fixpoints.contains(FixProjection(fixpoint._1, fixpoint._2))
    }
    def +(fixpoint: FixProjection): UnfoldingSuppressor = UnfoldingSuppressor(fixpoints + fixpoint)
    def +(fixpoint: (Value.FixThunk, String)): UnfoldingSuppressor = {
      UnfoldingSuppressor(fixpoints + FixProjection(fixpoint._1, fixpoint._2))
    }
  }

  protected object UnfoldingSuppressor {
    def apply(): UnfoldingSuppressor = UnfoldingSuppressor(Set.empty)
    def apply(fixpoint: FixProjection): UnfoldingSuppressor = UnfoldingSuppressor(Set(fixpoint))
  }

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
