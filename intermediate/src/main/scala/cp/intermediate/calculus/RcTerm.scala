package cp.intermediate.calculus

import cp.core.Literal
import cp.intermediate.{NativePrototype, TypeEnv, TypeValue as Type}

enum RcTerm {
  case Var(index: Int)
  case Primitive(value: Literal)
  case If(condition: RcTerm, thenBranch: RcTerm, elseBranch: RcTerm)
  case Lambda(paramType: Type, body: RcTerm)
  case Apply(func: RcTerm, arg: RcTerm)
  case Fixpoint(fixpointType: Type, body: RcTerm)
  case Record(fields: Map[String, RcTerm])
  case Project(record: RcTerm, field: String)
  case Merge(left: RcTerm, right: RcTerm)
  case NativeCall(fn: NativePrototype[Type], args: List[RcTerm])

  def infer(using env: TypeEnv = Map.empty): Type = ???
}
