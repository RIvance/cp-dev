package cp.intermediate.calculus

import cp.core.Literal
import cp.intermediate.{NativePrototype, TypeEnv, TypeValue as Type}
import cp.util.IdentifiedByIndex

enum RcTerm extends IdentifiedByIndex {
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

  // De Bruijn indices
  override def contains(index: Int): Boolean = this match {
    case RcTerm.Var(i) => i == index
    case RcTerm.Primitive(_) => false
    case RcTerm.If(cond, thenBr, elseBr) =>
      cond.contains(index) || thenBr.contains(index) || elseBr.contains(index)
    case RcTerm.Lambda(_, body) =>
      body.contains(index + 1)
    case RcTerm.Apply(func, arg) =>
      func.contains(index) || arg.contains(index)
    case RcTerm.Fixpoint(_, body) =>
      body.contains(index + 1)
    case RcTerm.Record(fields) =>
      fields.values.exists(_.contains(index))
    case RcTerm.Project(record, _) =>
      record.contains(index)
    case RcTerm.Merge(left, right) =>
      left.contains(index) || right.contains(index)
    case RcTerm.NativeCall(_, args) =>
      args.exists(_.contains(index))
  }
}
