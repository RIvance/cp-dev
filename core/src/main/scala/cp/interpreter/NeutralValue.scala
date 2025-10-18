package cp.interpreter

import cp.util.IdentifiedByString

enum NeutralValue extends IdentifiedByString {
  case Var(name: String)
  case Apply(func: NeutralValue, arg: Value)
  case Project(record: NeutralValue, field: String)
  case Merge(left: NeutralValue, right: Value)

  override def contains(name: String): Boolean = this match {
    case NeutralValue.Var(n) => n == name
    case NeutralValue.Apply(func, arg) => func.contains(name) || arg.contains(name)
    case NeutralValue.Project(record, field) => record.contains(name)
    case NeutralValue.Merge(left, right) => left.contains(name) || right.contains(name)
  }
}
