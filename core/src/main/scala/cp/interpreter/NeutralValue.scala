package cp.interpreter

import cp.core.Literal

enum NeutralValue {
  case Var(name: String)
  case Apply(func: NeutralValue, arg: Value)
  case Project(record: NeutralValue, field: String)
  case Merge(left: NeutralValue, right: Value)
}
