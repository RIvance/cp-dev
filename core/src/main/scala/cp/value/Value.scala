package cp.value

import cp.core.Type

enum Value {
  case Neutral(neutral: NeutralValue)
  // HOAS
  case Lambda(param: Option[String], paramType: Type, body: Value => Value)
  case Record(fields: Map[String, Value])
}
