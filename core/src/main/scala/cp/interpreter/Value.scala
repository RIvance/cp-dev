package cp.interpreter

import cp.core.{Environment, Literal, Type}
import cp.util.IdentifiedByString

type ValueEnv = Environment[String, Type, Value]

enum Value extends IdentifiedByString {

  case Neutral(neutral: NeutralValue)
  // HOAS
  case Primitive(value: Literal)
  case Lambda(param: Option[String], paramType: Type, body: Value)
  case Record(fields: Map[String, Value])
  case FixThunk(annotatedType: Type, body: Value, env: ValueEnv = Environment.empty[String, Type, Value])

  override def contains(name: String): Boolean = ???
}
