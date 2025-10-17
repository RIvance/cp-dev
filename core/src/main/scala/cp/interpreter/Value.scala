package cp.interpreter

import cp.core.{Environment, Literal, Type}
import cp.util.RecNamed

type ValueEnv = Environment[Type, Value]

enum Value extends RecNamed {

  case Neutral(neutral: NeutralValue)
  // HOAS
  case Primitive(value: Literal)
  case Lambda(param: Option[String], paramType: Type, body: Value)
  case Record(fields: Map[String, Value])
  case FixThunk(annotatedType: Type, body: Value, env: ValueEnv = Environment.empty[Type, Value])

  override def contains(name: String): Boolean = ???
}
