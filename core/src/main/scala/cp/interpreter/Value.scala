package cp.interpreter

import cp.common.Environment
import cp.core.{FullyQualifiedName, PrimitiveValue, Term, Type}
import cp.util.IdentifiedByString

type ValueEnv = Environment[String, Type, Value]

enum Value extends IdentifiedByString {

  case Neutral(neutral: NeutralValue)
  case Primitive(value: PrimitiveValue)
  case Closure(env: ValueEnv, param: String, paramType: Type, body: Term, isCoe: Boolean)
  case Record(fields: Map[String, Value])
  case FixThunk(annotatedType: Type, name: String, body: Term, env: ValueEnv)
  case NativeCall(name: FullyQualifiedName, args: List[Value])

  override def contains(name: String): Boolean = ???
}
