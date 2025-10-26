package cp.intermediate

import cp.core.PrimitiveType
import cp.util.IdentifiedByIndex

enum TypeValue extends IdentifiedByIndex {
  case Primitive(ty: PrimitiveType)
  case Record(fields: Map[String, TypeValue])
  case Fixpoint(name: String, body: TypeValue)
  case Function(paramType: TypeValue, returnType: TypeValue)
}

type TypeEnv = Map[Int, TypeValue]

object TypeValue {
  def int: TypeValue = TypeValue.Primitive(PrimitiveType.IntType)
  def float: TypeValue = TypeValue.Primitive(PrimitiveType.FloatType)
  def bool: TypeValue = TypeValue.Primitive(PrimitiveType.BoolType)
}
