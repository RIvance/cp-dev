package cp.intermediate

import cp.core.LiteralType
import cp.util.IdentifiedByIndex

enum TypeValue extends IdentifiedByIndex {
  case Primitive(ty: LiteralType)
  case Record(fields: Map[String, TypeValue])
  case Fixpoint(name: String, body: TypeValue)
  case Function(paramType: TypeValue, returnType: TypeValue)
}

type TypeEnv = Map[Int, TypeValue]

object TypeValue {
  def int: TypeValue = TypeValue.Primitive(LiteralType.IntType)
  def float: TypeValue = TypeValue.Primitive(LiteralType.FloatType)
  def bool: TypeValue = TypeValue.Primitive(LiteralType.BoolType)
}
