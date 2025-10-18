package cp.intermediate

import cp.core.LiteralType
import cp.util.IdentifiedByIndex

enum TypeValue extends IdentifiedByIndex {
  case Primitive(ty: LiteralType)
  case Record(fields: Map[String, TypeValue])
  case Function(paramType: TypeValue, returnType: TypeValue)
}

type TypeEnv = Map[Int, TypeValue]
