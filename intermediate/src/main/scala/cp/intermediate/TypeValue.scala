package cp.intermediate

import cp.core.LiteralType

enum TypeValue {
  case Primitive(ty: LiteralType)
  case Record(fields: Map[String, TypeValue])
  case Function(paramType: TypeValue, returnType: TypeValue)
}

type TypeEnv = Map[Int, TypeValue]
