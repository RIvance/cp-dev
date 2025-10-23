package cp.core

import cp.common.TypeEnvironment
import cp.error.CoreErrorKind
import cp.error.CoreErrorKind.TypeNotMatch

enum Pattern[T] {
  case Bind(name: String)
  case Primitive(value: PrimitiveValue)
  case Tuple(elements: List[Pattern[T]])
  case Record(fields: Map[String, Pattern[T]])
  case Annotated(pattern: Pattern[T], annotatedType: T)

  def map[U](f: T => U): Pattern[U] = this match {
    case Pattern.Bind(name) => Pattern.Bind(name)
    case Pattern.Primitive(value) => Pattern.Primitive(value)
    case Pattern.Tuple(elements) => Pattern.Tuple(elements.map(_.map(f)))
    case Pattern.Record(fields) => Pattern.Record(fields.map {
      (name, pattern) => (name, pattern.map(f))
    })
    case Pattern.Annotated(pattern, annotatedType) => {
      Pattern.Annotated(pattern.map(f), f(annotatedType))
    }
  }
}

extension (pattern: Pattern[Type]) {

  def matchBindingTypes(ty: Type)(using env: TypeEnvironment[String, Type]): Map[String, Type] = pattern match {
    case Pattern.Primitive(_) => Map.empty
    case Pattern.Bind(binding) => Map(binding -> ty)
    case Pattern.Tuple(elements) => ty match {
      case Type.Tuple(elementTypes) => elements.zip(elementTypes).flatMap {
        (pattern, ty) => pattern.matchBindingTypes(ty)
      }.toMap
      case _ => TypeNotMatch.raise {
        s"Type mismatch: expected Tuple type for pattern $pattern, got $ty"
      }
    }
    case Pattern.Record(fields) => ty match {
      case Type.Record(fieldTypes) => fields.flatMap {
        (name, pattern) => fieldTypes.get(name) match {
          case Some(ty) => pattern.matchBindingTypes(ty)
          case None => TypeNotMatch.raise {
            s"Type mismatch: field $name not found in type $ty for pattern $pattern"
          }
        }
      }
      case _ => TypeNotMatch.raise {
        s"Type mismatch: expected Record type for pattern $pattern, got $ty"
      }
    }
    case Pattern.Annotated(innerPattern, annotatedType) => {
      if (ty <:< annotatedType) {
        innerPattern.matchBindingTypes(ty)
      } else TypeNotMatch.raise {
        s"Type mismatch: expected type $annotatedType for pattern $pattern, got $ty"
      }
    }
  }

}

case class Clause[T, V](patterns: List[Pattern[T]], body: V)
