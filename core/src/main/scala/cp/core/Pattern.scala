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

  override def toString: String = this match {
    case Pattern.Bind(name) => name
    case Pattern.Primitive(value) => value.toString
    case Pattern.Tuple(elements) => s"(${elements.map(_.toString).mkString(", ")})"
    case Pattern.Record(fields) => s"{${fields.map { case (k, v) => s"$k = $v" }.mkString(", ")}}"
    case Pattern.Annotated(pattern, ty) => s"($pattern : $ty)"
  }

  // Collect all variable names bound by this pattern
  def collectNames: Set[String] = this match {
    case Pattern.Bind(name) => Set(name)
    case Pattern.Primitive(_) => Set.empty
    case Pattern.Tuple(elements) => elements.flatMap(_.collectNames).toSet
    case Pattern.Record(fields) => fields.values.flatMap(_.collectNames).toSet
    case Pattern.Annotated(pattern, _) => pattern.collectNames
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

  // Match a value against a pattern, returning bindings if successful
  def matchValue(value: Value)(using env: TypeEnvironment[String, Type]): Option[Map[String, Value]] = pattern match {
    case Pattern.Bind(name) => Some(Map(name -> value))

    case Pattern.Primitive(expectedValue) => value match {
      case Value.Primitive(actualValue) if actualValue == expectedValue => Some(Map.empty)
      case _ => None
    }

    case Pattern.Tuple(elementPatterns) => value match {
      case Value.Tuple(elementValues) if elementPatterns.length == elementValues.length =>
        val bindings = elementPatterns.zip(elementValues).map { case (pat, v) =>
          pat.matchValue(v)
        }
        if bindings.forall(_.isDefined) then
          Some(bindings.flatten.flatten.toMap)
        else
          None
      case _ => None
    }

    case Pattern.Record(fieldPatterns) => value match {
      case Value.Record(fieldValues) =>
        // Check that all pattern fields exist in the value
        val bindings = fieldPatterns.map { case (fieldName, fieldPattern) =>
          fieldValues.get(fieldName).flatMap(fieldPattern.matchValue)
        }
        if bindings.forall(_.isDefined) then
          Some(bindings.flatten.flatten.toMap)
        else
          None
      case _ => None
    }

    case Pattern.Annotated(innerPattern, annotatedType) =>
      // Check type compatibility and match inner pattern
      val valueType = value.infer(using env.asInstanceOf[cp.common.Environment[String, Type, Value]])
      if valueType <:< annotatedType then
        innerPattern.matchValue(value)
      else
        None
  }

}

case class Clause[T, V](patterns: List[Pattern[T]], body: V)
