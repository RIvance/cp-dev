package cp.core

import cp.common.{Environment, TypeEnvironment}
import cp.util.IdentifiedByString

type ValueEnv = Environment[String, Type, Value]

enum Value extends IdentifiedByString {

  case Neutral(neutral: NeutralValue)
  case Primitive(value: PrimitiveValue)
  case Closure(env: ValueEnv, param: String, paramType: Type, body: Term, returnType: Type, isCoe: Boolean)
  case TypeClosure(env: ValueEnv, typeParam: String, body: Term, returnType: Type)
  case Record(fields: Map[String, Value])
  case Tuple(elements: List[Value])
  case Array(elements: List[Value])
  case FixThunk(env: ValueEnv, annotatedType: Type, name: String, body: Term)

  override def contains(name: String): Boolean = this match {
    case Neutral(neutral) => neutral.contains(name)
    case Primitive(_) => false
    case Closure(_, param, _, body, _, _) => param != name && body.contains(name)
    case TypeClosure(_, _, body, _) => body.contains(name)
    case Record(fields) => fields.values.exists(_.contains(name))
    case Tuple(elements) => elements.exists(_.contains(name))
    case Array(elements) => elements.exists(_.contains(name))
    case FixThunk(_, _, fixName, body) => fixName != name && body.contains(name)
  }

  def merge(other: Value)(using env: ValueEnv): Value = {
    val leftType = this.infer(using env)
    val rightType = other.infer(using env)

    // Check if types are disjoint
    if !leftType.disjointWith(rightType) then {
      throw new RuntimeException(s"Cannot merge values with overlapping types: $leftType and $rightType")
    }

    (this, other) match {
      case (Record(leftFields), Record(rightFields)) => {
        // Merging two records, recursively merge common fields
        val commonFields = leftFields.keySet.intersect(rightFields.keySet).map { fieldName =>
          (fieldName, leftFields(fieldName).merge(rightFields(fieldName)))
        }.toMap
        val leftOnlyFields = leftFields.filterNot { (name, _) => commonFields.contains(name) }
        val rightOnlyFields = rightFields.filterNot { (name, _) => commonFields.contains(name) }
        Record(leftOnlyFields ++ rightOnlyFields ++ commonFields)
      }

      case (Tuple(leftElements), Tuple(rightElements)) if leftElements.length == rightElements.length => {
        val mergedElements = leftElements.zip(rightElements).map { case (l, r) => l.merge(r) }
        Tuple(mergedElements)
      }

      case _ => Value.Neutral(NeutralValue.Merge(this, other))
    }
  }

  // Compute the difference of two values (left \ right)
  def diff(other: Value)(using env: ValueEnv): Value = (this, other) match {
    case (Record(leftFields), Record(rightFields)) => {
      // Differencing two records, recursively difference common fields
      // For a pair of common fields `f: A` and `f: B`,
      // we remove `f` from the left record if `B <: A`
      val commonFields = leftFields.keySet.intersect(rightFields.keySet)
      val reservedCommonFields = commonFields.flatMap { fieldName =>
        val leftFieldValue = leftFields(fieldName)
        val rightFieldValue = rightFields(fieldName)
        val leftFieldType = leftFieldValue.infer
        val rightFieldType = rightFieldValue.infer

        // Check if rightFieldType <:< leftFieldType
        if rightFieldType <:< leftFieldType then {
          None // Remove this field
        } else {
          Some((fieldName, leftFieldValue.diff(rightFieldValue)))
        }
      }.toMap

      val leftOnlyFields = leftFields.filterNot { (name, _) => commonFields.contains(name) }
      Record(leftOnlyFields ++ reservedCommonFields)
    }

    case _ => this // For non-record values, return unchanged
  }

  def cast(targetType: Type)(using env: ValueEnv): Option[Value] = {
    (this, targetType.normalize) match {
      case (_, Type.Primitive(PrimitiveType.TopType)) =>
        // Casting to top-like type, generate appropriate top-like value
        Some(generateTopLikeValue(targetType))

      // Primitive types
      case (Primitive(primitiveValue), Type.Primitive(primitiveType))
        if primitiveValue.ty == primitiveType => Some(this)

      // Fixpoint types
      case (FixThunk(thunkEnv, annotatedType, _, _), _) =>
        given thunkTypeEnv: TypeEnvironment[String, Type] = thunkEnv.typeEnv
        val normalizedAnnotatedType = annotatedType.normalize
        if normalizedAnnotatedType <:< targetType then Some(this) else None

      // Record types
      case (Record(fields), Type.Record(targetFields)) =>
        // Cast record to target record type, recursively cast each field
        targetFields.foldLeft(Option(Map.empty[String, Value])) {
          case (accOpt, (fieldName, fieldType)) => accOpt.flatMap { acc =>
            fields.get(fieldName).flatMap(_.cast(fieldType)).map(casted => acc + (fieldName -> casted))
          }
        }.map(Record.apply)

      // Arrow types (function closures with coercion support)
      case (Closure(closureEnv, param, paramType, body, returnType, _), Type.Arrow(targetDomain, targetCodomain, isTrait)) =>
        given closureTypeEnv: TypeEnvironment[String, Type] = closureEnv.typeEnv
        val normalizedParamType = paramType.normalize
        val normalizedReturnType = returnType.normalize
        val normalizedTargetCodomain = targetCodomain.normalize(using env.typeEnv)

        // Check if returnType <: targetCodomain (covariant in return type)
        if normalizedReturnType <:< normalizedTargetCodomain then {
          // Create a closure with updated return type
          Some(Closure(closureEnv, param, normalizedParamType, body, normalizedTargetCodomain, isTrait))
        } else None

      // Type abstractions (forall types)
      case (TypeClosure(closureEnv, typeParam, body, returnType), Type.Forall(targetParam, targetCodomain, targetConstraints)) =>
        given closureTypeEnv: TypeEnvironment[String, Type] = closureEnv.typeEnv
        val normalizedReturnType = returnType.normalize
        val normalizedTargetCodomain = targetCodomain.normalize(using env.typeEnv)

        // Check if the return type is a subtype of target codomain
        if normalizedReturnType <:< normalizedTargetCodomain then {
          Some(TypeClosure(closureEnv, targetParam, body, normalizedTargetCodomain))
        } else None

      // Array types
      case (Array(elements), Type.Array(targetElementType)) =>
        if elements.isEmpty then {
          Some(Array(Nil))
        } else {
          val inferredElementType = elements.head.infer
          val normalizedInferred = inferredElementType.normalize(using env.typeEnv)
          val normalizedTarget = targetElementType.normalize(using env.typeEnv)

          if normalizedInferred <:< normalizedTarget then {
            Some(Array(elements))
          } else None
        }

      // Tuple types
      case (Tuple(elements), Type.Tuple(targetTypes)) if elements.length == targetTypes.length =>
        // Cast tuple elements to target types
        elements.zip(targetTypes).foldLeft(Option(List.empty[Value])) { case (accOpt, (elem, elemTargetType)) =>
          accOpt.flatMap { acc =>
            elem.cast(elemTargetType).map(castedValue => acc :+ castedValue)
          }
        }.map(Tuple.apply)

      // For neutral values, we cannot determine compatibility here, just return a neutral cast
      case (Neutral(nv), _) if this.isNeutral => Some(Value.Neutral(NeutralValue.Annotated(nv, targetType)))

      // Default case - incompatible cast
      case _ => {
        val splitTypes = targetType.split
        if splitTypes.size > 1 then {
          val castedParts = splitTypes.map { partType => this.cast(partType) }
          if castedParts.forall(_.isDefined) then {
            Some(castedParts.flatten.reduce { (left, right) => left.merge(right) })
          } else None
        } else this match {
          // Handle merge values - try casting either branch
          case Neutral(NeutralValue.Merge(leftValue, rightValue)) =>
            leftValue.cast(targetType).orElse(rightValue.cast(targetType))
          case _ => None
        }
      }
    }
  }

  private def generateTopLikeValue(targetType: Type): Value = targetType match {
    case Type.Primitive(PrimitiveType.TopType) => Primitive(PrimitiveValue.unit)
    case Type.Record(fields) => Record(fields.view.mapValues(generateTopLikeValue).toMap)
    case Type.Tuple(elements) => Tuple(elements.map(generateTopLikeValue))
    case Type.Arrow(_, codomain, _) => Value.Closure(
      env = Environment.empty,
      param = "_",
      paramType = Type.Primitive(PrimitiveType.TopType),
      body = Term.Primitive(PrimitiveValue.unit), // Dummy body
      returnType = codomain,
      isCoe = true
    )
    case _ => Primitive(PrimitiveValue.unit)
  }

  // Convert value back to term representation
  def toTerm: Term = this match {
    case Primitive(primitiveValue) => Term.Primitive(primitiveValue)
    case Closure(_, param, paramType, body, _, isCoe) => Term.Lambda(param, paramType, body, isCoe)
    case TypeClosure(_, typeParam, body, _) => Term.TypeLambda(typeParam, body)
    case Record(fields) => Term.Record(fields.view.mapValues(_.toTerm).toMap)
    case Tuple(elements) => Term.Tuple(elements.map(_.toTerm))
    case Array(elements) => Term.ArrayLiteral(elements.map(_.toTerm))
    case FixThunk(env, annotatedType, fixName, body) => {
      Term.Fixpoint(fixName, annotatedType, body)
    }
    case Neutral(neutralValue) => neutralValue.toTerm
  }

  def infer(using env: ValueEnv): Type = this match {
    case Primitive(primitiveValue) => Type.Primitive(primitiveValue.ty)
    case Closure(_, param, paramType, _, returnType, _) => Type.Arrow(paramType, returnType)
    case TypeClosure(_, typeParam, _, returnType) => Type.Forall(typeParam, returnType, Set.empty)
    case Record(fields) => Type.Record(fields.view.mapValues(_.infer).toMap)
    case Tuple(elements) => Type.Tuple(elements.map(_.infer))
    case Array(elements) if elements.nonEmpty => Type.Array(elements.head.infer)
    case Array(_) => Type.Array(Type.Primitive(PrimitiveType.TopType))
    case FixThunk(_, annotatedType, _, _) => annotatedType
    case Neutral(neutralValue) => neutralValue.infer
  }

  def isNeutral: Boolean = this match {
    case Neutral(NeutralValue.Merge(left, right)) => left.isNeutral || right.isNeutral
    case Neutral(_) => true
    case _ => false
  }

  override def toString: String = this match {
    case Primitive(primitiveValue) => primitiveValue.toString
    case Closure(_, param, paramType, body, returnType, isCoe) =>
      s"(λ${if isCoe then "coe " else ""}$param: $paramType. $body : $returnType)"
    case TypeClosure(_, typeParam, body, returnType) =>
      s"(Λ$typeParam. $body : $returnType)"
    case Record(fields) =>
      val fieldStrings = fields.map { case (name, value) => s"$name = $value" }.mkString(", ")
      s"{ $fieldStrings }"
    case Tuple(elements) =>
      val elementStrings = elements.map(_.toString).mkString(", ")
      s"($elementStrings)"
    case Array(elements) =>
      val elementStrings = elements.map(_.toString).mkString(", ")
      s"[ $elementStrings ]"
    case FixThunk(_, annotatedType, fixName, body) =>
      s"(fix $fixName: $annotatedType. $body)"
    case Neutral(neutralValue) => neutralValue.toString
  }
}
