package cp.syntax

import cp.core.{Constraint, PrimitiveType, Type, TypeEnvironment, synthesize, verify}
import cp.error.{CoreError, SpannedError, UnknownError}
import cp.error.CoreErrorKind.*
import cp.util.{OptionalSpanned, IdentifiedByString, SourceSpan}

private type TypeEnv = TypeEnvironment[String, Type]

enum ExprType extends OptionalSpanned[ExprType] with IdentifiedByString {

  case Primitive(ty: PrimitiveType)

  case Var(name: String)

  case Arrow(domain: ExprType, codomain: ExprType)

  case Forall(paramName: String, codomain: ExprType, constraints: Set[Constraint[ExprType]] = Set.empty)

  case Intersection(lhs: ExprType, rhs: ExprType)

  case Union(lhs: ExprType, rhs: ExprType)

  case Record(fields: Map[String, ExprType])

  case Tuple(elements: List[ExprType])

  case Ref(ty: ExprType)

  case Apply(func: ExprType, arg: ExprType)

  case Trait(inType: Option[ExprType], outType: ExprType)

  case SortOut(sortInTypeName: String)
  
  case SortApply(ty: ExprType, sort: ExprType.Sort)
  
  case Signature(sortInName: String, sortOutName: String, body: ExprType)

  case Array(elementType: ExprType)

  case Diff(lhs: ExprType, rhs: ExprType)

  case Span(ty: ExprType, span: SourceSpan)

  override def withSpan(span: SourceSpan): ExprType = this match {
    case ExprType.Span(_, _) => this
    case _ => ExprType.Span(this, span)
  }

  def synthesize(using env: TypeEnv)(
    using constraints: Set[Constraint[ExprType]] = Set.empty
  )(using sortMap: Map[String, String] = Map.empty[String, String]): Type = {
    
    val synthesizedType: Type = this match {

      case Primitive(ty) => Type.Primitive(ty)

      case Var(name) => env.types.get(name) match {
        case Some(ty) => ty
        case None => UnboundVariable.raise {
          s"Type variable $name is not bound in the current environment"
        }
      }

      case Arrow(domain, codomain) => Type.Arrow(domain.synthesize, codomain.synthesize)

      case Forall(paramName, codomain, cons) => {
        val allConstraints = constraints.filter(_.targetTypeParam == paramName) ++ cons
        val synthesizedConstraints = allConstraints.map(_.synthesize)
        env.withTypeVar(paramName, Type.Var(paramName)) { implicit newEnv =>
          Type.Forall(paramName, codomain.synthesize, synthesizedConstraints.map(_.rename(paramName)))
        }
      }

      case Intersection(lhs, rhs) => Type.Intersection(lhs.synthesize, rhs.synthesize)

      case Union(lhs, rhs) => Type.Union(lhs.synthesize, rhs.synthesize)

      case Record(fields) => Type.Record(fields.view.mapValues(_.synthesize).toMap)

      case Tuple(elements) => Type.Tuple(elements.map(_.synthesize))

      case SortOut(sortInTypeName) => sortMap.get(sortInTypeName) match {
        case Some(sortOutTypeName) => Type.Var(sortOutTypeName)
        case None => UnboundVariable.raise {
          s"Output type for sort `$sortInTypeName` is not bound in the current environment"
        }
      }
      
      case Ref(ty) => Type.Ref(ty.synthesize)

      case Apply(func, arg) => {
        val funcType = func.synthesize
        val argType = arg.synthesize
        funcType match {
          case Type.Forall(paramName, codomain, cons) => {
            cons.forall { constraint =>
              if !constraint.verify(argType) then {
                throw ConstraintNotSatisfied.raise {
                  s"Type argument $argType does not satisfy constraint $constraint"
                }
              } else true
            }
            // Substitute type parameter with argument type in codomain
            codomain.subst(paramName, argType)
          }
          case _ => TypeNotMatch.raise {
            s"Expected a polymorphic function type, got $funcType"
          }
        }
      }

      case Trait(inType, outType) => {
        Type.Trait(inType.map(_.synthesize).getOrElse(Type.top), outType.synthesize)
      }

      case SortApply(ty, sort) => {
        val baseType: Type = ty.synthesize
        val sortInType: Type = sort.inType.synthesize
        val sortOutType: Type = sort.outType match {
          // If output is explicitly provided, combine input & output
          case Some(value) => Type.Intersection(sortInType, value.synthesize).normalize
          // Default to input as output if no sort provided
          case None => sortInType
        }
        baseType match {
          case Type.Signature(sortInName, sortOutName, body) => {
            env.withTypeVars(
              sortInName -> sortInType,
              sortOutName -> sortOutType,
            ) { implicit newEnv => body.normalize }
          }
          case _ => TypeNotMatch.raise {
            s"Expected a signature type for sort application, got $baseType"
          }
        }
      }

      case Signature(sortIn, sortOut, body) => {
        env.withTypeVars(
          sortIn -> Type.Var(sortIn),
          sortOut -> Type.Var(sortOut),
        ) { implicit newEnv =>
          given newSortMap: Map[String, String] = sortMap + (sortIn -> sortOut)
          Type.Signature(sortIn, sortOut, body.synthesize)
        }
      }
      
      case Array(_) => ???

      case Diff(lhs, rhs) => Type.Diff(lhs.synthesize, rhs.synthesize)

      case Span(ty, span) => try ty.synthesize catch {
        case e: SpannedError => throw e
        case e: CoreError => throw e.withSpan(span)
        case e: Throwable => throw UnknownError(e, span)
      }
    }
    
    synthesizedType.normalize
  }
  
  override def contains(name: String): Boolean = this match {
    case Primitive(_) => false
    case Var(n) => n == name
    case Arrow(domain, codomain) => domain.contains(name) || codomain.contains(name)
    case Forall(paramName, codomain, constraints) =>
      paramName != name && (codomain.contains(name) || constraints.exists(_.subject.contains(name)))
    case Intersection(lhs, rhs) => lhs.contains(name) || rhs.contains(name)
    case Union(lhs, rhs) => lhs.contains(name) || rhs.contains(name)
    case Record(fields) => fields.values.exists(_.contains(name))
    case Tuple(elements) => elements.exists(_.contains(name))
    case Ref(ty) => ty.contains(name)
    case Apply(func, arg) => func.contains(name) || arg.contains(name)
    case Trait(inType, outType) =>
      inType.exists(_.contains(name)) || outType.contains(name)
    case SortOut(sortInTypeName) => sortInTypeName == name
    case SortApply(ty, sort) => 
      ty.contains(name) || sort.inType.contains(name) || sort.outType.exists(_.contains(name))
    case Signature(sortInName, sortOutName, body) =>
      sortInName != name && sortOutName != name && body.contains(name)
    case Array(elementType) => elementType.contains(name)
    case Diff(lhs, rhs) => lhs.contains(name) || rhs.contains(name)
    case Span(ty, _) => ty.contains(name)
  }

  override def toString: String = this match {
    case Primitive(ty) => ty.toString
    case Var(name) => name
    case Arrow(domain, codomain) => s"($domain) -> $codomain"
    case Forall(paramName, codomain, constraints) =>
      val consStr = if constraints.nonEmpty then
        constraints.map {
          case Constraint.Disjoint(_, disjoint) => s"disjoint $disjoint"
          case Constraint.UpperBound(_, upperBound) => s"<: $upperBound"
          case Constraint.LowerBound(_, lowerBound) => s">: $lowerBound"
        }.mkString(", ") + " "
      else ""
      s"forall $paramName. $consStr$codomain"
    case Signature(sortInName, sortOutName, body) => s"<$sortInName => $sortOutName> . $body"
    case Intersection(lhs, rhs) => s"($lhs & $rhs)"
    case Union(lhs, rhs) => s"($lhs | $rhs)"
    case Record(fields) =>
      s"{ ${fields.map { case (name, ty) => s"$name: $ty" }.mkString(", ")} }"
    case Tuple(elements) => s"(${elements.map(_.toString).mkString(", ")})"
    case Ref(ty) => s"&$ty"
    case SortOut(sortInTypeName) => s"(out $sortInTypeName)"
    case Apply(func, arg) => s"$func[$arg]"
    case Trait(inType, outType) =>
      inType match {
        case Some(inTy) => s"Trait[$inTy] -> $outType"
        case None => s"Trait -> $outType"
      }
    case SortApply(ty, sort) => s"$ty$sort"
    case Array(elementType) => s"Array[$elementType]"
    case Diff(lhs, rhs) => s"($lhs \\ $rhs)"
    case Span(ty, _) => ty.toString
  }
}

object ExprType {
  case class Sort(inType: ExprType, outType: Option[ExprType] = None) {
    override def toString: String = outType match {
      case Some(ty) => s"<$inType => $ty>"
      case None => s"<$inType>"
    }
  }
}

