package cp.syntax

import cp.core.{verify, synthesize, Constraint, Environment, LiteralType, Type}
import cp.error.{CoreError, SpannedError, UnknownError}
import cp.error.CoreErrorKind.*
import cp.util.{OptionalSpanned, SourceSpan}

enum ExprType extends OptionalSpanned[ExprType] {

  case Primitive(ty: LiteralType)

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

  case Sort(inType: ExprType, outType: Option[ExprType])

  case Array(elementType: ExprType)

  case Diff(lhs: ExprType, rhs: ExprType)

  case Span(ty: ExprType, span: SourceSpan)

  override def withSpan(span: SourceSpan): ExprType = this match {
    case ExprType.Span(_, _) => this
    case _ => ExprType.Span(this, span)
  }

  def synthesize(using env: Environment)(
    using constraints: Set[Constraint[ExprType]] = Set.empty
  ): Type = this match {
    
    case Primitive(ty) => Type.Primitive(ty)
    
    case Var(name) => {
      env.typeVars.get(name) match {
        case Some(ty) => ty
        case None => UnboundVariable.raise {
          s"Type variable $name is not bound in the current environment"
        }
      }
    }
    
    case Arrow(domain, codomain) => Type.Arrow(domain.synthesize, codomain.synthesize)
    
    case Forall(paramName, codomain, cons) => {
      val allConstraints = constraints.filter(_.targetTypeParam == paramName) ++ cons
      val synthesizedConstraints = allConstraints.map(_.synthesize)
      env.withTypeVar(paramName, Type.Var(paramName)) { newEnv =>
        Type.Forall(paramName, codomain.synthesize, synthesizedConstraints.map(_.rename(paramName)))
      }
    }
    
    case Intersection(lhs, rhs) => Type.Intersection(lhs.synthesize, rhs.synthesize)
    
    case Union(lhs, rhs) => Type.Union(lhs.synthesize, rhs.synthesize)
    
    case Record(fields) => Type.Record(fields.view.mapValues(_.synthesize).toMap)
    
    case Tuple(elements) => Type.Tuple(elements.map(_.synthesize))
    
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
    
    // TODO: implement
    case Sort(_, _) => ???
    
    case Array(_) => ???
    
    case Diff(lhs, rhs) => Type.Diff(lhs.synthesize, rhs.synthesize)
    
    case Span(ty, span) => try ty.synthesize catch {
      case e: SpannedError => throw e
      case e: CoreError => throw e.withSpan(span)
      case e: Throwable => throw UnknownError(e, span)
    }
  }
}
