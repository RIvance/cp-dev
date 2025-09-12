package cp.syntax

import cp.core.*
import cp.error.{CoreError, SpannedError, UnknownError}
import cp.error.CoreErrorKind.*
import cp.util.{OptionalSpanned, SourceSpan}

enum ExprTerm extends Synthesis[(Term, Type)] with OptionalSpanned[ExprTerm] {

  case Primitive(value: Literal)

  case Var(name: String)

  case Typed(expr: ExprTerm, expectedType: ExprType)

  case Apply(fn: ExprTerm, args: List[ExprTerm])

  case Lambda(paramName: String, paramType: Option[ExprType], body: ExprTerm)

  // Λα.M -- type-level lambda abstraction
  case TypeLambda(paramName: String, body: ExprTerm)

  case Fixpoint(paramName: String, paramType: Option[ExprType], body: ExprTerm)

  case IfThenElse(cond: ExprTerm, thenBranch: ExprTerm, elseBranch: ExprTerm)

  case LetIn(name: String, value: ExprTerm, body: ExprTerm)

  case LetRecIn(name: String, value: ExprTerm, body: ExprTerm)

  case Record(fields: Map[String, ExprTerm])

  case Tuple(elements: List[ExprTerm])

  case Merge(left: ExprTerm, right: ExprTerm, bias: MergeBias = MergeBias.Neutral)

//  case Match( scrutinee: ExprTerm, cases: List[(Pattern, ExprTerm)])

  case Projection(record: ExprTerm, field: String)

  case TypeApply(term: ExprTerm, tyArgs: List[ExprType])

  case OpenIn(record: ExprTerm, body: ExprTerm)

  case Update(record: ExprTerm, updates: Map[String, ExprTerm])

  case Trait(
    selfAnno: SelfAnnotation[ExprType],
    implements: Option[ExprType],
    inherits: Option[ExprTerm], body: ExprTerm
  )

  case New(body: ExprTerm)

  case Forward(left: ExprTerm, right: ExprTerm)

  case Exclude(left: ExprTerm, right: ExprType)

  case Remove(record: ExprTerm, fields: Set[String])

  case Diff(left: ExprTerm, right: ExprTerm)

  case Rename(record: ExprTerm, renames: Map[String, String])

  case FoldFixpoint(fixpointType: ExprType, body: ExprTerm)

  case UnfoldFixpoint(fixpointType: ExprType, term: ExprTerm)

  // See `cp.core.Term.Do` for explanation.
  case Do(expr: ExprTerm, body: ExprTerm)

  case Seq(first: ExprTerm, second: ExprTerm)

  case ArrayLiteral(elements: List[ExprTerm])

  case Document(content: ExprTerm)

  case Span(term: ExprTerm, span: SourceSpan)

  override def withSpan(span: SourceSpan): ExprTerm = this match {
    case ExprTerm.Span(_, _) => this
    case _ => ExprTerm.Span(this, span)
  }

  override def synthesize(using env: Environment): (Term, Type) = synthesize(None)(using env)

  def synthesize(ty: Option[Type])(using env: Environment): (Term, Type) = {
    this match {
      case ExprTerm.Primitive(value) => {
        val ty = value match {
          case Literal.IntValue(_) => LiteralType.IntType
          case Literal.FloatValue(_) => LiteralType.FloatType
          case Literal.BoolValue(_) => LiteralType.BoolType
          case Literal.RuneValue(_) => LiteralType.RuneType
          case Literal.StringValue(_) => LiteralType.StringType
          case Literal.UnitValue => LiteralType.UnitType
        }
        (Term.Primitive(value), Type.Primitive(ty))
      }

      case ExprTerm.Var(name) => {
        env.termVars.get(name) match {
          case Some(term) => (term, term.infer)
          case None => UnresolvedReference.raise(s"Undefined term: $name")
        }
      }

      case ExprTerm.Typed(expr, expectedTypeExpr) => {
        val (term, ty) = expr.synthesize
        val expectedType = expectedTypeExpr.synthesize
        if !(ty <:< expectedType) then {
          TypeNotMatch.raise {
            s"Expected type: $expectedType, found: $ty"
          }
        } else (term, ty)
      }

      case ExprTerm.Apply(fn, args) => {
        val (fnTerm, fnType) = fn.synthesize
        val (fnAppTerm, fnAppType) = args.foldLeft((fnTerm, fnType)) {
          case ((accFnTerm: Term, accFnType: Type), arg) => {
            accFnType match {
              case Type.Arrow(domain, codomain) => {
                // When arg is a lambda without annotation, 
                //  we need to infer its type from accFnType
                val (argTerm, argType) = arg.synthesize(Some(domain))
                if !argTerm.check(codomain) then TypeNotMatch.raise {
                  s"Expected argument type: $domain, found: $argType"
                }
                (Term.Apply(accFnTerm, argTerm), codomain)
              }
              case _ => TypeNotMatch.raise {
                s"Function application on a non-function type: $accFnType"
              }
            }
          }
        }
        if !fnAppTerm.check(fnAppType) then TypeNotMatch.raise {
          s"Function application term does not check against its type: $fnAppTerm : $fnAppType"
        } else (fnAppTerm, fnAppType)
      }

      case ExprTerm.Lambda(paramName, paramTypeOpt, body) => {
        val paramType: Type = paramTypeOpt match {
          case Some(tyExpr) => tyExpr.synthesize
          case None => ty match {
            case Some(Type.Arrow(domain, _)) => domain
            case _ => TypeNotMatch.raise {
              s"Cannot infer parameter type of lambda without annotation: $this"
            }
          }
        }
        env.withTermVar(paramName, Term.Typed(Term.Var(paramName), paramType)) { env =>
          val (bodyTerm, bodyType) = body.synthesize(using env)
          val lambdaTerm = Term.Lambda(paramName, paramType, bodyTerm)
          val lambdaType = Type.Arrow(paramType, bodyType)
          if !lambdaTerm.check(lambdaType) then TypeNotMatch.raise {
            s"Lambda term does not check against its type: $lambdaTerm : $lambdaType"
          } else (lambdaTerm, lambdaType)
        }
      }
      
      case ExprTerm.TypeLambda(_, _) => ???
      case ExprTerm.Fixpoint(_, _, _) => ???
      case ExprTerm.IfThenElse(_, _, _) => ???
      case ExprTerm.LetIn(_, _, _) => ???
      case ExprTerm.LetRecIn(_, _, _) => ???
      case ExprTerm.Record(_) => ???
      case ExprTerm.Tuple(_) => ???
      case ExprTerm.Merge(_, _, _) => ???
      case ExprTerm.Projection(_, _) => ???
      case ExprTerm.TypeApply(_, _) => ???
      case ExprTerm.OpenIn(_, _) => ???
      case ExprTerm.Update(_, _) => ???
      case ExprTerm.Trait(_, _, _, _) => ???
      case ExprTerm.New(_) => ???
      case ExprTerm.Forward(_, _) => ???
      case ExprTerm.Exclude(_, _) => ???
      case ExprTerm.Remove(_, _) => ???
      case ExprTerm.Diff(_, _) => ???
      case ExprTerm.Rename(_, _) => ???
      case ExprTerm.FoldFixpoint(_, _) => ???
      case ExprTerm.UnfoldFixpoint(_, _) => ???
      case ExprTerm.Seq(_, _) => ???
      case ExprTerm.ArrayLiteral(_) => ???
      case ExprTerm.Document(_) => ???

      case ExprTerm.Do(_, _) => ???

      case ExprTerm.Span(term, span) => try term.synthesize catch {
        case e: CoreError => throw e.withSpan(span)
        case e: SpannedError => throw e
        case e: Throwable => throw UnknownError(e, span)
      }
    }
  }
}

object ExprTerm {
  def int(value: Int): ExprTerm = ExprTerm.Primitive(Literal.IntValue(value))
  def str(value: String): ExprTerm = ExprTerm.Primitive(Literal.StringValue(value))
  def bool(value: Boolean): ExprTerm = ExprTerm.Primitive(Literal.BoolValue(value))
  def unit: ExprTerm = ExprTerm.Primitive(Literal.UnitValue)
}

