package cp.syntax

import cp.core.LiteralType
import cp.util.SourceSpan

enum ExprType {

  case Primitive(ty: LiteralType)

  case Var(name: String)

  case Arrow(domain: ExprType, codomain: ExprType)

  case Forall(paramName: String, codomain: ExprType)

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

  def withSpan(span: SourceSpan): ExprType = this match {
    case ExprType.Span(_, _) => this
    case _ => ExprType.Span(this, span)
  }
}
