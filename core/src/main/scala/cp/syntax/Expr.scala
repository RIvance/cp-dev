package cp.syntax

import cp.core.{Literal, LiteralType}
import cp.util.SourceSpan

trait Expr {
  val span: SourceSpan
}

enum ExprType(override val span: SourceSpan) extends Expr {
  
  case Primitive(
    ty: LiteralType
  )(implicit span: SourceSpan) extends ExprType(span)
  
  case Arrow(
    domain: ExprType, codomain: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)

  case Forall(
    paramName: String, codomain: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)
  
  case Intersection(
    lhs: ExprType, rhs: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)
  
  case Union(
    lhs: ExprType, rhs: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)
  
  case Record(
    fields: Map[String, ExprType]
  )(implicit span: SourceSpan) extends ExprType(span)
  
  case Ref(
    ty: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)

  case Apply(
    func: ExprType, arg: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)

  case Trait(
    inType: Option[ExprType], outType: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)

  case Sort(
    inType: ExprType, outType: Option[ExprType]
  )(implicit span: SourceSpan) extends ExprType(span)

  case Array(
    elementType: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)

  case Diff(
    lhs: ExprType, rhs: ExprType
  )(implicit span: SourceSpan) extends ExprType(span)
}

enum ExprTerm(override val span: SourceSpan) extends Expr {

  case Primitive(
    value: Literal
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Variable(
    name: String
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case UnaryOp(
    op: String, expr: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case BinaryOp(
    op: String, left: ExprTerm, right: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Apply(
    fn: ExprTerm, args: List[ExprTerm]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Lambda(
    paramName: String, paramType: Option[ExprType], body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  // Λα.M -- type-level lambda abstraction
  case TypeLambda(
    paramName: String, body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Fixpoint(
    paramName: String, paramType: Option[ExprType], body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case IfThenElse(
    cond: ExprTerm, thenBranch: ExprTerm, elseBranch: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case LetIn(
    name: String, value: ExprTerm, body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case LetRecIn(
    name: String, value: ExprTerm, body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Record(
    fields: Map[String, ExprTerm]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Merge(
    left: ExprTerm, right: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

//  case Match(
//    scrutinee: ExprTerm, cases: List[(Pattern, ExprTerm)]
//  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Project(
    record: ExprTerm, field: String
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case TypeApply(
    term: ExprTerm, tyArgs: List[ExprType]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case OpenIn(
    record: ExprTerm, body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Update(
    record: ExprTerm, updates: List[(String, ExprTerm)]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Trait(
    selfAnno: SelfAnnotation,
    implements: Option[ExprType],
    inherits: Option[ExprTerm], body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case New(
    body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Forward(
    left: ExprTerm, right: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Exclude(
    left: ExprTerm, right: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Remove(
    record: ExprTerm, fields: List[String]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Diff(
    left: ExprTerm, right: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Rename(
    record: ExprTerm, renames: List[(String, String)]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case FoldFixpoint(
    fixpointType: ExprType, body: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case UnfoldFixpoint(
    fixpointType: ExprType, term: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Ref(
    term: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Deref(
    term: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case RefAssign(
    ref: ExprTerm, value: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Seq(
    first: ExprTerm, second: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case ArrayLiteral(
    elements: List[ExprTerm]
  )(implicit span: SourceSpan) extends ExprTerm(span)

  case Document(
    content: ExprTerm
  )(implicit span: SourceSpan) extends ExprTerm(span)
}

enum MergeBias {
  case Neutral, Left, Right
}

case class SelfAnnotation(
  name: String,
  ty: Option[ExprType],
)