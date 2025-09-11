package cp.syntax

import cp.core.*
import cp.util.SourceSpan

enum ExprTerm extends Synthesis[(Term, Type)] {

  case Primitive(value: Literal)

  case Variable(name: String)

  case Typed(expr: ExprTerm, ty: ExprType)

  case UnaryOp(op: String, expr: ExprTerm)

  case BinaryOp(op: String, left: ExprTerm, right: ExprTerm)

  case Index(base: ExprTerm, index: ExprTerm)

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

  case Effective(effect: PureEffect[ExprTerm, ExprType], body: ExprTerm)

  case Neutral(neutral: NeutralEffect[ExprTerm, ExprType])

  case Seq(first: ExprTerm, second: ExprTerm)

  case ArrayLiteral(elements: List[ExprTerm])

  case Document(content: ExprTerm)

  case Span(term: ExprTerm, span: SourceSpan)

  def withSpan(span: SourceSpan): ExprTerm = this match {
    case ExprTerm.Span(_, _) => this
    case _ => ExprTerm.Span(this, span)
  }

  override def synthesize: (Term, Type) = ???
}

object ExprTerm {
  def int(value: Int): ExprTerm = ExprTerm.Primitive(Literal.IntValue(value))
  def str(value: String): ExprTerm = ExprTerm.Primitive(Literal.StringValue(value))
  def bool(value: Boolean): ExprTerm = ExprTerm.Primitive(Literal.BoolValue(value))
  def unit: ExprTerm = ExprTerm.Primitive(Literal.UnitValue)
}

