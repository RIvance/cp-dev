package cp.parser

import cp.syntax.{ExprTerm, ExprType}
import cp.util.SourceSpan

enum Statement(val span: SourceSpan) {
  case Expression(
    expr: ExprTerm
  )(implicit span: SourceSpan) extends Statement(span)
  
  case RefAssign(
    reference: ExprTerm, value: ExprTerm
  )(implicit span: SourceSpan) extends Statement(span)
  
  case Let(
    name: String, value: ExprTerm, ty: Option[ExprType]
  )(implicit span: SourceSpan) extends Statement(span)
  
  case LetTupleDestruct(
    names: List[String], value: ExprTerm
  )(implicit span: SourceSpan) extends Statement(span)
  
  case LetRecordDestruct(
    fields: Map[String, String], value: ExprTerm
  )(implicit span: SourceSpan) extends Statement(span)
}
