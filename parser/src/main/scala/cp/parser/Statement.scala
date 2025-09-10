package cp.parser

import cp.syntax.{ExprTerm, ExprType}

enum Statement {
  case Expression(expr: ExprTerm)
  case RefAssign(reference: ExprTerm, value: ExprTerm)
  case Let(name: String, value: ExprTerm)
  case LetRec(name: String, value: ExprTerm, expectedType: ExprType)
  case LetTupleDestruct(names: List[String], value: ExprTerm)
  case LetRecordDestruct(fields: Map[String, String], value: ExprTerm)
}
