package cp.syntax

import cp.util.{OptionalSpanned, SourceSpan}

enum Definition extends OptionalSpanned[Definition] {
  
  case TermDef(
    name: String, 
    term: ExprTerm, 
    constraints: Set[DisjointConstraint] = Set.empty,
  )
  
  case TypeDef(
    name: String, 
    typeDef: ExprType, 
    constraints: Set[DisjointConstraint] = Set.empty,
  )
  
  case SubmodDef(name: String, module: RawModule)
  
  case Spanned(span: SourceSpan, definition: Definition)
  
  override def withSpan(span: SourceSpan): Definition = this match {
    case Definition.Spanned(_, _) => this
    case _ => Definition.Spanned(span, this)
  }
}

case class DisjointConstraint(left: String, right: ExprType)
