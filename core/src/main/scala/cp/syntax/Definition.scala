package cp.syntax

import cp.core.{Constraint, Environment, Module, Term, Type}
import cp.util.{OptionalSpanned, SourceSpan}

enum Definition extends OptionalSpanned[Definition] {
  
  case TermDef(
    name: String, 
    term: ExprTerm, 
    constraints: Set[Constraint[ExprType]] = Set.empty,
  )
  
  case TypeDef(
    name: String, 
    typeDef: ExprType, 
    constraints: Set[Constraint[ExprType]] = Set.empty,
  )
  
  case SubmodDef(name: String, module: RawModule)
  
  case Spanned(span: SourceSpan, definition: Definition)
  
  override def withSpan(span: SourceSpan): Definition = this match {
    case Definition.Spanned(_, _) => this
    case _ => Definition.Spanned(span, this)
  }
  
  def synthesize(using env: Environment[Type, Term]): Term | Type | Module = ???
}
