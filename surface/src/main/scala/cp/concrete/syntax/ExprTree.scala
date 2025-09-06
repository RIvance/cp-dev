package cp.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import cp.concrete.span
import cp.core.{Entity, LiteralType, Param, Expr as CoreExpr}
import cp.core.syntax.*
import cp.error.CoreErrorKind
import cp.error.CoreErrorKind.UnsupportedFeature
import cp.util.SourceSpan

import scala.collection.Seq

enum ExprTree(implicit ctx: ParserRuleContext) extends SyntaxTree[CoreExpr] with Entity {

  override def span: SourceSpan = ctx.span

  case Universe()(implicit ctx: ParserRuleContext)

  case Variable(
    name: String,
  )(implicit ctx: ParserRuleContext)

  case PrimitiveValue(
    value: Literal,
  )(implicit ctx: ParserRuleContext)

  case PrimitiveType(
    `type`: LiteralType,
  )(implicit ctx: ParserRuleContext)
  
  given SourceSpan = ctx.span

  override def emit: CoreExpr = this match {

    case Universe() => CoreExpr.Universe()
    case Variable(name) => CoreExpr.Unresolved(name)
    case PrimitiveValue(value) => CoreExpr.Primitive(value)
    case PrimitiveType(ty) => CoreExpr.PrimitiveType(ty)
    
    // TODO
  }

}
