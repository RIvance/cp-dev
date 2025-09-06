package cp.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import cp.concrete.span

import cp.util.{unreachable, LateInit, SourceSpan}

import scala.collection.{mutable, Seq}

enum Definition(implicit ctx: ParserRuleContext) extends SyntaxTree[CoreDefinition[Expr]] {

  def ident: String

  override def span: SourceSpan = ctx.span

  override def emit: CoreDefinition[Expr] = null

}
