package cp.surface

import org.antlr.v4.runtime.ParserRuleContext
import cp.surface.SpineParser.{Associativity, Operator, Token, UnaryType}
import cp.surface.syntax.{Definition, Evaluation, ExprTree, Spanned, Statement, SyntaxTree}
import cp.surface.SyntaxError.*
import cp.core.Literal.*
import cp.core.{Pattern, Literal as LiteralValue}
import cp.core.syntax.{ApplyMode, Argument, Clause, Param, Var}
import cp.ast.CpParserBaseVisitor
import cp.ast.CpParser.*
import cp.util.*
import cp.error.CoreErrorKind.*

import scala.collection.Seq
import scala.jdk.CollectionConverters.*

class Visitor extends CpParserBaseVisitor[SyntaxTree[?] | Seq[SyntaxTree[?]]] {
  
}
