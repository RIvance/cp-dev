package cp.tool

import cp.cli.ReadEvalPrintLoop
import cp.common.Environment
import cp.core.{CoreModule, Dependency, Module, Namespace, Term, Type, Value}
import cp.error.SpannedError
import cp.interpreter.TrampolineInterpreter
import cp.parser.{ErrorListener, Statement, SyntaxError, Visitor}
import cp.prelude.Prelude
import cp.syntax.{Definition, ExprTerm, ExprType}
import cp.util.SourceSpan

import scala.annotation.tailrec

private class ReplCore {

  case class EvalResult(value: Term, ty: Type) {
    override def toString: String = s"$value : $ty"
  }

  private val visitor: Visitor = Visitor()
  implicit var environment: Environment[String, Type, Value] = Environment.empty

  trait ReplModule extends Module {
    def addTerm(name: String, term: Term): Unit
    def addType(name: String, ty: Type): Unit
    def importModule(other: Module): Unit
  }

  private lazy val interpreter = TrampolineInterpreter(Prelude, module)

  var module: ReplModule = new ReplModule {
    override val namespace: Namespace = Namespace("repl")
    var types: Map[String, Type] = Map.empty
    var terms: Map[String, Term] = Map.empty
    var dependencies: Set[Dependency] = Set(Prelude)
    var submodules: Map[String, CoreModule] = Map.empty
    
    override def addTerm(name: String, term: Term): Unit = terms += (name -> term)
    override def addType(name: String, ty: Type): Unit = types += (name -> ty)
    override def importModule(other: Module): Unit = {
      interpreter.loadModule(other)
      dependencies += other
    }

  }

  private def moduleEnvironment: Environment[String, Type, Term] = module.importEnvironment

  def iterate(source: String): Unit = {

    val listener = ErrorListener(source)
    
    // The ANTLR parser should be re-created for each new input to reset its state.
    //  See: https://github.com/antlr/antlr4/issues/4843
    def parser = parseSource(source, listener)
    
    try {
      
      val input: Definition | Statement | ExprTerm | ExprType = try {
        visitor.visitDefinition(parser.singletonDef.definition)
      } catch {
        case _: Throwable => {
          // Not seems to be a definition, try statement
          try {
            visitor.visitStatement(parser.singletonStmt.stmt)
          } catch {
            // Not seems to be a statement, try type term
            case _: Throwable => try {
              visitor.visitType(parser.singletonType.`type`)
            } catch {
              case _: Throwable => {
                // Not seems to be a type term, try expression
                try visitor.visitExpression(parser.singletonExpr.expression) catch {
                  case e: SyntaxError => throw e
                  case e: Throwable => throw SyntaxError.InvalidInput(
                    "Input is not a valid definition, statement, type, or expression.",
                    span = SourceSpan(0, source.length - 1)
                  )
                }
              }
            }
          }
        }
      }
      
      @tailrec
      def iterateInput(input: Definition | Statement | ExprTerm | ExprType): Unit = input match {
        case defn: Definition => defn match {
          case Definition.TermDef(name, termExpr, constraints) => {
            val (term: Term, ty: Type) = termExpr.synthesize(using module.importEnvironment)
            val evaluatedTerm = interpreter.eval(term)
            // println(s"  $name = ${evaluatedTerm} : ${ty.normalize}\n")
            println()
            module.addTerm(name, term)
          }
          case Definition.TypeDef(name, typeExpr, constraints) => {
            val ty: Type = typeExpr.synthesize
            // println(s"  type $name = ${ty.normalize}\n")
            println()
            environment = environment.addTypeVar(name, ty)
          }

          case Definition.SubmodDef(_, _) => ???
          case Definition.Spanned(_, defn) => iterateInput(defn)
        }
        case stmt: Statement => stmt match {
          case Statement.Expression(expr) => iterateInput(expr.withSpan(stmt.span))
          case Statement.Let(name, valueExpr, tyExprOpt) => {
            val termEnv = moduleEnvironment.merge(environment.mapValues { (_, v) => v.toTerm })
            val (term: Term, ty: Type) = valueExpr.synthesize(using moduleEnvironment.merge(termEnv))
            val evaluated = interpreter.eval(term)
            // println(s"  $name = ${evaluatedTerm} : ${ty.normalize}\n")
            println()
            environment = environment.addValueVar(name, evaluated)
          }
          case Statement.LetTupleDestruct(names, valueExpr) => ???
          case Statement.LetRecordDestruct(fields, valueExpr) => ???
          case Statement.RefAssign(referenceExpr, valueExpr) => ???
        }
        case expr: ExprTerm => {
          val termEnv = moduleEnvironment.merge(environment.mapValues { (_, v) => v.toTerm })
          val (term: Term, ty: Type) = expr.synthesize(using termEnv)
          println(s"  ${interpreter.eval(term)(using environment)} : ${ty.normalize}\n")
        }
        case tyExpr: ExprType => try {
          val ty = tyExpr.synthesize
          println(s"  type $ty\n")
        } catch {
          case _: Throwable => {
            // Failed to parse as type, try as expression
            val termEnv = moduleEnvironment.merge(environment.mapValues { (_, v) => v.toTerm })
            val (term, ty) = visitor.visitExpression(parser.singletonExpr.expression).synthesize(using termEnv)
            println(s"  ${interpreter.eval(term)(using environment)} : ${ty.normalize}\n")
          }
        }
      }

      catchError(source, path = None, doPrint = false) { _ => iterateInput(input) }
      
    } catch {
      case error: SpannedError => printError(source, error)
      case error => error.printStackTrace()
    }
  }

  private def printError(source: String, error: SpannedError): Unit = {
    error.infoSpans.headOption match {
      case Some((span, info)) => {
        val spanLength = span.end - span.start + 1
        ReadEvalPrintLoop.printError(source, "", error.message, info, span.start, spanLength)
      }
      case None => println(s"Error: ${error.message}")
    }
  }
}
