package cp.test

import cp.ast.{CpLexer, CpParser}
import cp.core.{Environment, EvalMode, Module, Term, Type}
import cp.error.SpannedError
import cp.parser.{ErrorListener, Visitor}
import cp.prelude.Prelude
import cp.syntax.ExprTerm
import cp.util.SourceSpan
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.matchers.should

trait TestExtension extends should.Matchers {
  
  extension (term: Term) def fullEval(using env: Environment): Term = {
    term.eval(using env)(using EvalMode.Full)
  }

  protected def parseExprTerm(code: String): ExprTerm = {
    val stripedCode = code.strip()
    catchError(stripedCode) { listener =>
      val lexer = CpLexer(CharStreams.fromString(stripedCode))
      lexer.removeErrorListeners()
      lexer.addErrorListener(listener)

      val parser = CpParser(CommonTokenStream(lexer))
      parser.removeErrorListeners()
      parser.addErrorListener(listener)

      Visitor().visitExpression(parser.expression)
    }
  }

  protected def synthExpr(code: String)(
    using env: Environment = Prelude.environment
  ): (Term, Type) = catchError(code.strip) { _ => 
    val (term, ty) = parseExprTerm(code).synthesize(using env)
    (term.eval, ty.normalize)
  }
  
  protected def synthModule(code: String)(
    using env: Environment = Prelude.environment
  ): Module = catchError(code.strip) { listener =>
    val stripedCode = code.strip()
    val lexer = CpLexer(CharStreams.fromString(stripedCode))
    lexer.removeErrorListeners()
    lexer.addErrorListener(listener)

    val parser = CpParser(CommonTokenStream(lexer))
    parser.removeErrorListeners()
    parser.addErrorListener(listener)

    val rawModule = Visitor().visitModule(parser.module())
    rawModule.synthesize(using env)
  }

  protected def printSourceWithHighlight(source: String, span: SourceSpan, info: String): Unit = {
    // Split source by lines for display
    val lines = source.split("\n").zipWithIndex

    // Calculate the line and character positions of the error
    val (startLine, endLine) = {
      val (start, end) = (span.start, span.end)
      val startLine = source.substring(0, start).count(_ == '\n')
      val endLine = source.substring(0, end).count(_ == '\n')
      (startLine, endLine)
    }

    // Print each line, and highlight the range containing the error
    lines.foreach {
      case (line, idx) if idx >= startLine && idx <= endLine => {
        println(f"$idx%4d: $line")
        // Highlight the error within the line
        if (idx == startLine) {
          val highlightStart = span.start - source.substring(0, span.start).lastIndexOf('\n') - 1
          val highlightEnd = if (startLine == endLine) span.end - span.start + highlightStart else line.length
          println(" " * (highlightStart + 6) + "^" * (highlightEnd - highlightStart + 1) + " " + info)
        }
      }
      case (line, idx) => println(f"$idx%4d: $line")
    }
  }

  protected def catchError[R](source: String)(action: ErrorListener => R): R = {
    val errorListener = ErrorListener(source)
    try action(errorListener) catch {
      case error: SpannedError => {
        error.infoSpans.headOption match {
          case Some(span, info) => {
            println(s"Error: ${error.message}")
            printSourceWithHighlight(source, span, info)
          }
          case None => println(s"Error: ${error.message}")
        }
        // Rethrow the original Error after showing it
        throw error
      }
    }
  }
  
  extension (expr: String) {
    protected infix def shouldEvalAs(expected: (Term, Type))(using env: Environment): Any = {
      val (term, ty) = synthExpr(expr)
      term should be (expected._1)
      ty should be (expected._2)
    }
    
    protected infix def shouldEvalAs(expected: Term)(using env: Environment): Any = {
      val (term, _) = synthExpr(expr)
      term should be (expected)
    }
    
    protected infix def shouldHaveType(expected: Type)(using env: Environment): Any = {
      // TODO: use `check`
      val (_, ty) = synthExpr(expr)
      ty should be (expected)
    }
  }
  
  protected def withIn(code: String)(f: Environment => Any): Unit = {
    f(synthModule(code)(using Prelude.environment).toEnv)
  }
}
