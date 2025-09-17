package cp.tool

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import cp.cli.ReadEvalPrintLoop
import cp.parser.{ErrorListener, Visitor}
import cp.core.{Literal, Module}
import cp.core.Term
import cp.syntax.RawModule
import cp.error.{CoreError, SpannedError, PanicError}
import cp.ast.{CpLexer, CpParser}

def catchError[R](source: String, path: Option[String] = None, doPrint: Boolean = true)(
  action: ErrorListener => R
): R = {
  val errorListener = ErrorListener(source)
  try action(errorListener) catch {
    case error: SpannedError => {
      error.infoSpans.headOption match {
        case Some(span, info) => {
          val spanLength = span.end - span.start + 1
          if doPrint then {
            ReadEvalPrintLoop.printError(source, path.getOrElse(""), error.message, info, span.start, spanLength)
          }
        }
        case None => if doPrint then println(s"Error: ${error.message}")
      }
      // Rethrow the original Error after showing it
      throw error
    }
  }
}

def loadModule(source: String, path: Option[String] = None): Module = {
  catchError(source, path) { listener =>
    val parser = parseSource(source, listener)
    val rawModule: RawModule = Visitor().visitModule(parser.module())
    rawModule.synthesize
  }
}

def parseSource(source: String, listener: ErrorListener): CpParser = {
  val lexer = CpLexer(CharStreams.fromString(source))
  lexer.removeErrorListeners()
  lexer.addErrorListener(listener)
  val parser = CpParser(CommonTokenStream(lexer))
  parser.removeErrorListeners()
  parser.addErrorListener(listener)
  return parser
}
