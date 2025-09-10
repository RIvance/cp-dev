package cp.core

//import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
//import cp.parser.Visitor
//import cp.core.catchError
//import cp.core.context.Environment
//import cp.core.domain.{NeutralValue, Type, Value}
//import cp.core.elaborate.Resolve
//import cp.core.syntax.{Module, Var}
//import cp.core
//import cp.core.term
//import cp.core.term.Term
//import cp.ast.{CpLexer, CpParser}
//import cp.prelude.Prelude
//
//import scala.annotation.targetName

//trait CpTestExt {
//
//  extension (str: String) {
//    @targetName("localVar")
//    def unary_! : Var.Local = Var.Local(str)
//  }
//
//  extension (literal: Literal) {
//    def term: core.term.Primitive = core.term.Primitive(literal)
//    def value: Value.Primitive = Value.Primitive(literal)
//  }
//
//  extension (literalType: LiteralType) {
//    def term: core.term.PrimitiveType = core.term.PrimitiveType(literalType)
//    def value: Value.PrimitiveType = Value.PrimitiveType(literalType)
//  }
//
//  extension (value: Value) {
//    def neutral: NeutralValue = value match {
//      case Value.Neutral(neutral) => neutral
//      case _ => throw new Exception("Not a neutral value.")
//    }
//
//    def apply(arg: Value): Value = value match {
//      case Value.Lambda(_, closure) => closure(arg)
//      case Value.Neutral(neutral) => Value.Neutral(NeutralValue.Apply(neutral, arg))
//      case _ => throw new Exception("Not a function.")
//    }
//  }
//
//  given Environment.Typed[Value] = Prelude.environment
//  given Resolve.Context = Resolve.Context(Prelude.symbols)
//
//  extension (module: Module) {
//    def eval(code: String): Module.EvalResult = {
//      val stripedCode = code.strip()
//      catchError(stripedCode) { listener =>
//        val lexer = CpLexer(CharStreams.fromString(stripedCode))
//        lexer.removeErrorListeners()
//        lexer.addErrorListener(listener)
//
//        val parser = CpParser(CommonTokenStream(lexer))
//        parser.removeErrorListeners()
//        parser.addErrorListener(listener)
//
//        val expr = Visitor().visitExpr(parser.expr())
//        module.evaluate(expr.emit)
//      }
//    }
//  }
//
//  def parseExpr(code: String): Expr = {
//    val stripedCode = code.strip()
//    catchError(stripedCode) { listener =>
//      val lexer = CpLexer(CharStreams.fromString(stripedCode))
//      lexer.removeErrorListeners()
//      lexer.addErrorListener(listener)
//
//      val parser = CpParser(CommonTokenStream(lexer))
//      parser.removeErrorListeners()
//      parser.addErrorListener(listener)
//
//      Visitor().visitExpr(parser.expr()).emit.resolve._1
//    }
//  }
//
//  def synthExpr(code: String): (Term, Value) = catchError(code.strip) {
//    _ => parseExpr(code).synth.normalize.unpack
//  }
//
//  def synthCodeBlock(code: String): (Term, Type) = {
//    val stripedCode = "{\n" + code.strip() + "\n}"
//    catchError[(Term, Value)](stripedCode) { listener =>
//      val lexer = CpLexer(CharStreams.fromString(stripedCode))
//      lexer.removeErrorListeners()
//      lexer.addErrorListener(listener)
//
//      val parser = CpParser(CommonTokenStream(lexer))
//      parser.removeErrorListeners()
//      parser.addErrorListener(listener)
//
//      val block = Visitor().visitBlockExpr(parser.blockExpr())
//      block.emit.synth.normalize.unpack
//    }
//  }
//}
