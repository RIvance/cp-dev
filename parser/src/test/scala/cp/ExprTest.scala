package cp

import cp.core.{Environment, Literal, LiteralType, Term, Type}
import cp.core.LiteralType.*
import cp.core.Literal.*
import cp.prelude.Prelude
import cp.test.TestExtension
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class ExprTest extends AnyFunSuite with should.Matchers with TestExtension {
  
  given Environment = Prelude.environment
  
  test("synth primitive type Int") {
    val (term, ty) = synthExpr("1")
    ty should be (IntType.toType)
    term should be (IntValue(1).toTerm)
  }
  
  test("term in environment") {
    given env: Environment = Prelude.environment.addTermVar("x", IntValue(42).toTerm)
    val (term, ty) = synthExpr("x")
    ty should be (IntType.toType)
    term should be (IntValue(42).toTerm)
  }
  
  test("add two integers") {
    val (term, ty) = synthExpr("1 + 2")
    ty should be (IntType.toType)
    term should be (IntValue(3).toTerm)
  }
  
  test("concatenate two strings") {
    val (term, ty) = synthExpr("\"Hello, \" ++ \"world!\"")
    ty should be (StringType.toType)
    term should be (StringValue("Hello, world!").toTerm)
  }
  
  test("synth lambda") {
    val (term, ty) = synthExpr("fun (x: Int) -> x + 1")
    ty should be (Type.Arrow(IntType.toType, IntType.toType))
    Term.Apply(term, IntValue(41).toTerm).eval should be (IntValue(42).toTerm)
  }
  
  test("synth high-order lambda") {
    val (term, ty) = synthExpr("fun (f: Int -> Int) -> f(41)")
    ty should be (Type.Arrow(
      Type.Arrow(IntType.toType, IntType.toType),
      IntType.toType
    ))
    val (lambdaArg, _) = synthExpr("fun (x: Int) -> x + 1")
    Term.Apply(term, lambdaArg).fullEval should be (IntValue(42).toTerm)
  }
}