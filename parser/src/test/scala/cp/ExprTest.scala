package cp

import cp.core.Literal.*
import cp.core.LiteralType.*
import cp.core.{Literal, LiteralType, Type}
import cp.prelude.Prelude
import cp.test.TestExtension
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class ExprTest extends AnyFunSuite with should.Matchers with TestExtension {
  
  given Env = Prelude.environment
  
  test("primitive type Int") {
    "1" >>> (IntValue(1).toTerm, IntType.toType)
  }
  
  test("term in environment") {
    given env: Env = Prelude.environment.addValueVar("x", IntValue(42).toTerm)
    "x" >>> (IntValue(42).toTerm, IntType.toType)
  }
  
  test("add two integers") {
    "1 + 2" >>> (IntValue(3).toTerm, IntType.toType)
  }
  
  test("concatenate two strings") {
    "\"Hello, \" ++ \"world!\"" >>> (
      Literal.StringValue("Hello, world!").toTerm, 
      LiteralType.StringType.toType
    )
  }
  
  test("simple lambda") {
    "fun (x: Int) -> x + 1" >>: Type.Arrow(IntType.toType, IntType.toType)
    "(fun (x: Int) -> x + 1) 41" >>> (IntValue(42).toTerm, IntType.toType)
  }
  
  test("high-order lambda") {
    "fun (f: Int -> Int) -> f(41)" >>: Type.Arrow(
      Type.Arrow(IntType.toType, IntType.toType),
      IntType.toType
    )
    "(fun (f: Int -> Int) -> f(41)) (fun (x: Int) -> x + 1)" >>> (
      IntValue(42).toTerm, IntType.toType
    )
    "(fun (f: Int -> Int) -> f(41)) (x -> x + 1)" >>> (
      IntValue(42).toTerm, IntType.toType
    )
  }
  
  test("method style function call") {
    "114514.toString" >>> (
      Literal.StringValue("114514").toTerm, 
      LiteralType.StringType.toType
    )
    "(1 + 2).toString" >>> (
      Literal.StringValue("3").toTerm, 
      LiteralType.StringType.toType
    )
    "(\"Hello, \" ++ \"world!\").length" >>> (
      Literal.IntValue(13).toTerm, 
      LiteralType.IntType.toType
    )
    "(114.toString ++ 514.toString).length" >>> (
      Literal.IntValue(6).toTerm, 
      LiteralType.IntType.toType
    )
  }
}