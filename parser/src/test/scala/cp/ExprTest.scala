package cp

import cp.core.PrimitiveValue.*
import cp.core.PrimitiveType.*
import cp.core.{PrimitiveType, PrimitiveValue, Type}
import cp.prelude.Prelude
import cp.runtime.Interpreter
import cp.test.TestExtension
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class ExprTest extends AnyFunSuite with should.Matchers with TestExtension {
  
  given Interpreter = Interpreter(Prelude)
  
  test("primitive type Int") {
    "1" >>> (IntValue(1).toTerm, IntType.toType)
  }
  
  test("add two integers") {
    "1 + 2" >>> (IntValue(3).toTerm, IntType.toType)
  }
  
  test("concatenate two strings") {
    "\"Hello, \" ++ \"world!\"" >>> (
      PrimitiveValue.StringValue("Hello, world!").toTerm,
      PrimitiveType.StringType.toType
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

  test("multi-arg lambda application function call style") {
    "(fun (x: Int, y: Int) -> x + y)(20, 22)" >>> (
      IntValue(42).toTerm, IntType.toType
    )
  }

  test("multi-arg application partial application style") {
    val add = "fun (x: Int, y: Int) -> x + y"
    s"(fun (f: Int -> Int -> Int) -> f(20)(22))($add)" >>> (IntValue(42).toTerm, IntType.toType)
  }
  
  test("method style function call") {
    "114514.toString" >>> (
      PrimitiveValue.StringValue("114514").toTerm,
      PrimitiveType.StringType.toType
    )
    "(1 + 2).toString" >>> (
      PrimitiveValue.StringValue("3").toTerm,
      PrimitiveType.StringType.toType
    )
    "(\"Hello, \" ++ \"world!\").length" >>> (
      PrimitiveValue.IntValue(13).toTerm,
      PrimitiveType.IntType.toType
    )
    "(114.toString ++ 514.toString).length" >>> (
      PrimitiveValue.IntValue(6).toTerm,
      PrimitiveType.IntType.toType
    )
  }
}