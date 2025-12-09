package cp

import cp.core.PrimitiveType.*
import cp.core.PrimitiveValue.*
import cp.core.{PrimitiveType, PrimitiveValue, Term, Type}
import cp.interpreter.{DirectInterpreter, TrampolineInterpreter, WorkListInterpreter, Interpreter}
import cp.prelude.Prelude
import cp.test.{InterpreterGroup, TestExtension}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class ExprTest extends AnyFunSuite with should.Matchers with TestExtension {

  given InterpreterGroup = InterpreterGroup(
    "Direct" -> DirectInterpreter(Prelude),
    "Trampoline" -> TrampolineInterpreter(Prelude),
    "WorkList" -> WorkListInterpreter(Prelude)
  )

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

  test("non-arrow type fixpoint should be unfolded") {
    "fix a: Int. (((x: Int) -> 0) a)" >>> (
      IntValue(0).toTerm,
      PrimitiveType.IntType.toType
    )
  }

  test("trait unfolding") {
    "new trait [self : { a: Int; b: Int }] => { a = 42; b = self.a }" >>> (
      Term.Record(Map(
        "a" -> IntValue(42).toTerm,
        "b" -> IntValue(42).toTerm
      )),
      Type.Record(Map(
        "a" -> IntType.toType,
        "b" -> IntType.toType
      ))
    )
  }

  test("trait field override by merging") {
    "((fix self : { a: Int; b: Int } . { a = 1; b = self.a }) : { b : Int }) ,, { a = 2 }" >>> (
      Term.Record(Map(
        "a" -> IntValue(2).toTerm,
        "b" -> IntValue(1).toTerm
      )),
      Type.Record(Map(
        "a" -> IntType.toType,
        "b" -> IntType.toType
      ))
    )
  }

  test("pattern matching - primitive value") {
    """match 42 {
      case 0 => "zero";
      case 42 => "forty-two";
      case _ => "other";
    }""" >>> (StringValue("forty-two").toTerm, StringType.toType)
  }

  test("pattern matching - wildcard bind") {
    """match 100 {
      case x => x;
    }""" >>> (IntValue(100).toTerm, IntType.toType)
  }

  test("pattern matching - nested tuple") {
    """match ((((1, 2)), ((3, 4)))) {
      case ((a, b), (c, d)) => a + b + c + d;
    }""" >>> (IntValue(10).toTerm, IntType.toType)
  }

  test("pattern matching - record destructuring") {
    """match { x = 10; y = 20 } {
      case { x = a; y = b } => a + b;
    }""" >>> (IntValue(30).toTerm, IntType.toType)
  }

  test("pattern matching - mixed tuple and record") {
    """match ((1, { x = 2; y = 3 })) {
      case (n, { x = a; y = b }) => n + a + b;
    }""" >>> (IntValue(6).toTerm, IntType.toType)
  }

  test("pattern matching - multiple cases") {
    """match 1 {
      case 0 => "zero";
      case 1 => "one";
      case 2 => "two";
      case _ => "many";
    }""" >>> (StringValue("one").toTerm, StringType.toType)
  }

  test("pattern matching - string patterns") {
    """match "hello" {
      case "hello" => true;
      case _ => false;
    }""" >>> (BoolValue(true).toTerm, BoolType.toType)
  }
}