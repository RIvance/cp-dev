package cp

import cp.core.{Literal, LiteralType, Term, Type}
import cp.core.Literal.*
import cp.core.LiteralType.*
import cp.test.TestExtension
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class DefinitionTest extends AnyFunSuite with should.Matchers with TestExtension  {
  
  test("synth term definition id 1") {
    module("""
      def id = Λ A . fun (x: A) -> x;
    """) { implicit env =>
      "id[Int](42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("synth term definition id 2") {
    module("""
        // Another style of definition
        def id[A](x: A) = x;
    """) { implicit env =>
      "id[Int](42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }
  
  test("synth fibonacci") {
    module("""
      def fib(n: Int): Int =
        if n <= 1 then n
        else fib(n - 1) + fib(n - 2);
    """) { implicit env =>
      "fib(10)" >>> (IntValue(55).toTerm, IntType.toType)
    }
  }

  test("synth simple type alias") {
    module("""
      type Integer = Int;
    """) { implicit env =>
      "(1 + 2 : Integer)" >>> (IntValue(3).toTerm, IntType.toType)
    }
  }

  test("synth simple record type") {
    module("""
      type Point = { x: Int; y: Int };

      def origin: Point = { x = 0; y = 0 };

      def move(p: Point, dx: Int, dy: Int): Point =
        { x = p.x + dx; y = p.y + dy };
    """) { implicit env =>
      "move(origin, 3, 4)" >>> (
        Term.Record(Map(
          "x" -> IntValue(3).toTerm,
          "y" -> IntValue(4).toTerm,
        )),
        Type.Record(Map(
          "x" -> IntType.toType, 
          "y" -> IntType.toType
        ))
      )
    }
  }

  test("synth simple type with type parameter") {
    module("""
      type Box[A] = { value: A };
      def box[A](x: A): Box[A] = { value = x };
      def unbox[A](b: Box[A]): A = b.value;
    """) { implicit env =>
      "unbox[Int](box[Int] 42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("synth type definition indexed") {
    module("""
      type Box[A] = ∀R . ((A -> R) -> R);
      def box[A](x: A): Box[A] = ΛR . fun (f: A -> R) -> f(x);
      def unbox[A](b: Box[A]): A = b[A](fun (x: A) -> x);
    """) { implicit env =>
      "unbox[Int](box[Int] 42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("synth type definition pair") {
    module("""
      type Pair[A, B] = ∀R . ((A -> B -> R) -> R);

      def pair[A, B](x: A, y: B): Pair[A, B] =
        Λ R . fun (f: A -> B -> R) -> f(x, y);

      def fst[A, B](p: Pair[A, B]): A =
        p[A](fun (x: A, y: B) -> x);

      def snd[A, B](p: Pair[A, B]): B =
        p[B](fun (x: A, y: B) -> y);
    """) { implicit env =>
      "fst[Int, String](pair[Int, String](42, \"Hello\"))" >>> (IntValue(42).toTerm, IntType.toType)
      "snd[Int, String](pair[Int, String](42, \"Hello\"))" >>> (StringValue("Hello").toTerm, StringType.toType)
      "let x = pair[Int, String](114514, \"good!\") in fst[Int, String] x" >>> (IntValue(114514).toTerm, IntType.toType)
    }
  }
  
  test("synth merge overloading") {
    module("""
      def doubleInt(value: Int): Int = value * 2;
      def doubleString(value: String): String = value ++ value;
      def double = doubleInt ,, doubleString;
    """) { implicit env =>
      "double(21)" >>> (IntValue(42).toTerm, IntType.toType)
      "double(\"ha\")" >>> (StringValue("haha").toTerm, StringType.toType)
    }
  }
  
//  test("synth merge") {
//    module("""
//      def double(value: Int) = value * 2;
//      def isHello(str: String) = if str == "hello" then true else false;
//    """) { implicit env =>
//      "(double ,, isHello)(42)" >>> (IntValue(84).toTerm, IntType.toType)
//      "(double ,, isHello)(\"hello\")" >>> (BoolValue(true).toTerm, BoolType.toType)
//      "(double ,, isHello)(\"world\")" >>> (BoolValue(false).toTerm, BoolType.toType)
//      "(double ,, isHello) (42 ,, \"hello\")" >>> (
//        Term.Merge(IntValue(84).toTerm, BoolValue(true).toTerm), 
//        Type.Intersection(IntType.toType, BoolType.toType)
//      )
//    }
//  }
//
//  test("synth coercion") {
//    module("""
//      f (x: Int) = false ,, x;
//    """) { implicit env =>
//      "(f : Bool & Int -> Bool & Int) (true ,, 42)" >>> (
//        Term.Merge(Term.Primitive(BoolValue(true)), Term.Primitive(IntValue(42))), 
//        Type.Intersection(BoolType.toType, IntType.toType)
//      )
//    }
//  }
}
