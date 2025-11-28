package cp

import cp.core.PrimitiveType.*
import cp.core.PrimitiveValue.*
import cp.core.{PrimitiveType, PrimitiveValue, Term, Type}
import cp.test.TestExtension
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class DefinitionTest extends AnyFunSuite with should.Matchers with TestExtension  {

  test("term definition id 1") {
    module("""
      def id = Λ A . fun (x: A) -> x;
    """) { implicit interpreter =>
      "id[Int](42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("term definition id 2") {
    module("""
        // Another style of definition
        def id[A](x: A) = x;
    """) { implicit interpreter =>
      "id[Int](42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("fibonacci") {
    module("""
      def fib(n: Int): Int =
        if n <= 1 then n
        else fib(n - 1) + fib(n - 2);
    """) { implicit interpreter =>
      "fib(10)" >>> (IntValue(55).toTerm, IntType.toType)
    }
  }

  test("simple type alias") {
    module("""
      type Integer = Int;
    """) { implicit interpreter =>
      "(1 + 2 : Integer)" >>> (IntValue(3).toTerm, IntType.toType)
    }
  }

  test("simple record type") {
    module("""
      type Point = { x: Int; y: Int };

      def origin: Point = { x = 0; y = 0 };

      def move(p: Point, dx: Int, dy: Int): Point =
        { x = p.x + dx; y = p.y + dy };
    """) { implicit interpreter =>
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

  test("simple type with type parameter") {
    module("""
      type Box[A] = { value: A };
      def box[A](x: A): Box[A] = { value = x };
      def unbox[A](b: Box[A]): A = b.value;
    """) { implicit interpreter =>
      "unbox[Int](box[Int] 42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("type definition indexed") {
    module("""
      type Box[A] = ∀R . ((A -> R) -> R);
      def box[A](x: A): Box[A] = ΛR . fun (f: A -> R) -> f(x);
      def unbox[A](b: Box[A]): A = b[A](fun (x: A) -> x);
    """) { implicit interpreter =>
      "unbox[Int](box[Int] 42)" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("type definition pair") {
    module("""
      type Pair[A, B] = ∀R . ((A -> B -> R) -> R);

      def pair[A, B](x: A, y: B): Pair[A, B] =
        Λ R . fun (f: A -> B -> R) -> f(x, y);

      def fst[A, B](p: Pair[A, B]): A =
        p[A](fun (x: A, y: B) -> x);

      def snd[A, B](p: Pair[A, B]): B =
        p[B](fun (x: A, y: B) -> y);
    """) { implicit interpreter =>
      "fst[Int, String](pair[Int, String](42, \"Hello\"))" >>> (IntValue(42).toTerm, IntType.toType)
      "snd[Int, String](pair[Int, String](42, \"Hello\"))" >>> (StringValue("Hello").toTerm, StringType.toType)
      "let x = pair[Int, String](114514, \"good!\") in fst[Int, String] x" >>> (IntValue(114514).toTerm, IntType.toType)
    }
  }
  
  test("merge overloading") {
    module("""
      def doubleInt(value: Int): Int = value * 2;
      def doubleString(value: String): String = value ++ value;
      def double = doubleInt ,, doubleString;
    """) { implicit interpreter =>
      "double(21)" >>> (IntValue(42).toTerm, IntType.toType)
      "double(\"ha\")" >>> (StringValue("haha").toTerm, StringType.toType)
    }
  }
  
  test("merge") {
    module("""
      def double(value: Int) = value * 2;
      def isHello(str: String) = if str == "hello" then true else false;
    """) { implicit interpreter =>
      "(double ,, isHello) 42" >>> (IntValue(84).toTerm, IntType.toType)
      "(double ,, isHello) \"hello\"" >>> (BoolValue(true).toTerm, BoolType.toType)
      "(double ,, isHello) \"world\"" >>> (BoolValue(false).toTerm, BoolType.toType)
      "(double ,, isHello) (42 ,, \"hello\")" >>> (
        Term.Merge(IntValue(84).toTerm, BoolValue(true).toTerm),
        Type.Intersection(IntType.toType, BoolType.toType)
      )
    }
  }

  test("coercion") {
    module("""
      f (x: Int) = true ,, x;
    """) { implicit interpreter =>
      "(f : Bool & Int -> Bool & Int) (false ,, 42)" >>> (
        Term.Merge(Term.Primitive(BoolValue(true)), Term.Primitive(IntValue(42))),
        Type.Intersection(BoolType.toType, IntType.toType)
      )
    }
  }
  
  test("trait") {
    module("""
      type Editor = {
        onKey: String -> String;
        doCut: String;
        showHelp: String;
      };
      
      type Version = { version : String };
      
      editor = trait [self : Editor & Version] implements Editor => {
        onKey = fun (key: String) -> "Pressing " ++ key;
        doCut = self.onKey "C-x" ++ " for cutting text";
        showHelp = "Version " ++ self.version ++ ": usage...";
      };
      
      version = trait => { version = "0.1" };
    """) { implicit interpreter =>
      "(new editor ,, version).showHelp" >>> (
        StringValue("Version 0.1: usage...").toTerm,
        StringType.toType
      )
    }
  }

  test("trait mutual recursion") {
    module("""
      type Even = { isEven : Int -> Bool };
      type Odd = { isOdd : Int -> Bool };
      
      def even = trait [self: Odd] implements Even => {
        isEven (n : Int) = if n == 0 then true else self.isOdd(n - 1)
      };
      
      def odd = trait [self: Even] implements Odd => {
        isOdd (n : Int) = if n == 0 then false else self.isEven(n - 1)
      };
    """) { implicit interpreter =>
      "(new even ,, odd).isEven(42)" >>> (BoolValue(true).toTerm, BoolType.toType)
      "(new even ,, odd).isOdd(42)" >>> (BoolValue(false).toTerm, BoolType.toType)
      "(new even ,, odd).isEven(41)" >>> (BoolValue(false).toTerm, BoolType.toType)
      "(new even ,, odd).isOdd(41)" >>> (BoolValue(true).toTerm, BoolType.toType)
    }
  }

  test("trait with sort") {
    module("""
      type Eval = { eval : Int };
      type Print = { print : String };
      
      type AddSig<Exp> = {
        Lit(Int): out Exp;
        Add(Exp, Exp): out Exp;
      };
      
      def evalAdd = trait implements AddSig<Eval> => {
        (Lit     n).eval = n;
        (Add e1 e2).eval = e1.eval + e2.eval;
      };
      
      def printAdd = trait implements AddSig<Print> => {
        (Lit     n).print = n.toString;
        (Add e1 e2).print = "(" ++ e1.print ++ " + " ++ e2.print ++ ")";
      };
      
      type MulSig<Exp> = AddSig<Exp> & {
        Mul(Exp, Exp): Exp;
      };
      
      def evalMul = trait implements MulSig<Eval> inherits evalAdd => {
        (Mul e1 e2).eval = e1.eval * e2.eval;
      };
      
      def printMul = trait implements MulSig<Print> inherits printAdd => {
        (Mul e1 e2).print = "(" ++ e1.print ++ " * " ++ e2.print ++ ")";
      };
      
      def expAdd[Exp] = trait [self : AddSig<Exp>] => {
        exp = open self in Add(Lit 4, Lit 8);
      };
      
      def expMul[Exp] = trait [self : MulSig<Exp>] inherits expAdd[Exp] => {
        override exp = open self in Mul(super.exp, Lit 4);
      };
    """) { implicit interpreter =>
      """
        let e = new evalMul ,, printMul ,, expMul[Eval & Print] in
        e.exp.print ++ " is " ++ e.exp.eval.toString
      """ >>> StringValue("((4 + 8) * 4) is 48").toTerm
    }
  }

  test("pattern matching - isZero function") {
    module("""
      def isZero(n: Int): Bool = match n {
        case 0 => true;
        case _ => false;
      };
    """) { implicit interpreter =>
      "isZero(0)" >>> (BoolValue(true).toTerm, BoolType.toType)
      "isZero(42)" >>> (BoolValue(false).toTerm, BoolType.toType)
    }
  }

// TODO: We do not have grammar for tuple type yet
//  test("pattern matching - tuple swap") {
//    module("""
//      def swap[A, B](p: (A, B)): (B, A) = match p {
//        case (x, y) => ((y, x));
//      };
//    """) { implicit interpreter =>
//      "swap[Int, String](((1, \"hello\")))" >>> (
//        Term.Tuple(List(StringValue("hello").toTerm, IntValue(1).toTerm)),
//        Type.Tuple(List(StringType.toType, IntType.toType))
//      )
//    }
//  }

  test("pattern matching - option-like type") {
    module("""
      type Option[A] = { tag: String; value: A };
      
      def some[A](x: A): Option[A] = { tag = "some"; value = x };
      def none[A]: Option[A] = { tag = "none"; value = 0 };
      
      def getOrElse[A](opt: Option[A], default: A): A = match opt {
        case { tag = "some"; value = v } => v;
        case { tag = "none"; value = _ } => default;
      };
    """) { implicit interpreter =>
      "getOrElse[Int](some[Int](42), 0)" >>> (IntValue(42).toTerm, IntType.toType)
      "getOrElse[Int](none[Int], 99)" >>> (IntValue(99).toTerm, IntType.toType)
    }
  }

  test("pattern matching - record field shorthand") {
    module("""
      type Point = { x: Int; y: Int };
      
      def getX(p: Point): Int = match p {
        case { x = x; y = _ } => x;
      };
      
      def getY(p: Point): Int = match p {
        case { x = _; y = y } => y;
      };
    """) { implicit interpreter =>
      "getX({ x = 10; y = 20 })" >>> (IntValue(10).toTerm, IntType.toType)
      "getY({ x = 10; y = 20 })" >>> (IntValue(20).toTerm, IntType.toType)
    }
  }

  test("pattern matching - nested structures") {
    module("""
      type Nested = { outer: { inner: Int } };
      
      def getInner(n: Nested): Int = match n {
        case { outer = { inner = x } } => x;
      };
    """) { implicit interpreter =>
      "getInner({ outer = { inner = 42 } })" >>> (IntValue(42).toTerm, IntType.toType)
    }
  }

  test("pattern matching - factorial with pattern") {
    module("""
      def fac(n: Int): Int = match n {
        case 0 => 1;
        case m => m * fac(m - 1);
      };
    """) { implicit interpreter =>
      "fac(5)" >>> (IntValue(120).toTerm, IntType.toType)
      "fac(0)" >>> (IntValue(1).toTerm, IntType.toType)
    }
  }

  test("pattern matching - tuple of records") {
    module("""
      type Person = { name: String; age: Int };
      
      def comparePeople(p1: Person, p2: Person): String = match ((p1, p2)) {
        case ({ name = n1; age = a1 }, { name = n2; age = a2 }) => {
          if a1 > a2 then n1 ++ " is older"
          else if a2 > a1 then n2 ++ " is older"
          else "Same age"
        };
      };
    """) { implicit interpreter =>
      """comparePeople({ name = "Alice"; age = 30 }, { name = "Bob"; age = 25 })""" >>> (
        StringValue("Alice is older").toTerm,
        StringType.toType
      )
    }
  }
}
