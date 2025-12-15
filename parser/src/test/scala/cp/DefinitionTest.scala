package cp

import cp.core.PrimitiveType.*
import cp.core.PrimitiveValue.*
import cp.core.{Module, PrimitiveType, PrimitiveValue, Term, Type}
import cp.interpreter.{DirectInterpreter, TrampolineInterpreter, WorkListInterpreter, Interpreter}
import cp.test.{InterpreterGroup, TestExtension}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class DefinitionTest extends AnyFunSuite with should.Matchers with TestExtension  {

  given (Seq[Module] => InterpreterGroup) = modules => InterpreterGroup(
    "Direct" -> DirectInterpreter(modules*),
    "Trampoline" -> TrampolineInterpreter(modules*),
    "WorkList" -> WorkListInterpreter(modules*)
  )

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
  
  test("attributed_s - synthesized attributes") {
    module("""
      type Eval = { eval : Int };
      type Print = { print : String };

      type ExpSig<Exp> = {
        Lit: Int -> Exp;
        Add: Exp -> Exp -> Exp;
      };

      def evalNum = trait implements ExpSig<Eval> => {
        (Lit     n).eval = n;
        (Add e1 e2).eval = e1.eval + e2.eval;
      };

      def printChild = trait implements ExpSig<Eval => Print> => {
        (Lit     n).print = n.toString;
        (Add e1 e2).print = if e2.eval == 0 then e1.print
                            else "(" ++ e1.print ++ " + " ++ e2.print ++ ")";
      };

      def expAdd[Exp] = trait [self : ExpSig<Exp>] => {
        exp = open self in Add (Lit 4) (Lit 8);
      };
    """) { implicit interpreter =>
      """
        let e = new evalNum ,, printChild ,, expAdd[Eval&Print] in
        e.exp.print ++ " is " ++ e.exp.eval.toString
      """ >>> StringValue("(4 + 8) is 12").toTerm
    }
  }

  test("coercion with merge") {
    module("""
      def f(x:Int) = false ,, x;
    """) { implicit interpreter =>
      "(f : Bool&Int -> Bool&Int) (true ,, 42)" >>> (
        Term.Merge(BoolValue(false).toTerm, IntValue(42).toTerm),
        Type.Intersection(BoolType.toType, IntType.toType)
      )
    }
  }

  test("conflict - trait composition with conflict resolution") {
    module("""
      def t1 = trait => { f = 1; g = "A" };
      def t2 = trait => { f = 2; g = "B" };

      def o1 = new t1 +# t2;
      def o2 = new t1 #+ t2;

      def t3 = trait [self] inherits t1\f ,, t2\g => {
        override f = super.f + (t1 ^ self).f;
      };
      def o3 = new t3;
    """) { implicit interpreter =>
      "o1.f.toString ++ o1.g ++ o2.f.toString ++ o2.g ++ o3.f.toString ++ o3.g" >>> (
        StringValue("1A2B3A").toTerm,
        StringType.toType
      )
    }
  }

  test("factorial - simple recursive") {
    module("""
      def fact(x:Int) : Int = if x == 0 then 1 else x * fact (x - 1);
    """) { implicit interpreter =>
      "fact 10" >>> (IntValue(3628800).toTerm, IntType.toType)
    }
  }

  test("fib - fibonacci") {
    module("""
      def fib(n:Int) : Int = if n <= 1 then n else fib (n-1) + fib (n-2);
    """) { implicit interpreter =>
      "fib 13" >>> (IntValue(233).toTerm, IntType.toType)
    }
  }

  test("inexact - mixin with constraint") {
    module("""
      def toUpperCase(s: String) = s;

      def mixin(TBase * { m: Int })(base: Trait<TBase>) =
        trait [this: TBase] inherits base => { m = 48 };

      def mkA = trait [this: { m: String; n: String }] => {
        m = "FOOBAR";
        n = toUpperCase this.m;
      };
    """) { implicit interpreter =>
      "(new mixin[{ m: String; n: String }] mkA).n" >>> (
        StringValue("FOOBAR").toTerm,
        StringType.toType
      )
    }
  }

  test("isorecursive - Church-style encoding") {
    module("""
      type ExpSig<Exp> = {
        Lit: Int -> Exp;
        Add: Exp -> Exp -> Exp;
      };

      interface Exp {
        eval: Int;
        dbl:  Exp;
        eq:   Exp -> Bool;
      };

      def exp = trait [self: ExpSig<Exp>] implements ExpSig<Exp> => {
        Lit n = trait => {
          eval  = n;
          dbl   = new self.Lit (n * 2);
          eq e' = e'.eval == n;
        };
        Add e1 e2 = trait => {
          eval  = e1.eval + e2.eval;
          dbl   = new self.Add e1.dbl e2.dbl;
          eq e' = e'.eval == e1.eval + e2.eval;
        };
      };

      def repo[Exp] = trait [self: ExpSig<Exp>] => {
        seven = new self.Lit 7;
        seven' = open self in Add (Lit 3) (Lit 4);
      };
    """) { implicit interpreter =>
      "(new repo[Exp] ,, exp).seven.dbl.eq (new repo[Exp] ,, exp).seven'.dbl" >>> (
        BoolValue(true).toTerm,
        BoolType.toType
      )
    }
  }

  test("merge - overloading") {
    module("""
      def f(x:Int) = x * 16;
      def g(x:Float) = if x > 0.0 then true else false;
    """) { implicit interpreter =>
      "(f ,, g) (3 ,, 1.5)" >>> (
        Term.Merge(IntValue(48).toTerm, BoolValue(true).toTerm),
        Type.Intersection(IntType.toType, BoolType.toType)
      )
    }
  }

  test("mixin - dynamic inheritance") {
    module("""
      type AddSig<Exp> = {
        Lit: Int -> Exp;
        Add: Exp -> Exp -> Exp;
      };

      type Eval = { eval: Int };

      def familyEval =
        trait implements AddSig<Eval> => {
          (Lit   n).eval = n;
          (Add l r).eval = l.eval + r.eval;
        };

      type Print = { print: String };

      def familyPrint =
        trait implements AddSig<Print> => {
          (Lit   n).print = n.toString;
          (Add l r).print = l.print ++ " + " ++ r.print;
        };

      type NegSig<Exp> = { Neg: Exp -> Exp };

      def familyNeg(TBase * NegSig<Eval&Print>)(base: Trait<TBase>) =
        trait [this: TBase] implements NegSig<Eval&Print> inherits base => {
          (Neg e).eval  = -e.eval;
          (Neg e).print = "-(" ++ e.print ++ ")";
        };
    """) { implicit interpreter =>
      """
        let fam = new familyNeg[AddSig<Eval&Print>] (familyEval,,familyPrint)
              : AddSig<Eval&Print> & NegSig<Eval&Print> in
        let e = new fam.Add (new fam.Lit 48) (new fam.Neg (new fam.Lit 2)) in
        e.print ++ " = " ++ e.eval.toString
      """ >>> StringValue("48 + -(2) = 46").toTerm
    }
  }

  test("monolithic - single large trait") {
    module("""
      type Mono = {
        eval : Int;
        plus : Int -> Int;
        print : String;
      };

      type LitSig<Exp> = {
        IntLit : Int -> Exp;
        StrLit : String -> Exp;
      };

      def eval = trait implements LitSig<Mono> => {
        (IntLit n).eval = n;
        (IntLit n).print = n.toString;
        (StrLit s).print = s;
        _.eval = 0;
        [self:Mono].plus m = self.eval + m;
      };

      def lit[Exp] = trait [self : LitSig<Exp>] => {
        int = new self.IntLit 48;
        str = new self.StrLit "HKG";
      };
    """) { implicit interpreter =>
      "(new eval ,, lit[Mono]).int.plus (-2)" >>> (
        IntValue(46).toTerm,
        IntType.toType
      )
    }
  }

  test("mutype - iso-recursive types") {
    module("""
      type Exp = fix Exp. {
        eval: Int;
        dbl:  Exp;
        eq:   Exp -> Bool;
      };

      def eval(e:Exp)         = (unfold[Exp] e).eval;
      def dbl(e:Exp)          = (unfold[Exp] e).dbl;
      def eq(e1:Exp)(e2:Exp) = (unfold[Exp] e1).eq e2;

      def lit(n:Int) : Exp = fold[Exp] {
        eval       = n;
        dbl        = lit (n * 2);
        eq (e:Exp) = eval e == n;
      };

      def add(e1:Exp)(e2:Exp) : Exp = fold[Exp] {
        eval       = eval e1 + eval e2;
        dbl        = add (dbl e1) (dbl e2);
        eq (e:Exp) = eval e == eval e1 + eval e2;
      };

      def seven = lit 7;
      def seven' = add (lit 3) (lit 4);
    """) { implicit interpreter =>
      "eq (dbl seven) (dbl seven)" >>> (BoolValue(true).toTerm, BoolType.toType)
    }
  }

  test("polymorphism - bounded polymorphism") {
    module("""
      def poly = Λ(T * Int). fun (x:T) -> x ,, 42;
    """) { implicit interpreter =>
      "poly[String & Bool] (\"∀\" ,, true)" >>> (
        Term.Merge(
          Term.Merge(StringValue("∀").toTerm, BoolValue(true).toTerm),
          IntValue(42).toTerm
        ),
        Type.Intersection(
          Type.Intersection(StringType.toType, BoolType.toType),
          IntType.toType
        )
      )
    }
  }

  test("refinement - type refinement") {
    module("""
      def f(n : Int) = { x = n; y = n + 1; z = n * 2 };
      def f' = f : Int -> { y : Int; z : Int };
    """) { implicit interpreter =>
      "(f' 10).z" >>> (IntValue(20).toTerm, IntType.toType)
    }
  }

  test("rename - field renaming") {
    module("""
      type X = { x : Int };
      def t = trait [self : X] => { x = 1; y = self.x };
      def o = new t [x <- z];
    """) { implicit interpreter =>
      "o.y + o.z" >>> (IntValue(2).toTerm, IntType.toType)
    }
  }

  test("scope - lexical scoping") {
    module("""
      def x = 1;
      def f[Dummy](y:Int) = x + y;
      def g(a: Unit) = let x = 2 in f[Unit] 0;
    """) { implicit interpreter =>
      "g ()" >>> (IntValue(1).toTerm, IntType.toType)
    }
  }

  test("sharing - structure sharing") {
    module("""
      type ExpSig<Exp> = {
        Lit : Int -> Exp;
        Add : Exp -> Exp -> Exp;
      };

      type Eval = { eval : Int };
      def eval = trait implements ExpSig<Eval> => {
        (Lit     n).eval = n;
        (Add e1 e2).eval = e1.eval + e2.eval;
      };

      type Dble[Exp] = { dble : Exp };
      def dble[Exp] = trait [self : ExpSig<Exp>] implements ExpSig<Dble Exp> => {
        (Lit     n).dble = new self.Lit (n*2);
        (Add e1 e2).dble = new self.Add(e1.dble, e2.dble);
      };

      def exp[Exp] = trait [self : ExpSig<Exp>] => {
        test = letrec tree (n:Int) : Exp =
          if n == 0 then new self.Lit 1
          else let shared = tree (n-1) in new self.Add shared shared
        in tree 20;
      };
    """) { implicit interpreter =>
      "(new exp[Eval & Dble Eval] ,, eval ,, dble[Eval]).test.dble.eval" >>> (
        IntValue(2097152).toTerm,
        IntType.toType
      )
    }
  }

//  test("tak - Takeuchi function") {
//    module("""
//      def tak(x:Int)(y:Int)(z:Int) : Int = if x <= y then y
//        else tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y);
//    """) { implicit interpreter =>
//      "tak 99 66 33" >>> (IntValue(99).toTerm, IntType.toType)
//    }
//  }

  test("fulltypeindex - full type indexing") {
    module("""
      type A = (Int -> Int&Bool) -> Int;
      type B = (Int -> Int&String) -> Int;
      def f(g : Int -> Int) = g 0;
      def f' = f : A & B;
      def f'' = f' : A;
    """) { implicit interpreter =>
      "f'' (fun (x:Int) -> x,,true)" >>> (IntValue(0).toTerm, IntType.toType)
    }
  }
}
