package cp

import cp.core.{Environment, Literal, LiteralType, Term, Type}
import cp.core.LiteralType.*
import cp.core.Literal.*
import cp.prelude.Prelude
import cp.test.TestExtension
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class DefinitionTest extends AnyFunSuite with should.Matchers with TestExtension  {

  given prelude: Environment = Prelude.environment

  test("synth term definition id") {
    val code = """
      def id = Λ A . fun (x: A) -> x;
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term, ty) = synthExpr("id[Int](42)")
    ty should be (IntType.toType)
    term should be (IntValue(42).toTerm)
  }

  test("synth term definition id 2") {
    val code =
      """
        // Another style of definition
        def id[A](x: A) = x;
      """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term, ty) = synthExpr("id[Int](42)")
    ty should be(IntType.toType)
    term should be(IntValue(42).toTerm)
  }

  test("synth simple type alias") {
    val code ="""
      type Integer = Int;
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term, ty) = synthExpr("(1 + 2 : Integer)")
    ty should be(IntType.toType)
    term should be(IntValue(3).toTerm)
  }

  test("synth simple record type") {
    val code = """
      type Point = { x: Int; y: Int };

      def origin: Point = { x = 0; y = 0 };

      def move(p: Point, dx: Int, dy: Int): Point =
        { x = p.x + dx; y = p.y + dy };
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term, ty) = synthExpr("move(origin, 3, 4)")
    ty should be(Type.Record(Map("x" -> IntType.toType, "y" -> IntType.toType)))
    term should be(
      Term.Record(Map(
        "x" -> IntValue(3).toTerm,
        "y" -> IntValue(4).toTerm,
      ))
    )
  }

  test("synth simple type with type parameter") {
    val code = """
      type Box[A] = { value: A };

      def box[A](x: A): Box[A] = { value = x };

      def unbox[A](b: Box[A]): A = b.value;
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term, ty) = synthExpr("unbox[Int](box[Int](42))")
    ty should be(IntType.toType)
    term should be(IntValue(42).toTerm)
  }

  test("synth type definition pair") {
    val code = """
      type Pair[A, B] = ∀R . ((A -> B -> R) -> R);

      def pair[A, B](x: A, y: B): Pair[A, B] =
        Λ R . fun (f: A -> B -> R) -> f(x, y);

      def fst[A, B](p: Pair[A, B]): A =
        p[A](fun (x: A, y: B) -> x);

      def snd[A, B](p: Pair[A, B]): B =
        p[B](fun (x: A, y: B) -> y);
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term1, ty1) = synthExpr("fst[Int, String](pair[Int, String](42, \"Hello\"))")
    ty1.normalize should be(IntType.toType)
    term1.fullEval should be(IntValue(42).toTerm)
    val (term2, ty2) = synthExpr("snd[Int, String](pair[Int, String](42, \"Hello\"))")
    ty2.normalize should be(StringType.toType)
    term2.fullEval should be(StringValue("Hello").toTerm)
    
    val (term3, ty3) = synthExpr("let x = pair[Int, String](114514, \"good!\") in fst[Int, String] x")
    ty3.normalize should be(IntType.toType)
    term3.fullEval should be(IntValue(114514).toTerm)
  }
  
  test("synth merge") {
    val code = """
      def double(value: Int) = value * 2;
      def isHello(str: String) = if str == "hello" then true else false;
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term1, ty1) = synthExpr("(double ,, isHello) (42 ,, \"hello\")")
    ty1 should be (Type.Intersection(IntType.toType, BoolType.toType))
    term1 should be (Term.Merge(IntValue(84).toTerm, BoolValue(true).toTerm))
  }
  
  test("synth coercion") {
    val code = """
      f (x: Int) = false ,, x;
    """
    given newEnv: Environment = synthModule(code)(using prelude).toEnv
    val (term1, ty1) = synthExpr("(f : Bool & Int -> Bool & Int) (true ,, 42)")
  }
}
