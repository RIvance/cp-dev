package cp.prelude

import cp.core.{Environment, NativeFunction, NativeCallable, Term, Type}
import cp.core.Literal.*
import cp.core.LiteralType.*

object Prelude {
  import NativeImplementations.*
  
  lazy val environment: Environment = Environment.builder.buildWith { builder =>
    builder.termVar("+", overloaded(addInt, addFloat))
    builder.termVar("-", overloaded(subInt, subFloat))
    builder.termVar("*", overloaded(mulInt, mulFloat))
    builder.termVar("==", overloaded(eqInt, eqFloat, eqString))
    builder.termVar("++", concat.toTerm)
  }
  
  private def overloaded(implementations: NativeFunction*): Term = {
    implementations.map(_.toTerm).reduceLeft { (acc, fn) => Term.Merge(acc, fn) }
  }
  
  private object NativeImplementations {
    
    lazy val addInt: NativeFunction = NativeFunction(
      returnType = IntType.toType,
      paramTypes = Seq(IntType.toType, IntType.toType),
      kind = NativeCallable.Kind.Operator("+"),
      implementation = {
        case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
          IntValue(a + b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for +: $args")
      }
    )
    
    lazy val addFloat: NativeFunction = NativeFunction(
      returnType = FloatType.toType,
      paramTypes = Seq(FloatType.toType, FloatType.toType),
      kind = NativeCallable.Kind.Operator("+"),
      implementation = {
        case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
          FloatValue(a + b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for +: $args")
      }
    )
    
    lazy val subInt: NativeFunction = NativeFunction(
      returnType = IntType.toType,
      paramTypes = Seq(IntType.toType, IntType.toType),
      kind = NativeCallable.Kind.Operator("+"),
      implementation = {
        case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
          IntValue(a - b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for -: $args")
      }
    )
    
    lazy val subFloat: NativeFunction = NativeFunction(
      returnType = FloatType.toType,
      paramTypes = Seq(FloatType.toType, FloatType.toType),
      kind = NativeCallable.Kind.Operator("+"),
      implementation = {
        case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
          FloatValue(a - b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for -: $args")
      }
    )

    lazy val mulInt: NativeFunction = NativeFunction(
      returnType = IntType.toType,
      paramTypes = Seq(IntType.toType, IntType.toType),
      kind = NativeCallable.Kind.Operator("*"),
      implementation = {
        case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
          IntValue(a * b).toTerm
        case args =>
          throw new IllegalArgumentException(s"Invalid arguments for *: $args")
      }
    )
    
    lazy val mulFloat: NativeFunction = NativeFunction(
      returnType = FloatType.toType,
      paramTypes = Seq(FloatType.toType, FloatType.toType),
      kind = NativeCallable.Kind.Operator("*"),
      implementation = {
        case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
          FloatValue(a * b).toTerm
        case args =>
          throw new IllegalArgumentException(s"Invalid arguments for *: $args")
      }
    )
    
    lazy val concat: NativeFunction = NativeFunction(
      returnType = StringType.toType,
      paramTypes = Seq(StringType.toType, StringType.toType),
      kind = NativeCallable.Kind.Operator("++"),
      implementation = {
        case Seq(Term.Primitive(StringValue(a)), Term.Primitive(StringValue(b))) =>
          StringValue(a + b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for ++: $args")
      }
    )
    
    lazy val eqInt: NativeFunction = NativeFunction(
      returnType = BoolType.toType,
      paramTypes = Seq(IntType.toType, IntType.toType),
      kind = NativeCallable.Kind.Operator("=="),
      implementation = {
        case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
          BoolValue(a == b).toTerm
        case args =>
          throw new IllegalArgumentException(s"Invalid arguments for ==: $args")
      }
    )

    lazy val eqFloat: NativeFunction = NativeFunction(
      returnType = BoolType.toType,
      paramTypes = Seq(FloatType.toType, FloatType.toType),
      kind = NativeCallable.Kind.Operator("=="),
      implementation = {
        case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
          BoolValue(a == b).toTerm
        case args =>
          throw new IllegalArgumentException(s"Invalid arguments for ==: $args")
      }
    )

    lazy val eqString: NativeFunction = NativeFunction(
      returnType = BoolType.toType,
      paramTypes = Seq(StringType.toType, StringType.toType),
      kind = NativeCallable.Kind.Operator("=="),
      implementation = {
        case Seq(Term.Primitive(StringValue(a)), Term.Primitive(StringValue(b))) =>
          BoolValue(a == b).toTerm
        case args =>
          throw new IllegalArgumentException(s"Invalid arguments for ==: $args")
      }
    )
  }
}
