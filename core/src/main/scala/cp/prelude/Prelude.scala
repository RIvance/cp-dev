package cp.prelude

import cp.core.{Environment, NativeFunction, NativeCallable, Term, Type}
import cp.core.Literal.*
import cp.core.LiteralType.*

object Prelude {
  lazy val environment: Environment = Environment.builder.buildWith { builder =>
    builder.termVar("+", NativeImplementations.add.toTerm)
    builder.termVar("-", NativeImplementations.sub.toTerm)
    builder.termVar("*", NativeImplementations.mul.toTerm)
    builder.termVar("++", NativeImplementations.concat.toTerm)
  }
  
  private object NativeImplementations {
    
    lazy val add: NativeFunction = NativeFunction(
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
    
    lazy val sub: NativeFunction = NativeFunction(
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
    
    lazy val mul: NativeFunction = NativeFunction(
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
