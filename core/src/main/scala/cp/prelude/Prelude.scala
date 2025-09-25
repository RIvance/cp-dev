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
    builder.termVar("<", overloaded(ltInt, ltFloat))
    builder.termVar("<=", overloaded(leInt, leFloat))
    builder.termVar(">", overloaded(gtInt, gtFloat))
    builder.termVar(">=", overloaded(geInt, geFloat))
    builder.termVar("&&", logicAnd.toTerm)
    builder.termVar("||", logicOr.toTerm)
    builder.termVar("!", logicNot.toTerm)
    builder.termVar("++", concatString.toTerm)
    builder.termVar("length", lengthString.toTerm)
    builder.termVar("toString", overloaded(toStringInt, toStringFloat, toStringBool))
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
          throw new IllegalArgumentException(s"Invalid arguments for `+`: $args")
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
          throw new IllegalArgumentException(s"Invalid arguments for `+`: $args")
      }
    )
    
    lazy val subInt: NativeFunction = NativeFunction(
      returnType = IntType.toType,
      paramTypes = Seq(IntType.toType, IntType.toType),
      kind = NativeCallable.Kind.Operator("-"),
      implementation = {
        case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
          IntValue(a - b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for `-`: $args")
      }
    )
    
    lazy val subFloat: NativeFunction = NativeFunction(
      returnType = FloatType.toType,
      paramTypes = Seq(FloatType.toType, FloatType.toType),
      kind = NativeCallable.Kind.Operator("-"),
      implementation = {
        case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
          FloatValue(a - b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for `-`: $args")
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
          throw new IllegalArgumentException(s"Invalid arguments for `*`: $args")
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
          throw new IllegalArgumentException(s"Invalid arguments for `*`: $args")
      }
    )
    
    lazy val concatString: NativeFunction = NativeFunction(
      returnType = StringType.toType,
      paramTypes = Seq(StringType.toType, StringType.toType),
      kind = NativeCallable.Kind.Operator("++"),
      implementation = {
        case Seq(Term.Primitive(StringValue(a)), Term.Primitive(StringValue(b))) =>
          StringValue(a + b).toTerm
        case args => 
          throw new IllegalArgumentException(s"Invalid arguments for `++`: $args")
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
          throw new IllegalArgumentException(s"Invalid arguments for `==`: $args")
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
          throw new IllegalArgumentException(s"Invalid arguments for `==`: $args")
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
          throw new IllegalArgumentException(s"Invalid arguments for `==`: $args")
      }
    )
  }
  
  lazy val logicAnd: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(BoolType.toType, BoolType.toType),
    kind = NativeCallable.Kind.Operator("&&"),
    implementation = {
      case Seq(Term.Primitive(BoolValue(a)), Term.Primitive(BoolValue(b))) =>
        BoolValue(a && b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `&&`: $args")
    }
  )
  
  lazy val logicOr: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(BoolType.toType, BoolType.toType),
    kind = NativeCallable.Kind.Operator("||"),
    implementation = {
      case Seq(Term.Primitive(BoolValue(a)), Term.Primitive(BoolValue(b))) =>
        BoolValue(a || b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `||`: $args")
    }
  )
  
  lazy val logicNot: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(BoolType.toType),
    kind = NativeCallable.Kind.Operator("!"),
    implementation = {
      case Seq(Term.Primitive(BoolValue(a))) =>
        BoolValue(!a).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `!`: $args")
    }
  )
  
  lazy val ltInt: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(IntType.toType, IntType.toType),
    kind = NativeCallable.Kind.Operator("<"),
    implementation = {
      case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
        BoolValue(a < b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `<`: $args")
    }
  )
  
  lazy val ltFloat: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(FloatType.toType, FloatType.toType),
    kind = NativeCallable.Kind.Operator("<"),
    implementation = {
      case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
        BoolValue(a < b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `<`: $args")
    }
  )
  
  lazy val leInt: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(IntType.toType, IntType.toType),
    kind = NativeCallable.Kind.Operator("<="),
    implementation = {
      case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
        BoolValue(a <= b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `<=`: $args")
    }
  )
  
  lazy val leFloat: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(FloatType.toType, FloatType.toType),
    kind = NativeCallable.Kind.Operator("<="),
    implementation = {
      case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
        BoolValue(a <= b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `<=`: $args")
    }
  )
  
  lazy val gtInt: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(IntType.toType, IntType.toType),
    kind = NativeCallable.Kind.Operator(">"),
    implementation = {
      case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
        BoolValue(a > b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `>`: $args")
    }
  )
  
  lazy val gtFloat: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(FloatType.toType, FloatType.toType),
    kind = NativeCallable.Kind.Operator(">"),
    implementation = {
      case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
        BoolValue(a > b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `>`: $args")
    }
  )
  
  lazy val geInt: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(IntType.toType, IntType.toType),
    kind = NativeCallable.Kind.Operator(">="),
    implementation = {
      case Seq(Term.Primitive(IntValue(a)), Term.Primitive(IntValue(b))) =>
        BoolValue(a >= b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `>=`: $args")
    }
  )
  
  lazy val geFloat: NativeFunction = NativeFunction(
    returnType = BoolType.toType,
    paramTypes = Seq(FloatType.toType, FloatType.toType),
    kind = NativeCallable.Kind.Operator(">="),
    implementation = {
      case Seq(Term.Primitive(FloatValue(a)), Term.Primitive(FloatValue(b))) =>
        BoolValue(a >= b).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments for `>=`: $args")
    }
  )
  
  // Functions
  
  lazy val lengthString: NativeFunction = NativeFunction(
    returnType = IntType.toType,
    paramTypes = Seq(StringType.toType),
    kind = NativeCallable.Kind.Function("length"),
    implementation = {
      case Seq(Term.Primitive(StringValue(s))) =>
        IntValue(s.length).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments: $args")
    }
  )
  
  lazy val toStringInt: NativeFunction = NativeFunction(
    returnType = StringType.toType,
    paramTypes = Seq(IntType.toType),
    kind = NativeCallable.Kind.Function("toString"),
    implementation = {
      case Seq(Term.Primitive(IntValue(i))) =>
        StringValue(i.toString).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments: $args")
    }
  )
  
  lazy val toStringFloat: NativeFunction = NativeFunction(
    returnType = StringType.toType,
    paramTypes = Seq(FloatType.toType),
    kind = NativeCallable.Kind.Function("toString"),
    implementation = {
      case Seq(Term.Primitive(FloatValue(f))) =>
        StringValue(f.toString).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments: $args")
    }
  )
  
  lazy val toStringBool: NativeFunction = NativeFunction(
    returnType = StringType.toType,
    paramTypes = Seq(BoolType.toType),
    kind = NativeCallable.Kind.Function("toString"),
    implementation = {
      case Seq(Term.Primitive(BoolValue(b))) =>
        StringValue(b.toString).toTerm
      case args =>
        throw new IllegalArgumentException(s"Invalid arguments: $args")
    }
  )
}
