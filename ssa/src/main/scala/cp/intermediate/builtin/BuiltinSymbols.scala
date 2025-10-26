package cp.intermediate.builtin

import cp.core.NativeFunction
import cp.core.PrimitiveValue.*
import cp.intermediate.calculus.RcTerm
import cp.intermediate.{BuiltInFunction, Prototype, NativePrototypeImpure, TypeValue as Type}
import cp.prelude.PreludeNativeImplementations

object BuiltinSymbols {
  val addInt = BuiltInFunction(
    name = "builtin$addInt",
    argTypes = List(Type.int, Type.int),
    returnType = Type.int,
    fn = {
      case Seq(RcTerm.Primitive(IntValue(a)), RcTerm.Primitive(IntValue(b))) =>
        RcTerm.Primitive(IntValue(a + b))
      case _ => throw new IllegalArgumentException("Invalid arguments for addInt")
    }
  )
  
  val addFloat = BuiltInFunction(
    name = "builtin$addFloat",
    argTypes = List(Type.float, Type.float),
    returnType = Type.float,
    fn = {
      case Seq(RcTerm.Primitive(FloatValue(a)), RcTerm.Primitive(FloatValue(b))) =>
        RcTerm.Primitive(FloatValue(a + b))
      case _ => throw new IllegalArgumentException("Invalid arguments for addFloat")
    }
  )
  
  val subInt = BuiltInFunction(
    name = "builtin$subInt",
    argTypes = List(Type.int, Type.int),
    returnType = Type.int,
    fn = {
      case Seq(RcTerm.Primitive(IntValue(a)), RcTerm.Primitive(IntValue(b))) =>
        RcTerm.Primitive(IntValue(a - b))
      case _ => throw new IllegalArgumentException("Invalid arguments for subInt")
    }
  )
  
  val subFloat = BuiltInFunction(
    name = "builtin$subFloat",
    argTypes = List(Type.float, Type.float),
    returnType = Type.float,
    fn = {
      case Seq(RcTerm.Primitive(FloatValue(a)), RcTerm.Primitive(FloatValue(b))) =>
        RcTerm.Primitive(FloatValue(a - b))
      case _ => throw new IllegalArgumentException("Invalid arguments for subFloat")
    }
  )
}

extension (nativeFunction: NativeFunction) {
  def toBuiltinPrototype: Prototype[Type] = nativeFunction match {
    case PreludeNativeImplementations.addInt => BuiltinSymbols.addInt
    case PreludeNativeImplementations.addFloat => BuiltinSymbols.addFloat
    case PreludeNativeImplementations.subInt => BuiltinSymbols.subInt
    case PreludeNativeImplementations.subFloat => BuiltinSymbols.subFloat
  }
}
