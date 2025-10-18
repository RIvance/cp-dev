package cp.intermediate.builtin

import cp.core.NativeFunction
import cp.intermediate.{NativePrototype, NativePrototypeImpure, TypeValue as Type}
import cp.prelude.PreludeNativeImplementations

object BuiltinSymbols {
  val addInt = NativePrototypeImpure(
    name = "builtin$addInt",
    argTypes = List(Type.int, Type.int),
    returnType = Type.int
  )
  
  val addFloat = NativePrototypeImpure(
    name = "builtin$addFloat",
    argTypes = List(Type.float, Type.float),
    returnType = Type.float
  )
  
  val subInt = NativePrototypeImpure(
    name = "builtin$subInt",
    argTypes = List(Type.int, Type.int),
    returnType = Type.int
  )
  
  val subFloat = NativePrototypeImpure(
    name = "builtin$subFloat",
    argTypes = List(Type.float, Type.float),
    returnType = Type.float
  )
}

extension (nativeFunction: NativeFunction) {
  def toBuiltinPrototype: NativePrototype[Type] = nativeFunction match {
    case PreludeNativeImplementations.addInt => BuiltinSymbols.addInt
    case PreludeNativeImplementations.addFloat => BuiltinSymbols.addFloat
    case PreludeNativeImplementations.subInt => BuiltinSymbols.subInt
    case PreludeNativeImplementations.subFloat => BuiltinSymbols.subFloat
  }
}
