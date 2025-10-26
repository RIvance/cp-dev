package cp.core

import cp.common.{CallablePrototype, Environment}
import cp.error.CoreErrorKind
import cp.error.CoreErrorKind.*

trait NativeCallable extends CallablePrototype[String, Type, Value]
object NativeCallable {
  type Env = Environment[String, Type, Value]
  enum Kind {
    case Default
    case Operator(symbol: String)
    case Function(name: String)
  }
}

private sealed abstract class NativeCallableBase (
  override val returnType: Type,
  override val paramTypes: Seq[Type],
  val implementation: Seq[Value] => Value,
) extends NativeCallable {
  override def call(args: Seq[Value])(using env: NativeCallable.Env): Value = {
    if args.length != paramTypes.length then {
      throw new IllegalArgumentException(s"Expected ${paramTypes.length} arguments, got ${args.length}")
    }
    // Verify and filter arguments
    val filteredArgs = args.zip(paramTypes).map { case (arg, ty) =>
      val inferredType = arg.infer
      if !(inferredType <:< ty) then TypeNotMatch.raise {
        s"Expected argument of type $ty, got $inferredType"
      } else arg.cast(ty) match {
        case Some(castedArg) => castedArg
        case None => TypeCastError.raise {
          s"Failed to cast argument of type $inferredType to $ty"
        }
      }
    }
    implementation(filteredArgs)
  }
}

class NativeFunction(
  returnType: Type,
  paramTypes: Seq[Type],
  implementation: Seq[Value] => Value,
  val kind: NativeCallable.Kind = NativeCallable.Kind.Default,
) extends NativeCallableBase(returnType, paramTypes, implementation) {
  def toTerm: Term = Term.NativeFunctionCall(this, Seq.empty[Term])
  override def isPure: Boolean = true
  override def name: String = toString
  override def toString: String = kind match {
    case NativeCallable.Kind.Operator(symbol) => symbol
    case NativeCallable.Kind.Function(name) => name
    case _ => s"native@${System.identityHashCode(this)}"
  }
}

class NativeProcedure(
  override val name: String,
  returnType: Type,
  paramTypes: Seq[Type],
  implementation: Seq[Value] => Value,
) extends NativeCallableBase(returnType, paramTypes, implementation) {
  override def isPure: Boolean = false
}
