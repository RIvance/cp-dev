package cp.core

import cp.error.CoreErrorKind
import cp.error.CoreErrorKind.*

trait NativeCallable {
  def returnType: Type
  def paramTypes: Seq[Type]
  def arity: Int = paramTypes.length
  def call(args: Seq[Term])(using env: Environment): Term
}

object NativeCallable {
  enum Kind {
    case Default
    case Operator(symbol: String)
    case Function(name: String)
  }
}

private sealed abstract class NativeCallableBase (
  override val returnType: Type,
  override val paramTypes: Seq[Type],
  val implementation: Seq[Term] => Term,
) extends NativeCallable {
  override def call(args: Seq[Term])(using env: Environment): Term = {
    if args.length != paramTypes.length then {
      throw new IllegalArgumentException(s"Expected ${paramTypes.length} arguments, got ${args.length}")
    }
    // Verify argument types
    args.zip(paramTypes).foreach { case (arg, ty) =>
      val inferredType = arg.infer
      if !(inferredType <:< ty) then TypeNotMatch.raise {
        s"Expected argument of type $ty, got $inferredType"
      }
    }
    implementation(args)
  }
}

class NativeFunction(
  returnType: Type,
  paramTypes: Seq[Type],
  implementation: Seq[Term] => Term,
  val kind: NativeCallable.Kind = NativeCallable.Kind.Default,
) extends NativeCallableBase(returnType, paramTypes, implementation) {
  def toTerm: Term = Term.NativeFunctionCall(this, Seq.empty[Term])
  override def toString: String = kind match {
    case NativeCallable.Kind.Operator(symbol) => symbol
    case NativeCallable.Kind.Function(name) => name
    case _ => s"native@${System.identityHashCode(this)}"
  }
}

class NativeProcedure(
  returnType: Type,
  paramTypes: Seq[Type],
  implementation: Seq[Term] => Term,
) extends NativeCallableBase(returnType, paramTypes, implementation)
