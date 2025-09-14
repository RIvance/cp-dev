package cp.core

trait NativeCallable {
  def returnType: Type
  def paramTypes: Seq[Type]
  def arity: Int = paramTypes.length
  def call(args: Seq[Term])(using env: Environment): Term
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
      if inferredType != ty then {
        throw new IllegalArgumentException(s"Expected argument of type $ty, got $inferredType")
      }
    }
    implementation(args)
  }
}

class NativeFunction(
  returnType: Type,
  paramTypes: Seq[Type],
  implementation: Seq[Term] => Term,
) extends NativeCallableBase(returnType, paramTypes, implementation)

class NativeProcedure(
  returnType: Type,
  paramTypes: Seq[Type],
  implementation: Seq[Term] => Term,
) extends NativeCallableBase(returnType, paramTypes, implementation)
