package cp.core

trait NativeCallable {
  def returnType: Type
  def argTypes: Seq[Type]
  def call(args: Seq[Term])(using env: Environment): Term
}

private sealed abstract class NativeCallableBase (
  override val returnType: Type,
  override val argTypes: Seq[Type],
  val implementation: Seq[Term] => Term,
) extends NativeCallable {
  override def call(args: Seq[Term])(using env: Environment): Term = {
    if args.length != argTypes.length then {
      throw new IllegalArgumentException(s"Expected ${argTypes.length} arguments, got ${args.length}")
    }
    // Verify argument types
    args.zip(argTypes).foreach { case (arg, ty) =>
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
  argTypes: Seq[Type],
  implementation: Seq[Term] => Term,
) extends NativeCallableBase(returnType, argTypes, implementation)

class NativeProcedure(
  returnType: Type,
  argTypes: Seq[Type],
  implementation: Seq[Term] => Term,
) extends NativeCallableBase(returnType, argTypes, implementation)
