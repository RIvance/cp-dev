package cp.intermediate

trait NativePrototype[T] {
  def name: String
  def argTypes: List[T]
  def returnType: T
  def isPure: Boolean
  final def arity: Int = argTypes.length
}

case class NativePrototypePure[T](
  override val name: String,
  override val argTypes: List[T],
  override val returnType: T
) extends NativePrototype[T] {
  override def isPure: Boolean = true
}

case class NativePrototypeEffective[T](
  override val name: String,
  override val argTypes: List[T],
  override val returnType: T
) extends NativePrototype[T] {
  override def isPure: Boolean = false
}
