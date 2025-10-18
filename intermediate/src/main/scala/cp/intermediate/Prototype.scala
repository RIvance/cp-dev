package cp.intermediate

trait Prototype[T] {
  def name: String
  def argTypes: List[T]
  def returnType: T
  def isPure: Boolean
  final def arity: Int = argTypes.length
}

trait CallablePrototype[T, V] extends Prototype[T] {
  def call(args: Seq[V]): V
}

trait NativePrototype[T] extends Prototype[T]

case class NativePrototypePure[T](
  override val name: String,
  override val argTypes: List[T],
  override val returnType: T
) extends NativePrototype[T] {
  override def isPure: Boolean = true
}

case class BuiltInFunction[T, V](
  override val name: String,
  override val argTypes: List[T],
  override val returnType: T,
  fn: Seq[V] => V,
) extends CallablePrototype[T, V] {
  override def isPure: Boolean = true
  
  def call(args: Seq[V]): V = {
    if args.length == argTypes.length then fn(args) else {
      throw new IllegalArgumentException(s"Expected ${argTypes.length} arguments, got ${args.length}")
    }
  }
}

case class NativePrototypeImpure[T](
  override val name: String,
  override val argTypes: List[T],
  override val returnType: T
) extends NativePrototype[T] {
  override def isPure: Boolean = false
}
