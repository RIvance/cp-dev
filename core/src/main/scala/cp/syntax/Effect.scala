package cp.syntax

enum PureEffect[V, T] {
  case Deref(reference: V)
  case RefAssign(reference: V, value: V)
  case Exec(expr: V)
}

enum NeutralEffect[V, T] {
  case InitRef(reference: V)
  case NativeCall(nativeFunction: NativeFunction[T], args: Seq[V])
}

trait NativeFunction[T] {
  def name: String
  def returnType: T
  def argTypes: Seq[T]
}
