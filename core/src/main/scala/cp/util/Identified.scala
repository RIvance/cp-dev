package cp.util

trait Identified[T] {
  def contains(id: T): Boolean
}

trait IdentifiedByString extends Identified[String] {
  override def contains(name: String): Boolean = false
}

trait IdentifiedByIndex extends Identified[Int] {
  override def contains(index: Int): Boolean = false
}
