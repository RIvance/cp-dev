package cp.util

trait RecNamed {
  def contains(name: String): Boolean
}

trait RecIndexed {
  def contains(index: Int): Boolean
}
