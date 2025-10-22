package cp.core

case class Namespace(path: List[String], isRelative: Boolean = false) {

  override def toString: String = (if isRelative then "this::" else "") + path.mkString("::")

  infix def ::(that: String): Namespace = Namespace(this.path :+ that, this.isRelative)

  def superNamespace: Namespace = if isRelative then {
    if path.isEmpty then Namespace("super" :: Nil, isRelative)
    else Namespace(path.init, isRelative)
  } else {
    if path.isEmpty then throw RuntimeException("Cannot get super namespace of root namespace")
    else Namespace(path.init, isRelative)
  }

  def join(that: String): Namespace = Namespace(this.path :+ that, this.isRelative)
  
  def qualified(localName: String): FullyQualifiedName = FullyQualifiedName(this, localName)
}

object Namespace {
  def apply(name: String): Namespace = Namespace(List(name))
}

case class FullyQualifiedName(namespace: Namespace, localName: String) {
  override def toString: String = s"${namespace}::${localName}"
}
