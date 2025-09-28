package cp.syntax

sealed trait RecordField {
  def isOverride: Boolean
  def contains(name: String): Boolean
  def subst(name: String, replacement: ExprTerm): RecordField
}

case class RecordFieldValue(
  value: ExprTerm,
  override val isOverride: Boolean = false
) extends RecordField {

  def contains(name: String): Boolean = value.contains(name)

  def subst(name: String, replacement: ExprTerm): RecordField = {
    RecordFieldValue(value.subst(name, replacement), isOverride)
  }

  override def toString: String = value.toString
}

case class RecordFieldMethod(
  fieldParams: Seq[(String, Option[ExprType])],
  methodName: String,
  methodBody: ExprTerm,
  override val isOverride: Boolean = false
) extends RecordField {

  def contains(name: String): Boolean = methodBody.contains(name)

  def subst(name: String, replacement: ExprTerm): RecordField = {
    RecordFieldMethod(fieldParams, methodName, methodBody.subst(name, replacement), isOverride)
  }

  override def toString: String = {
    val fieldParamsStr = fieldParams.map {
      case (name, Some(ty)) => s"$name: $ty"
      case (name, None) => name
    }.mkString(", ")
    s"($fieldParamsStr).$methodName = $methodBody"
  }
}
