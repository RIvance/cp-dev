package cp.core

enum PrimitiveValue {

  case UnitValue
  case BoolValue(value: Boolean)
  case IntValue(value: BigInt)
  case FloatValue(value: Double)
  case RuneValue(value: Char)
  case StringValue(value: String)

  override def toString: String = this match {
    case UnitValue => "unit"
    case BoolValue(value) => value.toString
    case IntValue(value) => value.toString
    case FloatValue(value) => value.toString
    case RuneValue(value) => s"'${value.toString}'"
    case StringValue(value) => s""""$value""""
  }

  def ty: PrimitiveType = this match {
    case UnitValue => PrimitiveType.UnitType
    case BoolValue(_) => PrimitiveType.BoolType
    case IntValue(_) => PrimitiveType.IntType
    case FloatValue(_) => PrimitiveType.FloatType
    case RuneValue(_) => PrimitiveType.RuneType
    case StringValue(_) => PrimitiveType.StringType
  }
  
  def toTerm: Term = Term.Primitive(this)
  def toValue: Value = Value.Primitive(this)
}

object PrimitiveValue {
  def unit: PrimitiveValue = UnitValue
  def trueValue: PrimitiveValue = BoolValue(true)
  def falseValue: PrimitiveValue = BoolValue(false)
}

enum PrimitiveType {

  case TopType
  case BottomType
  case UnitType
  case BoolType
  case IntType
  case FloatType
  case RuneType
  case StringType

  override def toString: String = this match {
    case TopType => "Any"
    case BottomType => "Nothing"
    case UnitType => "Unit"
    case BoolType => "Bool"
    case IntType => "Int"
    case FloatType => "Float"
    case RuneType => "Char"
    case StringType => "String"
  }
  
  def toType: Type = Type.Primitive(this)

}