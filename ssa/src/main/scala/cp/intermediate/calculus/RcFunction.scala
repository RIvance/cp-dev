package cp.intermediate.calculus

import cp.intermediate.{CallablePrototype, TypeValue}

case class RcFunction(
  name: String,
  argTypes: List[TypeValue],
  returnType: TypeValue,
  body: RcTerm,
  isPure: Boolean,
) extends CallablePrototype[TypeValue, RcTerm] {
  override def call(args: Seq[RcTerm]): RcTerm = {
    if args.length == argTypes.length then {
      // TODO: Implement function application logic
      throw new NotImplementedError("Function application is not implemented yet")
    } else {
      throw new IllegalArgumentException(s"Expected ${argTypes.length} arguments, got ${args.length}")
    }
  }
}
