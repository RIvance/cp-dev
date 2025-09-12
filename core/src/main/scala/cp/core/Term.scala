package cp.core

import cp.core.Term.EvalMode

enum Term {
  case Var(name: String)
  case Primitive(value: Literal)
  case Apply(func: Term, arg: Term)
  case TypeApply(term: Term, tyArg: Type)
  case Lambda(param: String, paramType: Type, body: Term)
  case TypeLambda(param: String, body: Term)
  case Fixpoint(param: String, paramType: Type, body: Term)
  case Projection(record: Term, field: String)
  case Record(fields: Map[String, Term])
  case Tuple(elements: List[Term])
  case Merge(left: Term, right: Term, bias: MergeBias = MergeBias.Neutral)
  // case Match
  case ArrayLiteral(elements: List[Term])
  case FoldFixpoint(fixpointType: Type, body: Term)
  case UnfoldFixpoint(fixpointType: Type, term: Term)
  
  // `Do` term will ensure that `expr` is evaluated before `body`.
  //  It is useful when `expr` has side effects (e.g. native procedure calls).
  //  The value of `Do` term is the value of `body` and the value of `expr` is discarded.
  //  It also plays a role as a barrier to prevent unwanted beta-reduction.
  case Do(expr: Term, body: Term)
  
  // A reference to a memory/virtual address holding a value of given type.
  //   It can only be created/utilized by native procedures.
  case RefAddr(refType: Type, address: Long)
  
  // A call to a pure native function/procedure with given arguments.
  //  To support curried calls, the length of args can be less than
  //  the arity of the native function.
  // e.g. for a native function `add: (Int, Int) -> Int`,
  //  `PureNativeCall(add, [1])` represents a function of type `Int -> Int`.
  // Only when the number of args equals the arity,
  //  the call can be fully evaluated to a Primitive term.
  case NativeFunctionCall(function: NativeFunction, args: List[Term])
  // A native procedure is only evaluated in FULL eval mode.
  //  Similar to native function calls, it can be partially applied.
  case NativeProcedureCall(procedure: NativeProcedure, args: List[Term])

  def infer(using env: Environment): Type = ???

  def check(ty: Type)(using env: Environment): Boolean = ???

  def eval(mode: EvalMode = EvalMode.Normalize)(using env: Environment): Term = ???
  
  def normalize(using env: Environment = Environment.empty): Term = eval(EvalMode.Normalize)
}

object Term {

  enum EvalMode {
    case Normalize
    case Partial(unfoldLevel: Int = 3)
    case Full
  }

  case class EvalEnv(
    baseEnv: Environment,
    currentRecursionDepth: Int = 0,
    currentRecursionFunction: Option[String] = None
  ) {
    def typeVars: Map[String, Type] = baseEnv.typeVars
    def termVars: Map[String, Term] = baseEnv.termVars

    def addTypeVar(name: String, ty: Type): EvalEnv =
      copy(baseEnv = baseEnv.addTypeVar(name, ty))
    def addTermVar(name: String, term: Term): EvalEnv =
      copy(baseEnv = baseEnv.addTermVar(name, term))

    def withTypeVar[T](name: String, ty: Type)(f: EvalEnv => T): T =
      f(addTypeVar(name, ty))
    def withTermVar[T](name: String, term: Term)(f: EvalEnv => T): T =
      f(addTermVar(name, term))
  }
}

enum MergeBias {
  case Neutral, Left, Right
}

case class SelfAnnotation[T](
  name: String,
  ty: Option[T],
)
