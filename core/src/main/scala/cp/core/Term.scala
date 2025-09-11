package cp.core

import cp.core.Term.EvalMode
import cp.syntax.{NeutralEffect, PureEffect}

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
//  case Match
  case ArrayLiteral(elements: List[Term])
  case FoldFixpoint(fixpointType: Type, body: Term)
  case UnfoldFixpoint(fixpointType: Type, term: Term)

  case Effective(effect: PureEffect[Term, Type], body: Term)
  case Neutral(neutral: NeutralEffect[Term, Type])

  def infer(using env: Environment): Type = ???

  def check(ty: Type)(using env: Environment): Boolean = ???

  def eval(mode: EvalMode)(using env: Environment): Term = ???
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
