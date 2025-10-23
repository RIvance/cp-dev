package cp.core

import cp.common.Environment
import cp.error.CoreErrorKind.*
import cp.util.{Graph, IdentifiedByString, Recoverable}

import scala.util.{Failure, Success}

enum Term extends IdentifiedByString {

  type Env = Environment[String, Type, Term]
  
  case Var(name: String)
  
  // Symbols from external modules
  case Symbol(name: FullyQualifiedName, ty: Type)

  case Typed(term: Term, ty: Type)
  
  case Primitive(value: PrimitiveValue)
  
  case Apply(func: Term, arg: Term)
  
  case TypeApply(term: Term, typeArg: Type)
  
  case Lambda(param: String, paramType: Type, body: Term, isCoe: Boolean = false)
  
//  case Coercion(param: String, paramType: Type, body: Term)
  
  case TypeLambda(param: String, body: Term)
  
  case Fixpoint(name: String, ty: Type, recursiveBody: Term)
  
  case Projection(record: Term, field: String)
  
  case Record(fields: Map[String, Term])
  
  case Tuple(elements: List[Term])

  case Merge(left: Term, right: Term, bias: MergeBias = MergeBias.Neutral)

  case Diff(left: Term, right: Term)
  
  case IfThenElse(condition: Term, thenBranch: Term, elseBranch: Term)
  
  // case Match(scrutinee: Term, clauses: List[Clause])
  
  case ArrayLiteral(elements: List[Term])
  
  case FoldFixpoint(fixpointType: Type, body: Term)
  
  case UnfoldFixpoint(fixpointType: Type, term: Term)

  // `Do` term will ensure that `expr` is evaluated before `body`.
  //  It is useful when `expr` has side effects (e.g. native procedure calls).
  //  The value of `Do` term is the value of `body` and the value of `expr` is discarded.
  //  It also plays a role as a barrier to prevent unwanted beta-reduction.
  case Do(expr: Term, body: Term)

  // A reference to a memory/virtual address holding a value of given type.
  //  It can only be created/utilized by native procedures.
  case RefAddr(refType: Type, address: Long)
  
  // A call to a pure native function/procedure with given arguments.
  //  To support curried calls, the length of args can be less than
  //  the arity of the native function.
  // e.g. for a native function `add: (Int, Int) -> Int`,
  //  `PureNativeCall(add, [1])` represents a function of type `Int -> Int`.
  // Only when the number of args equals the arity,
  //  the call can be fully evaluated to a Primitive term.
  case NativeFunctionCall(function: NativeFunction, args: Seq[Term])
  
  // A native procedure is only evaluated in FULL eval mode.
  //  Similar to native function calls, it can be partially applied.
  case NativeProcedureCall(procedure: NativeProcedure, args: Seq[Term])

  def infer(using env: Env): Type = {
    val inferredType = this match {

      case Var(name) => env.values.get(name) match {
        case Some(term) => term.infer
        case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
      }

      case Symbol(_, ty) => ty

      case Typed(_, ty) => ty

      case Primitive(value) => Type.Primitive(value.ty)

      case Apply(func, arg) => func.infer match {
        case Type.Arrow(paramType, returnType, _) =>
          val argType: Type = arg.infer
          if !(argType <:< paramType) then TypeNotMatch.raise {
            s"Expected argument type: ${paramType}, but got: ${argType}"
          } else returnType
        case fnType@Type.Intersection(_, _) => 
          val argType = arg.infer
          fnType.testApplicationReturn(argType) match {
            case Some(returnType) => returnType
            case None => TypeNotMatch.raise {
              s"Function type ${fnType} cannot be applied to argument type ${argType}"
            }
          }
        case other => TypeNotMatch.raise(s"Expected function type, but got: ${other}")
      }

      case TypeApply(term, tyArg) => term.infer match {
        case Type.Forall(param, body, _) => body.subst(param, tyArg)
        case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
      }

      case Lambda(param, paramType, body, isCoe) => {
        env.withValueVar(param, Term.Typed(Term.Var(param), paramType)) {
          implicit newEnv => Type.Arrow(paramType, body.infer(using newEnv), isCoe)
        }
      }

      case TypeLambda(param, body) => {
        env.withTypeVar(param, Type.Var(param)) {
          implicit newEnv => Type.Forall(param, body.infer(using newEnv))
        }
      }

      case Fixpoint(name, ty, body) => {
        env.withValueVar(name, Term.Typed(Term.Var(name), ty)) { implicit newEnv =>
          val bodyType = body.infer(using newEnv)
          if !(bodyType <:< ty) then TypeNotMatch.raise {
            s"Body type `${bodyType}` does not match annotated type `${ty}` in fixpoint"
          } else bodyType
        }
      }

      case Projection(record, field) => record.infer match {
        case Type.Record(fieldTypes) => fieldTypes.get(field) match {
          case Some(fieldType) => fieldType
          case None => NoSuchField.raise {
            s"Field '${field}' does not exist in record type ${record.infer}"
          }
        }
        case other => TypeNotMatch.raise {
          s"Expected record type, but got: ${other}"
        }
      }

      case Record(fields) => {
        if fields.isEmpty then Type.unit
        else Type.Record(fields.map { (name, term) => (name, term.infer) })
      }

      case Tuple(elements) => {
        if elements.isEmpty then Type.unit
        else Type.Tuple(elements.map(_.infer))
      }

      case Merge(left, right, MergeBias.Left) => {
        Merge(left, Term.Diff(right, left), MergeBias.Neutral).infer
      }

      case Merge(left, right, MergeBias.Right) => {
        Merge(Term.Diff(left, right), right, MergeBias.Neutral).infer
      }

      case Merge(left, right, MergeBias.Neutral) => left.infer merge right.infer

      case Diff(left, right) => left.infer diff right.infer

      case IfThenElse(_, thenBranch, elseBranch) => {
        val thenType = thenBranch.infer
        val elseType = elseBranch.infer
        if thenType == elseType then thenType
        else if thenType <:< elseType then elseType
        else if elseType <:< thenType then thenType
        else TypeNotMatch.raise {
          s"Branches of IfThenElse have incompatible types: ${thenType} and ${elseType}"
        }
      }

      case ArrayLiteral(_) => ???
      case FoldFixpoint(_, _) => ???
      case UnfoldFixpoint(_, _) => ???
      case Do(_, _) => ???
      case RefAddr(_, _) => ???

      case NativeFunctionCall(function, args) => {
        if args.length > function.arity then TypeNotMatch.raise {
          s"Too many arguments for native function: expected at most ${function.arity}, got ${args.length}"
        }
        val paramTypes = function.paramTypes
        args.zip(paramTypes).foreach { (arg, paramType) =>
          val argType = arg.infer
          if !(argType <:< paramType) then TypeNotMatch.raise {
            s"Expected argument type: ${paramType}, but got: ${argType}"
          }
        }
        if args.length == function.arity then {
          function.returnType
        } else {
          paramTypes.drop(args.length).foldRight(function.returnType) {
            (paramType, acc) => Type.Arrow(paramType, acc)
          }
        }
      }

      case NativeProcedureCall(_, _) => ???
    }

    inferredType.normalize
  }

  def check(expectedType: Type)(using env: Env): Boolean = this.infer <:< expectedType

  /**
   * Evaluate the term according to the given evaluation mode.
   * @param mode
   *  EvalMode.Normalize: only perform terminating reductions (e.g. beta-reduction, unfolding fixpoints once).
   *    Do not evaluate expressions with side effects (e.g. native procedure calls).
   *  EvalMode.Full: perform full reductions until no more reductions are possible.
   *    Evaluate expressions with side effects (e.g. native procedure calls).
   * @param env the environment to use for variable lookups
   * @return the evaluated term
   */
  def eval(using env: Env = Environment.empty[String, Type, Term])(
    using mode: EvalMode = EvalMode.Normalize
  ): Term = ??? // This will be moved to a separate interpreter class.
  
  def new_(env: Env, traitType: Type): (Term, Type) = {
    given Env = env
    traitType.normalize match {
      case Type.Arrow(domain, codomain, isTrait) if isTrait => {
        if codomain <:< domain then {
          Term.Fixpoint("$self", domain, Term.Apply(this, Term.Var("$self"))) -> codomain
        } else TypeNotMatch.raise {
          s"Trait codomain $codomain is not a subtype of its domain $domain"
        }
      }
      case _ => TypeNotMatch.raise(s"`new` expression must be of a trait type, but got: $traitType")
    }
  }
  
  def new_(using env: Env): Term = {
    val traitType = this.infer
    this.new_(env, traitType)._1
  }

  def mapSubterms(f: Term => Term): Term = this match {
    case Var(_) | Symbol(_, _) | Primitive(_) => this
    case Typed(term, ty) => Typed(f(term), ty)
    case Apply(func, arg) => Apply(f(func), f(arg))
    case TypeApply(term, tyArg) => TypeApply(f(term), tyArg)
    case Lambda(param, paramType, body, isCoe) => Lambda(param, paramType, f(body), isCoe)
    case TypeLambda(param, body) => TypeLambda(param, f(body))
    case Fixpoint(name, ty, recursiveBody) => Fixpoint(name, ty, f(recursiveBody))
    case Projection(record, field) => Projection(f(record), field)
    case Record(fields) => Record(fields.map { (name, term) => (name, f(term)) })
    case Tuple(elements) => Tuple(elements.map(f))
    case Merge(left, right, bias) => Merge(f(left), f(right), bias)
    case Diff(left, right) => Diff(f(left), f(right))
    case IfThenElse(condition, thenBranch, elseBranch) =>
      IfThenElse(f(condition), f(thenBranch), f(elseBranch))
    // case Match(scrutinee, clauses) => Match(f(scrutinee), clauses.map(_.mapSubterms(f)))
    case ArrayLiteral(elements) => ArrayLiteral(elements.map(f))
    case FoldFixpoint(fixpointType, body) => FoldFixpoint(fixpointType, f(body))
    case UnfoldFixpoint(fixpointType, term) => UnfoldFixpoint(fixpointType, f(term))
    case Do(expr, body) => Do(f(expr), f(body))
    case RefAddr(_, _) => this
    case NativeFunctionCall(function, args) => NativeFunctionCall(function, args.map(f))
    case NativeProcedureCall(procedure, args) => NativeProcedureCall(procedure, args.map(f))
  }

  def mapTypes(f: Type => Type): Term = this match {
    case Var(_) | Primitive(_) => this
    case Symbol(name, ty) => Symbol(name, f(ty))
    case Typed(term, ty) => Typed(term.mapTypes(f), f(ty))
    case Apply(func, arg) => Apply(func.mapTypes(f), arg.mapTypes(f))
    case TypeApply(term, tyArg) => TypeApply(term.mapTypes(f), f(tyArg))
    case Lambda(param, paramType, body, isCoe) =>
      Lambda(param, f(paramType), body.mapTypes(f), isCoe)
    case TypeLambda(param, body) => TypeLambda(param, body.mapTypes(f))
    case Fixpoint(name, ty, recursiveBody) =>
      Fixpoint(name, f(ty), recursiveBody.mapTypes(f))
    case Projection(record, field) => Projection(record.mapTypes(f), field)
    case Record(fields) => Record(fields.map { (name, term) => (name, term.mapTypes(f)) })
    case Tuple(elements) => Tuple(elements.map(_.mapTypes(f)))
    case Merge(left, right, bias) =>
      Merge(left.mapTypes(f), right.mapTypes(f), bias)
    case Diff(left, right) => Diff(left.mapTypes(f), right.mapTypes(f))
    case IfThenElse(condition, thenBranch, elseBranch) =>
      IfThenElse(
        condition.mapTypes(f),
        thenBranch.mapTypes(f),
        elseBranch.mapTypes(f)
      )
    // case Match(scrutinee, clauses) =>
    //   Match(scrutinee.mapTypes(f), clauses.map(_.mapTypes(f)))
    case ArrayLiteral(elements) => ArrayLiteral(elements.map(_.mapTypes(f)))
    case FoldFixpoint(fixpointType, body) =>
      FoldFixpoint(f(fixpointType), body.mapTypes(f))
    case UnfoldFixpoint(fixpointType, term) =>
      UnfoldFixpoint(f(fixpointType), term.mapTypes(f))
    case Do(expr, body) => Do(expr.mapTypes(f), body.mapTypes(f))
    case RefAddr(refType, address) => RefAddr(f(refType), address)
    case NativeFunctionCall(function, args) =>
      NativeFunctionCall(function, args.map(_.mapTypes(f)))
    case NativeProcedureCall(procedure, args) =>
      NativeProcedureCall(procedure, args.map(_.mapTypes(f)))
  }

  def normalizeTypes(using env: Env): Term = this.mapTypes(_.normalize(using env))
  
  private def isValue(allowedParams: Set[String] = Set.empty): Boolean = this match {
    case Var(name) => allowedParams.contains(name)
    case Symbol(_, _) => false
    case Primitive(_) => true
    case Lambda(param, _, body, _) => body.isValue(allowedParams + param)
    case Fixpoint(name, _, body) => body.isValue(allowedParams + name)
    case TypeLambda(_, body) => body.isValue(allowedParams)
    case Typed(term, _) => term.isValue(allowedParams)
    case TypeApply(term, _) => term.isValue(allowedParams)
    case Record(fields) => fields.values.forall(_.isValue(allowedParams))
    case Tuple(elements) => elements.forall(_.isValue(allowedParams))
    case _ => false
  }
  
  def isValue: Boolean = this.isValue(Set.empty)

  def filter(expectedType: Type)(using env: Env): Term = this match {

    // For records, only keep the fields that are present in the expected type,
    // TODO: the current implementation drops fields that are not in the expected type
    //  However, in some cases we may want to keep those fields because they are referenced
    //  from fields that are in the expected type.
    //  For example, consider the following record: `{ a = 42; b = a + 1; c = "hello" }`
    //  If the expected type is `{ b: Int }`, we should keep field `a` as well,
    //  otherwise field `b` will be ill-typed after filtering.
    //  To solve this problem, we need to perform a reachability analysis on the record fields
    //  starting from the fields in the expected type.
    //  This is simple for record that do not contain mutual recursion,
    //  but may be tricky for records that contain mutual recursion.
    //  We leave this as future work for now.
    case Record(fields) => {
      val expectedFieldTypes = expectedType.normalize match {
        case Type.Record(fieldTypes) => fieldTypes
        case other => TypeNotMatch.raise(s"Expected record type, but got: ${other}")
      }
      val filteredFields = fields.flatMap { (name, term) =>
        expectedFieldTypes.get(name) match {
          case Some(expectedFieldType) => Some((name, term.filter(expectedFieldType)))
          case None => None // Field not in expected type, drop it.
        }
      }
      Term.Record(filteredFields)
    }

    // case Fixpoint(name, ty, Record(fields)) => {
    //   val expectedFieldTypes = expectedType.normalize match {
    //     case Type.Record(fieldTypes) => fieldTypes
    //     case other => TypeNotMatch.raise(s"Expected record type, but got: ${other}")
    //   }
    //   val filteredFields = fields.flatMap { (name, term) =>
    //     expectedFieldTypes.get(name) match {
    //       case Some(expectedFieldType) => Some((name, term.filter(expectedFieldType)))
    //       case None => None // Field not in expected type, drop it.
    //     }
    //   }
    //   Term.Fixpoint(name, ty, Term.Record(filteredFields))
    // }
    
    // For merged terms, filter out the branches that do not conform to the expected type.
    case Merge(left, right, MergeBias.Neutral) => {
      val leftType = left.infer
      val rightType = right.infer
      (leftType <:< expectedType, rightType <:< expectedType) match {
        case (true, true) => Term.Merge(left.filter(expectedType), right.filter(expectedType), MergeBias.Neutral)
        case (true, false) => left.filter(expectedType)
        case (false, true) => right.filter(expectedType)
        case (false, false) => Term.Merge(left, right, MergeBias.Neutral)
      }
    }
    
    case Merge(left, right, MergeBias.Left) => {
      Term.Merge(left.filter(expectedType), Term.Diff(right, left).filter(expectedType), MergeBias.Neutral)
    }
    
    case Merge(left, right, MergeBias.Right) => {
      Term.Merge(Term.Diff(left, right).filter(expectedType), right.filter(expectedType), MergeBias.Neutral)
    }
    
    case _ => this
  }
  
  override def contains(name: String): Boolean = this match {
    case Var(n) => n == name
    case Symbol(_, _) => false
    case Typed(term, _) => term.contains(name)
    case Primitive(_) => false
    case Apply(func, arg) => func.contains(name) || arg.contains(name)
    case TypeApply(term, _) => term.contains(name)
    // The bound variable shadows the name, so we do not look into the body.
    case Lambda(param, _, body, _) => param != name && body.contains(name)
    case TypeLambda(_, body) => body.contains(name)
    case Fixpoint(fixName, _, body) => fixName != name && body.contains(name)
    case Projection(record, _) => record.contains(name)
    case Record(fields) => fields.values.exists(_.contains(name))
    case Tuple(elements) => elements.exists(_.contains(name))
    case Merge(left, right, _) => left.contains(name) || right.contains(name)
    case Diff(left, right) => left.contains(name) || right.contains(name)
    case IfThenElse(cond, thenBr, elseBr) =>
      cond.contains(name) || thenBr.contains(name) || elseBr.contains(name)
    // case Match(scrutinee, clauses) =>
    //   scrutinee.contains(name) || clauses.exists(_.contains(name))
    case ArrayLiteral(elements) => elements.exists(_.contains(name))
    case FoldFixpoint(_, body) => body.contains(name)
    case UnfoldFixpoint(_, term) => term.contains(name)
    case Do(expr, body) => expr.contains(name) || body.contains(name)
    case RefAddr(_, _) => false
    case NativeFunctionCall(_, args) => args.exists(_.contains(name))
    case NativeProcedureCall(_, args) => args.exists(_.contains(name))
  }
  
  def contains(term: Term): Boolean = this match {
    // TODO: We should use unification here to check for structural equality.
    case _ if this == term => true
    case Var(_) => false
    case Symbol(_, _) => false
    case Typed(t, _) => t.contains(term)
    case Primitive(_) => false
    case Apply(func, arg) => func.contains(term) || arg.contains(term)
    case TypeApply(t, _) => t.contains(term)
    case Lambda(_, _, body, _) => body.contains(term)
    case TypeLambda(_, body) => body.contains(term)
    case Fixpoint(_, _, body) => body.contains(term)
    case Projection(record, _) => record.contains(term)
    case Record(fields) => fields.values.exists(_.contains(term))
    case Tuple(elements) => elements.exists(_.contains(term))
    case Merge(left, right, _) => left.contains(term) || right.contains(term)
    case Diff(left, right) => left.contains(term) || right.contains(term)
    case IfThenElse(cond, thenBr, elseBr) =>
      cond.contains(term) || thenBr.contains(term) || elseBr.contains(term)
    // case Match(scrutinee, clauses) =>
    //   scrutinee.contains(term) || clauses.exists(_.contains(term))
    case ArrayLiteral(elements) => elements.exists(_.contains(term))
    case FoldFixpoint(_, body) => body.contains(term)
    case UnfoldFixpoint(_, t) => t.contains(term)
    case Do(expr, body) => expr.contains(term) || body.contains(term)
    case RefAddr(_, _) => false
    case NativeFunctionCall(_, args) => args.exists(_.contains(term))
    case NativeProcedureCall(_, args) => args.exists(_.contains(term))
  }

  // Add parentheses where necessary to ensure correct parsing.
  def toAtomString: String = this match {
    case _: Symbol => s"($this)"
    case _: Lambda => s"($this)"
    case _: TypeLambda => s"($this)"
    case _: Fixpoint => s"($this)"
    case _: IfThenElse => s"($this)"
    case _: Merge => s"($this)"
    case _: Apply => s"($this)"
    case _: TypeApply => s"($this)"
    case _: NativeFunctionCall => s"($this)"
    case _ => this.toString
  }

  override def toString: String = this match {

    case Var(name) => name

    case Symbol(name, ty) => s"$name : $ty"

    case Typed(term, ty) => s"($term : $ty)"

    case Primitive(value) => value.toString

    case Apply(func, arg) => s"${func.toAtomString} ${arg.toAtomString}"

    case TypeApply(term, tyArg) => s"${term.toAtomString} @${tyArg}"

    case Lambda(param, paramType, body, false) => s"λ($param: $paramType). ${body.toAtomString}"

    case Lambda(param, paramType, body, true) => s"trait ($param: $paramType). $body"

    case TypeLambda(param, body) => s"Λ$param. $body"

    case Fixpoint(name, ty, recursiveBody) => s"fix $name: $ty = $recursiveBody"

    case Projection(record, field) => s"${record.toAtomString}.$field"

    case Record(fields) => s"{${fields.map { (name, term) => s"$name = $term" }.mkString("; ")}}"

    case Tuple(elements) => s"(${elements.map(_.toString).mkString(", ")})"

    case Merge(left, right, bias) => bias match {
      case MergeBias.Neutral => s"$left ,, $right"
      case MergeBias.Left => s"$left ,> $right"
      case MergeBias.Right => s"$left <, $right"
    }

    case Diff(left, right) => s"$left \\ $right"

    // case Match(scrutinee, clauses) =>
    //   s"match $scrutinee {\n${clauses.map(clause => s"  $clause").mkString("\n")}\n}"

    case IfThenElse(condition, thenBranch, elseBranch) => {
      s"if $condition then $thenBranch else $elseBranch"
    }

    case ArrayLiteral(elements) => s"[${elements.map(_.toString).mkString(", ")}]"

    case FoldFixpoint(fixpointType, body) => s"fold[$fixpointType] $body"

    case UnfoldFixpoint(fixpointType, term) => s"unfold[$fixpointType] $term"

    case Do(expr, body) => s"do { $expr; $body }"

    case RefAddr(refType, address) => s"ref[$refType]@$address"

    case NativeFunctionCall(function, args) => function.kind match {
      case NativeCallable.Kind.Default =>
        s"$function(${args.map(_.toAtomString).mkString(", ")})"
      case NativeCallable.Kind.Operator(symbol) =>
        if args.length == 2 then s"(${args.head.toAtomString} $symbol ${args(1).toAtomString})"
        else if args.length == 1 then s"($symbol${args.head.toAtomString})"
        else s"$function(${args.map(_.toAtomString).mkString(", ")})"
      case NativeCallable.Kind.Function(name) =>
        s"$name(${args.map(_.toAtomString).mkString(", ")})"
    }

    case NativeProcedureCall(procedure, args) => {
      s"$procedure(${args.map(_.toString).mkString(", ")})"
    }
  }
}

enum EvalMode {
  case Normalize, Unfold, Full
}

enum MergeBias {
  case Neutral, Left, Right
}

case class SelfAnnotation[T](
  name: String,
  ty: Option[T],
)
