package cp.core

import cp.error.CoreError
import cp.error.CoreErrorKind.*

enum Term {
  
  case Var(name: String)

  case Typed(term: Term, ty: Type)
  
  case Primitive(value: Literal)
  
  case Apply(func: Term, arg: Term)
  
  // Subtyping relationship A <: B implies a coercion function of type A -> B
  case CoeApply(coe: Term, arg: Term)
  
  case TypeApply(term: Term, tyArg: Type)
  
  case Lambda(param: String, paramType: Type, body: Term)
  
  case Coercion(param: String, paramType: Type, body: Term)
  
  case TypeLambda(param: String, body: Term)
  
  case Fixpoint(name: String, ty: Type, recursiveBody: Term)
  
  case Projection(record: Term, field: String)
  
  case Record(fields: Map[String, Term])
  
  case Tuple(elements: List[Term])
  
  case Merge(left: Term, right: Term, bias: MergeBias = MergeBias.Neutral)
  
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
  case NativeFunctionCall(function: NativeFunction, args: List[Term])
  
  // A native procedure is only evaluated in FULL eval mode.
  //  Similar to native function calls, it can be partially applied.
  case NativeProcedureCall(procedure: NativeProcedure, args: List[Term])

  def infer(using env: Environment): Type = this match {
    
    case Var(name) => env.termVars.get(name) match {
      case Some(term) => term.infer
      case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
    }
    
    case Typed(_, ty) => ty
    
    case Primitive(value) => Type.Primitive(value.ty)
    
    case Apply(func, arg) => func.infer match {
      case Type.Arrow(paramType, returnType) =>
        val argType = arg.infer
        if !(argType <:< paramType) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType}, but got: ${argType}"
        } else returnType
      // TODO: Figure out whether we also need to handle trait (coercive) application here.
      case other => TypeNotMatch.raise(s"Expected function type, but got: ${other}")
    }
    
    case CoeApply(coe, arg) => coe.infer match {
      case Type.Trait(paramType, returnType) =>
        val argType = arg.infer
        if !(argType <:< paramType) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType}, but got: ${argType}"
        } else returnType
      case other => TypeNotMatch.raise(s"Expected coercion function type, but got: ${other}")
    }
    
    case TypeApply(term, tyArg) => term.infer match {
      case Type.Forall(param, body, _) => body.subst(param, tyArg)
      case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
    }
    
    case Lambda(param, paramType, body) => {
      env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) {
        newEnv => Type.Arrow(paramType, body.infer(using newEnv))
      }
    }

    case Coercion(_, _, _) => ???
    
    case TypeLambda(param, body) => {
      env.withTypeVar(param, Type.Var(param)) { 
        newEnv => Type.Forall(param, body.infer(using newEnv), None)
      }
    }
    
    case Fixpoint(_, ty, _) => ty
    
    case Projection(_, _) => ???
    
    case Record(fields) => {
      Type.Record(fields.map { (name, term) => (name, term.infer) })
    }
    
    case Tuple(elements) => Type.Tuple(elements.map(_.infer))
    
    case Merge(_, _, _) => ???
    
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

  def check(expectedType: Type)(using env: Environment): Boolean = this match {

    case Var(name) => env.termVars.get(name) match {
      case Some(term) => term.check(expectedType)
      case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
    }
    
    case Typed(term, termType) => {
      if !(termType <:< expectedType) then TypeNotMatch.raise {
        s"Annotated type ${termType} does not match expected type ${expectedType}"
      } else term.check(termType)
    }
    
    case Primitive(value) => Type.Primitive(value.ty) == expectedType
    
    case Apply(func, arg) => func.infer match {
      case Type.Arrow(paramType, returnType) =>
        if !(returnType <:< expectedType) then TypeNotMatch.raise {
          s"Function return type ${returnType} does not match expected type ${expectedType}"
        } else arg.check(paramType)
      case other => TypeNotMatch.raise(s"Expected function type, but got: ${other}")
    }
    
    case CoeApply(coe, arg) => coe.infer match {
      case Type.Trait(paramType, returnType) =>
        if !(returnType <:< expectedType) then TypeNotMatch.raise {
          s"Coercion return type ${returnType} does not match expected type ${expectedType}"
        } else arg.check(paramType)
      case other => TypeNotMatch.raise(s"Expected coercion function type, but got: ${other}")
    }
    
    case TypeApply(term, tyArg) => term.infer match {
      case Type.Forall(param, body, disjoint) =>
        val instantiatedType = body.subst(param, tyArg)
        if !(instantiatedType <:< expectedType) then TypeNotMatch.raise {
          s"Instantiated type ${instantiatedType} does not match expected type ${expectedType}"
        } else if disjoint.exists(!tyArg.disjointWith(_)) then TypeNotMatch.raise {
          s"Type argument ${tyArg} is not disjoint with constraints ${disjoint.get}"
        } else true
      case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
    }
    
    case Lambda(param, paramType, body) => expectedType match {
      case Type.Arrow(expectedParamType, expectedReturnType) =>
        if !(paramType <:< expectedParamType) then TypeNotMatch.raise {
          s"Lambda parameter type ${paramType} does not match expected type ${expectedParamType}"
        } else {
          env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) {
            newEnv => body.check(expectedReturnType)(using newEnv)
          }
        }
      case other => TypeNotMatch.raise(s"Expected function type, but got: ${other}")
    }
    
    case Coercion(param, paramType, body) => expectedType match {
      case Type.Trait(expectedParamType, expectedReturnType) =>
        if !(paramType <:< expectedParamType) then TypeNotMatch.raise {
          s"Coercion parameter type ${paramType} does not match expected type ${expectedParamType}"
        } else {
          env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) {
            newEnv => body.check(expectedReturnType)(using newEnv)
          }
        }
      case other => TypeNotMatch.raise(s"Expected coercion function type, but got: ${other}")
    }
    
    case TypeLambda(param, body) => expectedType match {
      case Type.Forall(expectedParam, bodyType, _) =>
        if param != expectedParam then TypeNotMatch.raise {
          s"Type lambda parameter ${param} does not match expected parameter ${expectedParam}"
        } else {
          env.withTypeVar(param, Type.Var(param)) { newEnv =>
            body.check(bodyType)(using newEnv)
          }
        }
      case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
    }
    
    case Fixpoint(name, ty, recursiveBody) => {
      if !(ty <:< expectedType) then TypeNotMatch.raise {
        s"Fixpoint type ${ty} does not match expected type ${expectedType}"
      } else {
        env.withTermVar(name, Term.Typed(Term.Var(name), ty)) { newEnv =>
          recursiveBody.check(ty)(using newEnv)
        }
      }
    }
    
    case Projection(record, field) => record.infer match {
      case Type.Record(fieldTypes) => fieldTypes.get(field) match {
        case Some(fieldType) =>
          if !(fieldType <:< expectedType) then TypeNotMatch.raise {
            s"Field type ${fieldType} does not match expected type ${expectedType}"
          } else true
        case None => TypeNotMatch.raise {
          s"Field '${field}' does not exist in record type ${record.infer}"
        }
      }
      case other => TypeNotMatch.raise {
        s"Expected record type, but got: ${other}"
      }
    }
      
    case Record(fields) => expectedType match {
      case Type.Record(expectedFieldTypes) =>
        if fields.keySet != expectedFieldTypes.keySet then TypeNotMatch.raise {
          s"Record fields ${fields.keySet} do not match expected fields ${expectedFieldTypes.keySet}"
        } else fields.forall { (name, term) =>
          val fieldType = expectedFieldTypes(name)
          term.check(fieldType)
        }
      case other => TypeNotMatch.raise(s"Expected record type, but got: ${other}")
    }
    
    case Tuple(elements) => expectedType match {
      case Type.Tuple(expectedElementTypes) =>
        if elements.length != expectedElementTypes.length then TypeNotMatch.raise {
          s"Tuple length ${elements.length} does not match expected length ${expectedElementTypes.length}"
        } else elements.zip(expectedElementTypes).forall { (term, ty) =>
          term.check(ty)
        }
      case other => TypeNotMatch.raise(s"Expected tuple type, but got: ${other}")
    }
    
    case Merge(_, _, _) => ???
    case IfThenElse(_, _, _) => ???
    case ArrayLiteral(_) => ???
    case FoldFixpoint(_, _) => ???
    case UnfoldFixpoint(_, _) => ???
    
    case Do(_, body) => body.check(expectedType)
    
    case RefAddr(_, _) => ???
    
    // TODO: Do we need to implement a better checking for native function calls?
    case NativeFunctionCall(function, args) => this.infer <:< expectedType
    
    case NativeProcedureCall(_, _) => ???
    
  }

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
  def eval(using env: Environment = Environment.empty)(
    using mode: EvalMode = EvalMode.Normalize
  ): Term = this match {
    
    case Var(name) => env.termVars.get(name) match {
      case Some(term) => term.eval
      case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
    }
    
    case Typed(term, ty) => {
      if !term.check(ty) then TypeNotMatch.raise {
        s"Annotated type ${ty} does not match inferred type ${term.infer}"
      } else term.eval
    }
    
    case Primitive(_) => this
    
    case Apply(func, arg) => {
      // In all modes, we first evaluate the function and argument.
      val argEval: Term = arg.eval
      val funcEval: Term = func.eval
      // In normalization mode, 
      //  when argument is a pure value term (e.g. primitives, pure lambdas, etc.),
      //  we can perform beta-reduction.
      // Otherwise, we keep the application form to avoid multiple evaluations of the argument.
      // In full evaluation mode, we always perform beta-reduction.
      //  This is safe because all side effects are contained in native procedure calls,
      //  which is only evaluated once at the moment of argument evaluation.
      //  So that even if the argument is not a pure value term,
      //  it will not cause multiple side effects.
      (funcEval, argEval, mode) match {
        case (Lambda(param, _, body), argValue, EvalMode.Normalize) if argValue.isValue => 
          env.withTermVar(param, argValue) { newEnv => body.eval(using newEnv) }
        case (Lambda(param, _, body), argValue, EvalMode.Full) => 
          env.withTermVar(param, argValue) { newEnv => body.eval(using newEnv) }
        case _ => Term.Apply(funcEval, argEval)
      }
    }
    
    case CoeApply(coe, arg) => {
      // Similar to Apply.
      val argEval: Term = arg.eval
      val coeEval: Term = coe.eval
      (coeEval, argEval, mode) match {
        case (Coercion(param, _, body), argValue, EvalMode.Normalize) if argValue.isValue => 
          env.withTermVar(param, argValue) { newEnv => body.eval(using newEnv) }
        case (Coercion(param, _, body), argValue, EvalMode.Full) => 
          env.withTermVar(param, argValue) { newEnv => body.eval(using newEnv) }
        case _ => Term.CoeApply(coeEval, argEval)
      }
    }
    
    case TypeApply(term, tyArg) => {
      val termEval: Term = term.eval
      (termEval, mode) match {
        // It is always safe to perform type-level beta-reduction.
        case (TypeLambda(param, body), EvalMode.Normalize | EvalMode.Full) => 
          env.withTypeVar(param, tyArg) { newEnv => body.eval(using newEnv) }
        case _ => Term.TypeApply(termEval, tyArg)
      }
    }
    
    case Lambda(param, paramType, body) => {
      env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) { 
        newEnv => Term.Lambda(param, paramType, body.eval(using newEnv))
      }
    }
    
    case Coercion(param, paramType, body) => {
      env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) { 
        newEnv => Term.Coercion(param, paramType, body.eval(using newEnv))
      }
    }
    
    case TypeLambda(param, body) => {
      env.withTypeVar(param, Type.Var(param)) { 
        newEnv => Term.TypeLambda(param, body.eval(using newEnv))
      }
    }
    
    case Fixpoint(name, ty, recursiveBody) => {
      // In normalization mode, we do not unfold the fixpoint.
      // In full evaluation mode, we always unfold the fixpoint.
      mode match {
        case EvalMode.Normalize => {
          // To avoid infinite unfolding of fixpoints, we assign a variable
          //  representing the fixpoint itself in the environment.
          env.withTermVar(name, Term.Typed(Term.Var(name), ty)) { 
            newEnv => Term.Fixpoint(name, ty, recursiveBody.eval(using newEnv))
          }
        }
        case EvalMode.Full => {
          // In full evaluation mode, we always unfold the fixpoint.
          // Therefore, we assign the fixpoint term itself in the environment.
          env.withTermVar(name, this) { 
            newEnv => recursiveBody.eval(using newEnv)
          }
        }
      }
    }
    
    case Projection(record, field) => {
      val recordEval: Term = record.eval
      recordEval match {
        case Record(fields) => fields.get(field) match {
          case Some(value) => value.eval
          case None => NoSuchField.raise {
            s"Field '$field' does not exist in record: $recordEval"
          }
        }
        case _ => Term.Projection(recordEval, field)
      }
    }
      
    case Record(fields) => {
      val evaluatedFields = fields.map { (name, term) => (name, term.eval) }
      Term.Record(evaluatedFields)
    }
    
    case Tuple(elements) => {
      val evaluatedElements = elements.map(_.eval)
      Term.Tuple(evaluatedElements)
    }
    
    case Merge(_, _, _) => ???
    
    case IfThenElse(condition, thenBranch, elseBranch) => {
      val conditionEval: Term = condition.eval
      (conditionEval, mode) match {
        case (Primitive(Literal.BoolValue(true)), EvalMode.Normalize | EvalMode.Full) => 
          thenBranch.eval
        case (Primitive(Literal.BoolValue(false)), EvalMode.Normalize | EvalMode.Full) => 
          elseBranch.eval
        case _ => Term.IfThenElse(conditionEval, thenBranch.eval, elseBranch.eval)
      }
    }
    
    case ArrayLiteral(elements) => {
      val evaluatedElements = elements.map(_.eval)
      Term.ArrayLiteral(evaluatedElements)
    }
    
    case FoldFixpoint(_, _) => ???
    
    case UnfoldFixpoint(_, _) => ???
    
    case Do(expr, body) => {
      // In normalization mode, we do not discard the value of expr after evaluation.
      //  This is to prevent discarding side effects in expr.
      // In full evaluation mode, we discard the value of expr after evaluation.
      //  This is safe because all side effects are contained in native procedure calls,
      //  which is only evaluated once at the moment of expr evaluation.
      //  So that even if the value of expr is discarded, the side effects are preserved.
      val exprEval: Term = expr.eval
      mode match {
        case EvalMode.Normalize => Term.Do(exprEval, body.eval)
        case EvalMode.Full => body.eval
      }
    }
    
    case RefAddr(_, _) => this
    
    case NativeFunctionCall(function, args) => {
      // The evaluation of native function calls are similar to Apply.
      //  As native functions are only implemented to handle pure values,
      //  we only perform beta-reduction when all arguments are fully evaluated to pure values.
      val evaluatedArgs: List[Term] = args.map(_.eval)
      if evaluatedArgs.forall(_.isValue) && evaluatedArgs.length == function.arity then {
        function.call(evaluatedArgs)
      } else {
        Term.NativeFunctionCall(function, evaluatedArgs)
      }
    }
    
    case NativeProcedureCall(procedure, args) => {
      // The evaluation of native procedure calls are similar to Apply.
      //  As native procedures may have side effects,
      //  we only perform beta-reduction in full evaluation mode
      //  when all arguments are fully evaluated to pure values.
      val evaluatedArgs: List[Term] = args.map(_.eval)
      if mode == EvalMode.Full && evaluatedArgs.forall(_.isValue) && evaluatedArgs.length == procedure.arity then {
        procedure.call(evaluatedArgs)
      } else {
        Term.NativeProcedureCall(procedure, evaluatedArgs)
      }
    }
  }
  
  private def isValue(allowedParams: Set[String] = Set.empty): Boolean = this match {
    case Var(name) => allowedParams.contains(name)
    case Primitive(_) => true
    case Lambda(param, _, body) => body.isValue(allowedParams + param)
    case Coercion(param, _, body) => body.isValue(allowedParams + param)
    case Fixpoint(name, _, body) => body.isValue(allowedParams + name)
    case TypeLambda(_, body) => body.isValue(allowedParams)
    case Typed(term, _) => term.isValue(allowedParams)
    case TypeApply(term, _) => term.isValue(allowedParams)
    case Record(fields) => fields.values.forall(_.isValue(allowedParams))
    case Tuple(elements) => elements.forall(_.isValue(allowedParams))
    case _ => false
  }
  
  def isValue: Boolean = this.isValue(Set.empty)
}

enum EvalMode {
  case Normalize, Full
}

enum MergeBias {
  case Neutral, Left, Right
}

case class SelfAnnotation[T](
  name: String,
  ty: Option[T],
)
