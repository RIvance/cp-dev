package cp.core

import cp.error.CoreErrorKind.*
import cp.util.{Graph, Recoverable}

import scala.util.{Failure, Success}

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

  def infer(using env: Environment): Type = {
    val inferredType = this match {

      case Var(name) => env.termVars.get(name) match {
        case Some(term) => term.infer
        case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
      }

      case Typed(_, ty) => ty

      case Primitive(value) => Type.Primitive(value.ty)

      case Apply(func, arg) => func.infer match {
        case Type.Arrow(paramType, returnType) =>
          val argType: Type = arg.infer
          if !(argType <:< paramType) then TypeNotMatch.raise {
            s"Expected argument type: ${paramType}, but got: ${argType}"
          } else returnType
        case fnType@Type.Intersection(_, _) => {
          val argType = arg.infer
          fnType.testApplicationReturn(argType) match {
            case Some(returnType) => returnType
            case None => TypeNotMatch.raise {
              s"Function type ${fnType} cannot be applied to argument type ${argType}"
            }
          }
        }
        // TODO: Figure out whether we also need to handle trait (coercive) application here.
        case other => TypeNotMatch.raise(s"Expected function type, but got: ${other}")
      }

      case CoeApply(coe, arg) => coe.infer match {
        case Type.Trait(paramType, returnType) =>
          val argType = arg.infer
          if !(argType <:< paramType) then TypeNotMatch.raise {
            s"Expected argument type: ${paramType}, but got: ${argType}"
          } else returnType
        case fnType@Type.Intersection(_, _) => {
          val argType = arg.infer
          fnType.testApplicationReturn(argType) match {
            case Some(returnType) => returnType
            case None => TypeNotMatch.raise {
              s"Coercion function type ${fnType} cannot be applied to argument type ${argType}"
            }
          }
        }
        case other => TypeNotMatch.raise(s"Expected coercion function type, but got: ${other}")
      }

      case TypeApply(term, tyArg) => term.infer match {
        case Type.Forall(param, body, _) => body.subst(param, tyArg)
        case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
      }

      case Lambda(param, paramType, body) => {
        env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) {
          implicit newEnv => Type.Arrow(paramType, body.infer(using newEnv))
        }
      }

      case Coercion(param, paramType, body) => {
        env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) {
          implicit newEnv => Type.Trait(paramType, body.infer(using newEnv))
        }
      }

      case TypeLambda(param, body) => {
        env.withTypeVar(param, Type.Var(param)) {
          implicit newEnv => Type.Forall(param, body.infer(using newEnv))
        }
      }

      case Fixpoint(_, ty, _) => ty

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

  def check(expectedType: Type)(using env: Environment): Boolean = this match {

    case Var(name) => env.termVars.get(name) match {
      case Some(Typed(_, ty)) => if !(ty <:< expectedType) then TypeNotMatch.raise {
        s"Variable '$name' has type ${ty}, which does not match expected type ${expectedType}"
      } else true
      case Some(term) => term.check(expectedType)
      case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
    }
    
    case Typed(term, termType) => {
      if !(termType <:< expectedType) then TypeNotMatch.raise {
        s"Annotated type ${termType} does not match expected type ${expectedType}"
      } else term.check(termType)
    }
    
    case Primitive(value) => Type.Primitive(value.ty) unify expectedType
    
    case Apply(func, arg) => func.infer match {
      case Type.Arrow(paramType, returnType) =>
        if !(returnType <:< expectedType) then TypeNotMatch.raise {
          s"Function return type ${returnType} does not match expected type ${expectedType}"
        } else arg.check(paramType)
      case fnType@Type.Intersection(_, _) => {
        val argType: Type = arg.infer
        // Try to find a function type in the intersection that can produce expectedType
        fnType.testApplicationReturn(argType) match {
          case Some(returnType) =>
            if !(returnType <:< expectedType) then TypeNotMatch.raise {
              s"Function return type ${returnType} does not match expected type ${expectedType}"
            } else true
          case None => TypeNotMatch.raise {
            s"Function type ${fnType} cannot be applied to argument type ${argType}"
          }
        }
      }
      case other => TypeNotMatch.raise(s"Expected function type, but got: ${other}")
    }
    
    case CoeApply(coe, arg) => coe.infer match {
      case Type.Trait(paramType, returnType) =>
        if !(returnType <:< expectedType) then TypeNotMatch.raise {
          s"Coercion return type ${returnType} does not match expected type ${expectedType}"
        } else arg.check(paramType)
      case fnType@Type.Intersection(_, _) => {
        // Try to find a coercion function type in the intersection that can produce expectedType
        val argType: Type = arg.infer
        fnType.testApplicationReturn(argType) match {
          case Some(returnType) =>
            if !(returnType <:< expectedType) then TypeNotMatch.raise {
              s"Coercion return type ${returnType} does not match expected type ${expectedType}"
            } else true
          case None => TypeNotMatch.raise {
            s"Coercion function type ${fnType} cannot be applied to argument type ${argType}"
          }
        }
      }
      case other => TypeNotMatch.raise(s"Expected coercion function type, but got: ${other}")
    }
    
    case TypeApply(term, tyArg) => term.infer match {
      case Type.Forall(param, body, constraints) =>
        val instantiatedType = body.subst(param, tyArg)
        if !(instantiatedType <:< expectedType) then TypeNotMatch.raise {
          s"Instantiated type ${instantiatedType} does not match expected type ${expectedType}"
        } else constraints.forall { constraint =>
          if !constraint.verify(tyArg) then ConstraintNotSatisfied.raise {
            s"Type argument $tyArg does not satisfy constraint $constraint"
          } else true
        }
      case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
    }
    
    case Lambda(param, paramType, body) => expectedType match {
      case Type.Arrow(expectedParamType, expectedReturnType) =>
        if !(paramType <:< expectedParamType) then TypeNotMatch.raise {
          s"Lambda parameter type ${paramType} does not match expected type ${expectedParamType}"
        } else {
          env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) {
            implicit newEnv => body.check(expectedReturnType)(using newEnv)
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
            implicit newEnv => body.check(expectedReturnType)(using newEnv)
          }
        }
      case other => TypeNotMatch.raise(s"Expected coercion function type, but got: ${other}")
    }
    
    case TypeLambda(param, body) => expectedType match {
      case Type.Forall(expectedParam, bodyType, _) =>
        if param != expectedParam then TypeNotMatch.raise {
          s"Type lambda parameter ${param} does not match expected parameter ${expectedParam}"
        } else {
          env.withTypeVar(param, Type.Var(param)) { implicit newEnv =>
            body.check(bodyType)(using newEnv)
          }
        }
      case other => TypeNotMatch.raise(s"Expected polymorphic type, but got: ${other}")
    }
    
    case Fixpoint(name, ty, recursiveBody) => {
      if !(ty <:< expectedType) then TypeNotMatch.raise {
        s"Fixpoint type ${ty} does not match expected type ${expectedType}"
      } else {
        env.withTermVar(name, Term.Typed(Term.Var(name), ty)) { implicit newEnv =>
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
        expectedFieldTypes.forall { (name, expectedFieldType) =>
          fields.get(name) match {
            case Some(fieldTerm) => fieldTerm.check(expectedFieldType)
            case None => NoSuchField.raise {
              s"Field '${name}' does not exist in record: ${this}"
            }
          }
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

    case Merge(lhs, rhs, MergeBias.Left) => {
      Merge(lhs, Term.Diff(rhs, lhs), MergeBias.Neutral).check(expectedType)
    }
    
    case Merge(lhs, rhs, MergeBias.Right) => {
      Merge(Term.Diff(lhs, rhs), rhs, MergeBias.Neutral).check(expectedType)
    }
    
    case Merge(left, right, MergeBias.Neutral) => {
      val leftType = left.infer
      val rightType = right.infer
      leftType.merge(rightType) <:< expectedType
    }

    case Diff(left, right) => {
      val leftType = left.infer
      val rightType = right.infer
      (leftType diff rightType) <:< expectedType
    }
    
    case IfThenElse(_, thenBranch, elseBranch) => {
      thenBranch.check(expectedType) && elseBranch.check(expectedType)
    }
    
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
      case Some(Typed(Var(newName), _)) if newName == name => this
      case Some(Var(newName)) if newName == name => this
      case Some(term) => term.eval
      case None => UnboundVariable.raise(s"Variable '$name' is not bound in the environment.")
    }
    
    case Typed(term, expectedType) => {
      if !term.check(expectedType) then TypeNotMatch.raise {
        s"Annotated type ${expectedType} does not match inferred type ${term.infer}"
      } else term.eval.filter(expectedType) // TODO: do we need to keep the type annotation after evaluation?
    }
    
    case Primitive(_) => this
    
    case Apply(func, arg) => {
      // In all modes, we first evaluate the function and argument.
      val argEval: Term = arg.eval
      // If current mode is Full, we unfold the inner fixpoints once.
      // Consider evaluating the following example:
      //  Step 1:
      //   ```
      //   (fix $self = {
      //      isEven = λ(n: Int) -> (if n == 0 then true else $self.isOdd(n - 1)); 
      //      isOdd  = λ(n: Int) -> (if n == 0 then false else $self.isEven(n - 1));
      //    }).isEven 42
      //   ```
      //   Here, as the function part is a projection from a fixpoint,
      //    we need to unfold the fixpoint once to get the actual function body.
      //
      //  Step 2:
      //    ```
      //    (λ(n: Int) -> if n == 0 then true else (fix $self = { ... }).isOdd(n - 1)) 42
      //    ```
      //   After the unfolding, we can perform beta-reduction.
      //
      //  Step 3:
      //    ```
      //    if n == 0 then true else (fix $self = { ... }).isOdd(41)
      //    ```
      //   Now, it is safe to further unfold the inner fixpoint (one-time) when evaluating the else branch.
      //
      val evalMode = if mode == EvalMode.Full then EvalMode.Unfold else EvalMode.Normalize
      val funcEval: Term = func.eval(using env)(using evalMode)

      funcEval.infer match {
        // It is a coercion application if the function type is a Trait.
        case Type.Trait(_, _) => return Term.CoeApply(funcEval, argEval).eval(using env)(using mode)
        case _ => () // continue
      }
      
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
        
        case (Fixpoint(name, _, body), argValue, EvalMode.Full) if argValue.isValue => {
          // Unfold the fixpoint once when it is applied to a value argument.
          env.withTermVar(name, funcEval) { implicit newEnv =>
            Term.Apply(body, argValue).eval(using newEnv)
          }
        }
          
        case (Lambda(param, paramType, body), argValue, _) if argValue.isValue => {
          env.withTermVar(param, Typed(argValue.filter(paramType), paramType)) { 
            implicit newEnv => body.eval(using newEnv) 
          }
        }
        
        case (Lambda(param, paramType, body), argValue, EvalMode.Full) => {
          env.withTermVar(param,  Typed(argValue.filter(paramType), paramType)) { 
            implicit newEnv => body.eval(using newEnv) 
          }
        }
        
        case (NativeFunctionCall(function, args), argValue, _) if args.length < function.arity => {
          // For partially applied native function calls, we can perform type application
          //  to instantiate the polymorphic native function.
          val evaluatedArgs: Seq[Term] = (args :+ argValue).map(_.eval)
          if args.length + 1 == function.arity && evaluatedArgs.forall(_.isValue) then {
            // If this application makes the native function fully applied,
            //  we can evaluate it to a primitive value.
            function.call(evaluatedArgs)
          } else {
            // Otherwise, typecheck existing args and return a new partially applied native function call.
            evaluatedArgs.zip(function.paramTypes).foreach { (arg, paramType) =>
              val argType = arg.infer
              if !(argType <:< paramType) then TypeNotMatch.raise {
                s"Expected argument type: ${paramType}, but got: ${argType}"
              }
            }
            Term.NativeFunctionCall(function, evaluatedArgs)
          }
        }
          
        case (Merge(left, right, MergeBias.Neutral), argEval, _) => {
          // Special case: when the function is a merge of two functions,
          //  we can try to apply both branches to the argument.
          //  If one branch fails (e.g. due to type mismatch), we return the other branch's result.
          //  If both branches succeed, we return a merge of both results.
          val leftApp = Recoverable { Term.Apply(left, argEval).eval(using env)(using mode) }
          val rightApp = Recoverable { Term.Apply(right, argEval).eval(using env)(using mode) }
          (leftApp, rightApp) match {
            case (Success(l), Success(r)) => Term.Merge(l, r, MergeBias.Neutral)
            case (Success(l), Failure(_)) => l
            case (Failure(_), Success(r)) => r
            case (Failure(_), Failure(_)) => TypeNotMatch.raise {
              s"Both branches of merge function failed to apply to argument: $argEval"
            }
          }
        }
        
        case _ => Term.Apply(funcEval, argEval)
      }
    }
    
    case CoeApply(coe, arg) => {
      // Similar to `Apply`.
      val argEval: Term = arg.eval
      val coeEval: Term = coe.eval
      (coeEval, argEval, mode) match {
        case (Coercion(param, _, body), argValue, _) =>
          env.withTermVar(param, argValue) { implicit newEnv => body.eval(using newEnv) }
        case _ => Term.CoeApply(coeEval, argEval)
      }
    }
    
    case TypeApply(term, tyArg) => {
      val termEval: Term = term.eval
      val normalizedTyArg = tyArg.normalize
      (termEval, mode) match {
        // It is always safe to perform type-level beta-reduction.
        case (TypeLambda(param, body), _) => 
          env.withTypeVar(param, normalizedTyArg) { implicit newEnv => body.eval(using newEnv) }
        case _ => Term.TypeApply(termEval, normalizedTyArg)
      }
    }
    
    case Lambda(param, paramType, body) => {
      val newParamType = paramType.normalize
      env.withTermVar(param, Term.Typed(Term.Var(param), newParamType)) { 
        implicit newEnv => Term.Lambda(param, newParamType, body.eval(using newEnv))
      }
    }
    
    case Coercion(param, paramType, body) => {
      val newParamType = paramType.normalize
      env.withTermVar(param, Term.Typed(Term.Var(param), newParamType)) { 
        implicit newEnv => Term.Coercion(param, newParamType, body.eval(using newEnv))
      }
    }
    
    case TypeLambda(param, body) => {
      env.withTypeVar(param, Type.Var(param)) { 
        implicit newEnv => Term.TypeLambda(param, body.eval(using newEnv))
      }
    }
    
    case Fixpoint(name, ty, recursiveBody) => {
      if !recursiveBody.contains(name) then {
        // If the body does not contain the fixpoint variable,
        //  then it is not really a recursive definition.
        // We can just evaluate the body directly.
        recursiveBody.eval
      } else env.withTermVar(name, Term.Typed(Term.Var(name), ty)) {
        implicit newEnv => Term.Fixpoint(name, ty, recursiveBody.eval(using newEnv))
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
        // See <https://i.cs.hku.hk/~bruno/papers/yaozhu.pdf>
        //  page 206. Reconciliation between eagerness and laziness
        case Fixpoint(name, ty, Record(fields)) => {
          // We check whether there's a mutual recursion between the fields using graph analysis.
          lazy val initGraph = Graph.directed[String].addVertices(fields.keySet)
          lazy val graph = fields.foldLeft(initGraph) { (graph0, entry) =>
            val (fieldName, fieldTerm) = entry
            fields.keySet.foldLeft(graph0) { (graph1, otherFieldName) =>
              if fieldTerm.contains(Projection(Var(name), otherFieldName)) then {
                graph1.addEdge(fieldName, otherFieldName)
              } else graph1
            }
          }
          if mode == EvalMode.Normalize && graph.isInCycle(field) then {
            // If the field is in a cycle, we cannot safely unfold the fixpoint.
            //  We return the projection term as is to avoid non-termination.
            Term.Projection(recordEval, field)
          }
          // For a projection on a fixpoint whose body is a record:
          else fields.get(field) match {
            // If the field does not contain the fixpoint variable,
            //  we can safely return the field value directly.
            case Some(value) if !value.contains(name) => value.eval
            // Otherwise, we unfold the fixpoint once
            //  (i.e., replace the fixpoint variable with the fixpoint itself)
            //  and then return the field value.
            case Some(value) => env.withTermVar(name, recordEval) { implicit newEnv => 
              value.eval(using newEnv)(using if mode == EvalMode.Unfold then EvalMode.Normalize else mode)
            }
            case None => NoSuchField.raise {
              s"Field '$field' does not exist in record: $recordEval"
            }
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
      if elements.isEmpty then Term.Primitive(Literal.UnitValue)
      else Term.Tuple(elements.map(_.eval))
    }
    
    case Merge(left, right, MergeBias.Left) => {
      Merge(left, Term.Diff(right, left), MergeBias.Neutral).eval
    }
    
    case Merge(left, right, MergeBias.Right) => {
      Merge(Term.Diff(left, right), right, MergeBias.Neutral).eval
    }
    
    case Merge(left, right, MergeBias.Neutral) => {
      val leftEval: Term = left.eval
      val rightEval: Term = right.eval
      (leftEval, rightEval) match {
        case (Record(leftFields), Record(rightFields)) => {
          // Merging two records
          val commonFields = leftFields.keySet.intersect(rightFields.keySet)
          if commonFields.nonEmpty then TypeNotMatch.raise {
            s"Cannot merge two records with overlapping fields: ${commonFields.mkString(", ")}"
          }
          Term.Record(leftFields ++ rightFields)
        }
        case (Tuple(leftElements), Tuple(rightElements)) if leftElements.length == rightElements.length => {
          val mergedElements = leftElements.zip(rightElements).map { (l, r) =>
            Term.Merge(l, r, MergeBias.Neutral).eval
          }
          Term.Tuple(mergedElements)
        }
        case (Coercion(paramL, paramTypeL, bodyL), Coercion(paramR, paramTypeR, bodyR)) => {
          val param = if paramL == paramR then paramL else env.freshVarName(bodyL, bodyR)
          val paramType = paramTypeL merge paramTypeR
          env.withTermVar(param, Term.Typed(Term.Var(param), paramType)) { implicit newEnv =>
            val body = Term.Merge(bodyL, bodyR).eval(using newEnv)
            Term.Coercion(param, paramType, body)
          }
        }
        case _ => Term.Merge(leftEval, rightEval, MergeBias.Neutral)
      }
    }

    case Diff(_, _) => ???

    case IfThenElse(condition, thenBranch, elseBranch) => {
      val conditionEval: Term = condition.eval
      (conditionEval, mode) match {
        case (Primitive(Literal.BoolValue(true)), _) =>
          thenBranch.eval
        case (Primitive(Literal.BoolValue(false)), _) =>
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
        case EvalMode.Normalize | EvalMode.Unfold => Term.Do(exprEval, body.eval)
        case EvalMode.Full => body.eval
      }
    }

    case RefAddr(_, _) => this

    case NativeFunctionCall(function, args) => {
      // The evaluation of native function calls are similar to Apply.
      //  As native functions are only implemented to handle pure values,
      //  we only perform beta-reduction when all arguments are fully evaluated to pure values.
      val evaluatedArgs: Seq[Term] = args.map(_.eval)
      // Check the argument types against the function's parameter types.
      function.paramTypes.zip(evaluatedArgs).foreach { (paramType, arg) =>
        val argType = arg.infer
        if !(argType <:< paramType) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType}, but got: ${argType}"
        }
      }
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
      val evaluatedArgs: Seq[Term] = args.map(_.eval)
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

  /**
   * For merged terms, filter out the branches that do not conform to the expected type.
   */
  def filter(expectedType: Type)(using env: Environment): Term = this match {
    case Merge(left, right, MergeBias.Neutral) => {
      val leftType = left.infer
      val rightType = right.infer
      (leftType <:< expectedType, rightType <:< expectedType) match {
        case (true, true) => Term.Merge(left.filter(expectedType), right.filter(expectedType), MergeBias.Neutral)
        case (true, false) => left.filter(expectedType)
        case (false, true) => right.filter(expectedType)
        case (false, false) => TypeNotMatch.raise {
          s"Neither branch of merge conforms to expected type: ${expectedType}"
        }
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
  
  def contains(name: String): Boolean = this match {
    case Var(n) => n == name
    case Typed(term, _) => term.contains(name)
    case Primitive(_) => false
    case Apply(func, arg) => func.contains(name) || arg.contains(name)
    case CoeApply(coe, arg) => coe.contains(name) || arg.contains(name)
    case TypeApply(term, _) => term.contains(name)
    // The bound variable shadows the name, so we do not look into the body.
    case Lambda(param, _, body) => param != name && body.contains(name)
    case Coercion(param, _, body) => param != name && body.contains(name)
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
    case Typed(t, _) => t.contains(term)
    case Primitive(_) => false
    case Apply(func, arg) => func.contains(term) || arg.contains(term)
    case CoeApply(coe, arg) => coe.contains(term) || arg.contains(term)
    case TypeApply(t, _) => t.contains(term)
    case Lambda(_, _, body) => body.contains(term)
    case Coercion(_, _, body) => body.contains(term)
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
    case _: Lambda => s"($this)"
    case _: Coercion => s"($this)"
    case _: TypeLambda => s"($this)"
    case _: Fixpoint => s"($this)"
    case _: IfThenElse => s"($this)"
    case _: Apply => s"($this)"
    case _: CoeApply => s"($this)"
    case _: TypeApply => s"($this)"
    case _: NativeFunctionCall => s"($this)"
    case _ => this.toString
  }

  override def toString: String = this match {

    case Var(name) => name

    case Typed(term, ty) => s"($term : $ty)"

    case Primitive(value) => value.toString

    case Apply(func, arg) => s"${func.toAtomString} ${arg.toAtomString}"

    case CoeApply(coe, arg) => s"${coe.toAtomString} ${arg.toAtomString}"

    case TypeApply(term, tyArg) => s"${term.toAtomString} @${tyArg}"

    case Lambda(param, paramType, body) => s"λ($param: $paramType). ${body.toAtomString}"

    case Coercion(param, paramType, body) => s"trait ($param: $paramType). $body"

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
