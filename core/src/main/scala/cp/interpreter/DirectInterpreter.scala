package cp.interpreter

import cp.common.Environment
import cp.core.{MergeBias, Module, NeutralValue, PrimitiveType, PrimitiveValue, Term, Type, Value}
import cp.core.matchValue

class DirectInterpreter(initialModules: Module*) extends Interpreter(initialModules*) {

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value = term.evalDirect

  extension (term: Term) {

    private def evalDirect(
      using env: Env, unfoldingSuppressor: UnfoldingSuppressor = UnfoldingSuppressor()
    ): Value = term match {

      case Term.Var(name) => env.getValue(name) match {
        case Some(value) => value
        case None => throw new RuntimeException(s"Variable $name not found in environment")
      }

      case Term.Primitive(value) => Value.Primitive(value)

      case Term.Lambda(param, paramType, body, isCoe) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }.addValueVar(param, Term.Annotated(Term.Var(param), paramType))
        val returnType = body.infer(using termEnv).normalize
        Value.Closure(env, param, paramType.normalize, body, returnType, isCoe)
      }

      case Term.Apply(fnTerm, argTerm) => {
        val fnValue = fnTerm.evalDirect
        val argValue = argTerm.evalDirect
        fnValue.applyTo(argValue)(using env) match {
          case Some(result) => result
          case None => throw new RuntimeException(s"Runtime type error: cannot apply $fnValue to argument $argValue")
        }
      }

      case Term.Fixpoint(name, annotatedType, body) => {
        // Create a thunk that will be used for self-reference
        lazy val newEnv = env.addValueVar(name, fixThunk)
        // Fixpoint itself will be added to the environment when evaluating the thunk
        lazy val fixThunk: Value = Value.FixThunk(env, annotatedType.normalize, name, body)
        // Unfold once
        body.evalDirect(using newEnv, unfoldingSuppressor)
      }

      case Term.Record(fields) => {
        val evaluatedFields = fields.map { case (name, term) => name -> term.evalDirect }
        Value.Record(evaluatedFields)
      }

      case Term.Projection(recordTerm, field) => {
        def projectFromValue(value: Value): Option[Value] = value match {
          case Value.Record(fields) => fields.get(field)
          case Value.Merge(values) => {
            // Try to project from each value in the merge set
            val results = values.values.flatMap(projectFromValue)
            if results.isEmpty then None
            else Some(results.reduce((left, right) => left.merge(right)(using env)))
          }
          case Value.Neutral(NeutralValue.Merge(left, right)) => {
            // Try to project from merge - try left and right and merge results if both succeed
            (projectFromValue(Value.Neutral(left)), projectFromValue(right)) match {
              case (Some(leftResult), Some(rightResult)) => Some(leftResult.merge(rightResult)(using env))
              case (Some(leftResult), None) => Some(leftResult)
              case (None, Some(rightResult)) => Some(rightResult)
              case (None, None) => None
            }
          }
          case Value.Neutral(nv) => Some(Value.Neutral(NeutralValue.Project(nv, field)))
          case fixThunk @ Value.FixThunk(fixEnv, annotatedType, name, body) => {
            // Evaluate the fix thunk first
            val fixValue = if unfoldingSuppressor.isSuppressed(fixThunk -> field) then {
              // Unfolding is suppressed for this fixpoint projection
              Value.Neutral(NeutralValue.UnfoldingThunk(fixEnv, annotatedType, name, body))
            } else fixThunk
            val envWithFix = fixEnv.addValueVar(name, fixValue)
            val projectValue = body.evalDirect(using envWithFix, unfoldingSuppressor + (fixThunk, field))
            projectFromValue(projectValue)
          }
          case _ => throw new RuntimeException(s"Cannot project from non-record value: $value")
        }

        val recordValue = recordTerm.evalDirect
        projectFromValue(recordValue) match {
          case Some(projectedValue) => projectedValue
          case None => throw new RuntimeException(s"Field $field not found in record $recordValue")
        }
      }

      case Term.Symbol(symbolName, symbolType) => modules.get(symbolName.namespace) match {
        case Some(module) => module.terms.get(symbolName.localName) match {
          case Some(term) => term.evalDirect(using env, unfoldingSuppressor)
          case None => throw new RuntimeException(s"Term ${symbolName.localName} not found in module ${symbolName.namespace}")
        }
        case None => throw new RuntimeException(s"Module ${symbolName.namespace} not found")
      }

      case Term.Annotated(annotatedTerm, annotationType) => {
        val termValue = annotatedTerm.evalDirect
        termValue.cast(annotationType.normalize) match {
          case Some(castedValue) => castedValue
          case None => throw new RuntimeException(s"Cannot cast value $termValue to type $annotationType")
        }
      }

      case Term.TypeApply(polymorphicTerm, typeArgument) => {
        val polymorphicValue = polymorphicTerm.evalDirect
        val expandedTypeArg = typeArgument.normalize // Type expansion would happen here if needed
        polymorphicValue match {
          case Value.TypeClosure(closureEnv, typeParam, body, _) =>
            body.evalDirect(using closureEnv.addTypeVar(typeParam, expandedTypeArg), unfoldingSuppressor)
          case other => throw new RuntimeException(s"Runtime type error: expected type-polymorphic value, got $other")
        }
      }

      case Term.TypeLambda(typeParam, body) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }
        val returnType = body.infer(using termEnv.addTypeVar(typeParam, Type.Var(typeParam)))
        Value.TypeClosure(env, typeParam, body, returnType)
      }

      case Term.Tuple(elements) => {
        val evaluatedElements = elements.map(_.evalDirect)
        Value.Tuple(evaluatedElements)
      }

      case Term.Merge(leftTerm, rightTerm, mergeBias) => mergeBias match {
        case MergeBias.Left => {
          val leftValue = leftTerm.evalDirect
          val rightValue = rightTerm.evalDirect
          val diffValue = rightValue.diff(leftValue)(using env)
          leftValue.merge(diffValue)(using env)
        }

        case MergeBias.Right => {
          val leftValue = leftTerm.evalDirect
          val rightValue = rightTerm.evalDirect
          val diffValue = leftValue.diff(rightValue)(using env)
          diffValue.merge(rightValue)(using env)
        }

        case MergeBias.Neutral => {
          val leftValue = leftTerm.evalDirect
          val rightValue = rightTerm.evalDirect
          leftValue.merge(rightValue)(using env)
        }
      }

      case Term.Diff(leftTerm, rightTerm) => {
        val leftValue = leftTerm.evalDirect
        val rightValue = rightTerm.evalDirect
        leftValue.diff(rightValue)(using env)
      }

      case Term.IfThenElse(conditionTerm, thenBranch, elseBranch) => {
        val conditionValue = conditionTerm.evalDirect
        conditionValue match {
          case Value.Primitive(PrimitiveValue.BoolValue(true)) => thenBranch.evalDirect
          case Value.Primitive(PrimitiveValue.BoolValue(false)) => elseBranch.evalDirect
          case other => throw new RuntimeException(s"Runtime type error: condition must be boolean, got $other")
        }
      }

      case Term.Match(scrutineeTerm, clauses) => {
        val scrutineeValue = scrutineeTerm.evalDirect

        // Try each clause in order until one matches
        val matchedClause = clauses.find { clause =>
          // For now, we only support single-pattern clauses
          clause.patterns.headOption.exists(_.matchValue(scrutineeValue)(using env).isDefined)
        }

        matchedClause match {
          case Some(clause) =>
            // Get the bindings from the first pattern
            val bindings = clause.patterns.head.matchValue(scrutineeValue)(using env).get
            // Extend environment with bindings
            val extendedEnv = bindings.foldLeft(env) { case (acc, (name, value)) =>
              acc.addValueVar(name, value)
            }
            // Evaluate the body with extended environment
            clause.body.evalDirect(using extendedEnv, unfoldingSuppressor)
          case None =>
            throw new RuntimeException(s"No matching pattern for value: $scrutineeValue")
        }
      }

      case Term.ArrayLiteral(elements) => {
        val evaluatedElements = elements.map(_.evalDirect)
        Value.Array(evaluatedElements)
      }

      case Term.Do(exprTerm, bodyTerm) => {
        exprTerm.evalDirect  // Evaluate expression for side effects
        bodyTerm.evalDirect  // Return the body value
      }

      case Term.RefAddr(refType, address) => {
        // Reference addresses are runtime values that cannot be further evaluated
        Value.Primitive(PrimitiveValue.IntValue(BigInt(address)))
      }

      case Term.NativeFunctionCall(nativeFunction, args) => {
        val evaluatedArgs = args.map(_.evalDirect)
        if evaluatedArgs.length == nativeFunction.arity then {
          // Full application: call the native function
          nativeFunction.call(evaluatedArgs)
        } else {
          // Partial application: return a closure
          Value.Neutral(NeutralValue.NativeCall(nativeFunction, evaluatedArgs.toList))
        }
      }

      case Term.NativeProcedureCall(nativeProcedure, args) => {
        // Native procedures are similar to native functions but may have side effects
        // They are only evaluated in full eval mode
        val evaluatedArgs = args.map(_.evalDirect)
        if evaluatedArgs.length == nativeProcedure.arity then {
          // Full application: call the native procedure
          nativeProcedure.call(evaluatedArgs)
        } else {
          // Partial application: return a closure
          Value.Neutral(NeutralValue.NativeCall(nativeProcedure, evaluatedArgs.toList))
        }
      }
    }
  }

  // Extension method to check if a type is top-like
  extension (tpe: Type) def isTopLike: Boolean = tpe match {
    case Type.Primitive(PrimitiveType.TopType) => true
    case _ => false
  }

  extension (value: Value) {
    private def applyTo(arg: Value)(
      using env: Env, unfoldingSuppressor: UnfoldingSuppressor
    ): Option[Value] = {
      val argType = arg.infer(using env)
      value match {
        case Value.Closure(closureEnv, param, paramType, body, returnType, isCoe) => {
          // Check if argument type matches parameter type
          if argType <:< paramType then {
            // Extend the closure environment with the argument
            val extendedEnv = closureEnv.addValueVar(param, arg)
            // Evaluate the body in the extended environment
            val result = body.evalDirect(using extendedEnv, unfoldingSuppressor)
            result.cast(returnType).orElse {
              throw new RuntimeException(s"Type cast error: cannot cast result $result to return type $returnType")
            }
          } else None
        }

        case fixThunk @ Value.FixThunk(fixEnv, annotatedType, name, body) => {
          annotatedType match {
            case Type.Arrow(paramType, returnType, _) =>
              // Check if argument type matches parameter type
              if !(argType <:< paramType) then return None
            case _ => return None
          }
          // Create a new environment that includes the fixpoint itself
          val newEnv = fixEnv.addValueVar(name, fixThunk)
          // Evaluate the body in the new environment
          val fn = body.evalDirect(using newEnv, unfoldingSuppressor)
          fn.applyTo(arg)(using env)
        }

        case Value.Neutral(func @ NeutralValue.NativeCall(function, prevArgs)) => {
          function.paramTypes.lift(prevArgs.length) match {
            case Some(paramType) =>
              // Check if argument type matches parameter type
              if !(argType <:< paramType) then return None
            case None => return None
          }
          val args = prevArgs :+ arg
          if args.length == function.arity && !args.exists(_.isNeutral) then {
            // Last argument, perform full application
            Some(function.call(args))
          } else {
            // Partial application
            Some(Value.Neutral(NeutralValue.NativeCall(function, args)))
          }
        }

        case Value.Neutral(NeutralValue.Merge(left, right)) => {
          val leftApplication  = Value.Neutral(left).applyTo(arg)
          val rightApplication = right.applyTo(arg)
          (leftApplication, rightApplication) match {
            case (Some(leftResult), Some(rightResult)) => Some(leftResult.merge(rightResult)(using env))
            case (Some(leftResult), None) => Some(leftResult)
            case (None, Some(rightResult)) => Some(rightResult)
            case (None, None) => None
          }
        }

        case Value.Merge(values) => {
          val applications = values.values.flatMap(_.applyTo(arg))
          if applications.isEmpty then None
          else Some(applications.reduce((left, right) => left.merge(right)(using env)))
        }

        case Value.Neutral(nv) => Some(Value.Neutral(NeutralValue.Apply(nv, arg)))

        case _ => None
      }
    }
  }
}
