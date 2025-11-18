package cp.interpreter

import cp.common.Environment
import cp.core.{MergeBias, Module, NeutralValue, PrimitiveType, PrimitiveValue, Term, Type, Value}

import scala.util.{Failure, Success, Try}

class DirectInterpreter(initialModules: Module*) extends Interpreter(initialModules*) {

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value = term.evalDirect

  private enum Unfolding {
    case Normal
    case Suspend(suspended: Set[Value.FixThunk])
  }

  extension (term: Term) {

    private def evalDirect(using env: Env, unfolding: Unfolding = Unfolding.Normal): Value = term match {

      case Term.Var(name) => env.getValue(name) match {
        case Some(value) => value
        case None => throw new RuntimeException(s"Variable $name not found in environment")
      }

      case Term.Primitive(value) => Value.Primitive(value)

      case Term.Lambda(param, paramType, body, isCoe) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }
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
        body.evalDirect(using newEnv)
      }

      case Term.Record(fields) => {
        val evaluatedFields = fields.map { case (name, term) =>
          name -> term.evalDirect
        }
        Value.Record(evaluatedFields)
      }

      case Term.Projection(recordTerm, field) => recordTerm match {
        // Special case: projection from a merge term - distribute the projection
        case Term.Merge(leftTerm, rightTerm, _) => {
          // Try projecting from both sides first, and if both fail, evaluate normally
          val leftProjection  = Try { Term.Projection(leftTerm , field).evalDirect }
          val rightProjection = Try { Term.Projection(rightTerm, field).evalDirect }

          (leftProjection, rightProjection) match {
            case (Success(leftResult), Success(rightResult)) => leftResult.merge(rightResult)
            case (Success(leftResult), _) => leftResult
            case (_, Success(rightResult)) => rightResult
            case _ => {
              // Both failed, evaluate normally
              val mergedValue = recordTerm.evalDirect
              mergedValue match {
                case Value.Record(fields) => fields.get(field) match {
                  case Some(value) => value
                  case None => throw new RuntimeException(s"Field $field not found in record $mergedValue")
                }
                case Value.Neutral(nv) => Value.Neutral(NeutralValue.Project(nv, field))
                case _ => throw new RuntimeException(s"Cannot project from non-record value $recordTerm")
              }
            }
          }
        }

        // Normal case: evaluate the record term first, then project
        case _ => {
          def projectFromValue(value: Value): Value = value match {
            case Value.Record(fields) => fields.get(field) match {
              case Some(fieldValue) => fieldValue
              case None => throw new RuntimeException(s"Field $field not found in record $value")
            }
            case Value.Neutral(NeutralValue.Merge(left, right)) => {
              // Try to project from merge - try left first, then right
              Try(projectFromValue(left)) match {
                case Success(result) => result
                case Failure(_) => Try(projectFromValue(right)) match {
                  case Success(result) => result
                  case Failure(_) => Value.Neutral(NeutralValue.Project(NeutralValue.Merge(left, right), field))
                }
              }
            }
            case Value.Neutral(nv) => Value.Neutral(NeutralValue.Project(nv, field))
            case Value.FixThunk(env, annotatedType, name, body) => {
              // Evaluate the fix thunk first
              val fixNeutral = Value.Neutral(NeutralValue.UnfoldingThunk(env, annotatedType, name, body))
              val fixValue = body.evalDirect(using env.addValueVar(name, fixNeutral))
              projectFromValue(fixValue)
            }
            case _ => throw new RuntimeException(s"Cannot project from non-record value: $value")
          }

          val recordValue = recordTerm.evalDirect
          projectFromValue(recordValue)
        }
      }

      case Term.Symbol(symbolName, symbolType) => modules.get(symbolName.namespace) match {
        case Some(module) => module.terms.get(symbolName.localName) match {
          case Some(term) => term.evalDirect(using env)
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
            body.evalDirect(using closureEnv.addTypeVar(typeParam, expandedTypeArg))
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

      case Term.ArrayLiteral(elements) => {
        val evaluatedElements = elements.map(_.evalDirect)
        Value.Array(evaluatedElements)
      }

      case Term.FoldFixpoint(fixpointType, bodyTerm) => {
        val bodyValue = bodyTerm.evalDirect
        bodyValue // Folding is essentially wrapping, represented by the value itself
      }

      case Term.UnfoldFixpoint(fixpointType, foldedTerm) => {
        if fixpointType.isTopLike then Value.Primitive(PrimitiveValue.UnitValue)
        else {
          val foldedValue = foldedTerm.evalDirect
          foldedValue match {
            case value => {
              // Unfold by substituting the fixpoint type variable with the fixpoint itself
              val unfoldedType = fixpointType match {
                case Type.Fixpoint(fixpointName, fixpointBody) => fixpointBody.subst(fixpointName, fixpointType)
                case _ => fixpointType
              }
              // Cast the value to the unfolded type
              value.cast(unfoldedType) match {
                case Some(castedValue) => castedValue
                case None => throw new RuntimeException(s"Cannot unfold $value to type $unfoldedType")
              }
            }
          }
        }
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
  extension (tpe: Type) {
    private def isTopLike: Boolean = tpe match {
      case Type.Primitive(PrimitiveType.TopType) => true
      case _ => false
    }
  }

  extension (value: Value) {
    def applyTo(arg: Value)(using env: Env): Option[Value] = value match {
      case Value.Closure(closureEnv, param, paramType, body, returnType, isCoe) => {
        // Check if argument type matches parameter type
        if arg.infer(using env) <:< paramType then {
          // Extend the closure environment with the argument
          val extendedEnv = closureEnv.addValueVar(param, arg)
          // Evaluate the body in the extended environment
          Some(body.evalDirect(using extendedEnv))
        } else None
      }

      case fixThunk @ Value.FixThunk(fixEnv, annotatedType, name, body) => {
        // Create a new environment that includes the fixpoint itself
        val newEnv = fixEnv.addValueVar(name, fixThunk)
        // Evaluate the body in the new environment
        val fn = body.evalDirect(using newEnv)
        fn.applyTo(arg)(using env)
      }

      case Value.Neutral(func @ NeutralValue.NativeCall(function, args)) => {
        if args.length + 1 == function.arity then {
          // Full application
          Try { function.call(args :+ arg) }.toOption
        } else {
          // Partial application
          Some(Value.Neutral(NeutralValue.NativeCall(function, args :+ arg)))
        }
      }

      case Value.Neutral(NeutralValue.Merge(left, right)) => {
        val leftApplication  = left.applyTo(arg)
        val rightApplication = right.applyTo(arg)
        (leftApplication, rightApplication) match {
          case (Some(leftResult), Some(rightResult)) => Some(leftResult.merge(rightResult))
          case (Some(leftResult), None) => Some(leftResult)
          case (None, Some(rightResult)) => Some(rightResult)
          case (None, None) => None
        }
      }

      case Value.Neutral(nv) => Some(Value.Neutral(NeutralValue.Apply(nv, arg)))

      case _ => None
    }
  }
}
