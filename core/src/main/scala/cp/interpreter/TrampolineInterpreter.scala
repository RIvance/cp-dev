package cp.interpreter

import cp.common.Environment
import cp.core.{MergeBias, Module, NeutralValue, PrimitiveType, PrimitiveValue, Term, Type, Value}

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class TrampolineInterpreter(initialModules: Module*) extends Interpreter(initialModules*) {

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value = {
    term.evalTramp(using env, UnfoldingSuppressor()).result
  }

  extension (term: Term) {

    private def evalTramp(
      using env: Env, unfoldingSuppressor: UnfoldingSuppressor
    ): TailRec[Value] = term match {

      case Term.Var(name) => env.getValue(name) match {
        case Some(value) => done(value)
        case None => throw new RuntimeException(s"Variable $name not found in environment")
      }

      case Term.Primitive(value) => done(Value.Primitive(value))

      case Term.Lambda(param, paramType, body, isCoe) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }.addValueVar(param, Term.Annotated(Term.Var(param), paramType))
        val returnType = body.infer(using termEnv).normalize
        done(Value.Closure(env, param, paramType.normalize, body, returnType, isCoe))
      }

      case Term.Apply(fnTerm, argTerm) => for {
        fnValue <- fnTerm.evalTramp
        argValue <- argTerm.evalTramp
        result <- fnValue.applyTo(argValue)(using env, unfoldingSuppressor) match {
          case Some(resultValue) => resultValue
          case None => throw new RuntimeException(s"Runtime type error: cannot apply $fnValue to argument $argValue")
        }
      } yield result

      case Term.Fixpoint(name, annotatedType, body) => {
        // Create a thunk that will be used for self-reference
        lazy val newEnv = env.addValueVar(name, fixThunk)
        // Fixpoint itself will be added to the environment when evaluating the thunk
        lazy val fixThunk: Value = Value.FixThunk(env, annotatedType.normalize, name, body)
        // Unfold once
        tailcall(body.evalTramp(using newEnv, unfoldingSuppressor))
      }

      case Term.Record(fields) => {
        def evalFields(rest: List[(String, Term)], acc: Map[String, Value]): TailRec[Value] = rest match {
          case Nil => done(Value.Record(acc))
          case (name, term) :: rest => for {
            value <- term.evalTramp
            result <- tailcall(evalFields(rest, acc + (name -> value)))
          } yield result
        }
        tailcall(evalFields(fields.toList, Map.empty))
      }

      case Term.Projection(recordTerm, field) => {
        def projectFromValue(value: Value): Option[TailRec[Value]] = value match {
          case Value.Record(fields) => fields.get(field).map(done)
          case Value.Merge(values) => {
            // Try to project from each value in the merge set
            val results = values.flatMap(projectFromValue)
            if results.isEmpty then None
            else Some(done(results.map(_.result).reduce((left, right) => left.merge(right)(using env))))
          }
          case Value.Neutral(NeutralValue.Merge(left, right)) => {
            // Try to project from merge - try left and right and merge results if both succeed
            (projectFromValue(Value.Neutral(left)), projectFromValue(right)) match {
              case (Some(leftComp), Some(rightComp)) => Some(for {
                leftResult <- leftComp
                rightResult <- rightComp
              } yield leftResult.merge(rightResult)(using env))
              case (Some(leftComp), None) => Some(leftComp)
              case (None, Some(rightComp)) => Some(rightComp)
              case (None, None) => None
            }
          }
          case Value.Neutral(nv) => Some(done(Value.Neutral(NeutralValue.Project(nv, field))))
          case fixThunk @ Value.FixThunk(fixEnv, annotatedType, name, body) => {
            // Evaluate the fix thunk first
            val fixValue = if unfoldingSuppressor.isSuppressed(fixThunk -> field) then {
              // Unfolding is suppressed for this fixpoint projection
              Value.Neutral(NeutralValue.UnfoldingThunk(fixEnv, annotatedType, name, body))
            } else fixThunk
            val envWithFix = fixEnv.addValueVar(name, fixValue)
            Some(for {
              projectValue <- body.evalTramp(using envWithFix, unfoldingSuppressor + (fixThunk, field))
              result <- projectFromValue(projectValue) match {
                case Some(comp) => comp
                case None => throw new RuntimeException(s"Field $field not found in record")
              }
            } yield result)
          }
          case _ => None
        }

        for {
          recordValue <- recordTerm.evalTramp
          result <- projectFromValue(recordValue) match {
            case Some(comp) => comp
            case None => throw new RuntimeException(s"Field $field not found in record $recordValue")
          }
        } yield result
      }

      case Term.Symbol(symbolName, symbolType) => modules.get(symbolName.namespace) match {
        case Some(module) => module.terms.get(symbolName.localName) match {
          case Some(term) => tailcall(term.evalTramp(using env, unfoldingSuppressor))
          case None => throw new RuntimeException(s"Term ${symbolName.localName} not found in module ${symbolName.namespace}")
        }
        case None => throw new RuntimeException(s"Module ${symbolName.namespace} not found")
      }

      case Term.Annotated(annotatedTerm, annotationType) => for {
        termValue <- annotatedTerm.evalTramp
        result <- termValue.cast(annotationType.normalize) match {
          case Some(castedValue) => done(castedValue)
          case None => throw new RuntimeException(s"Cannot cast value $termValue to type $annotationType")
        }
      } yield result

      case Term.TypeApply(polymorphicTerm, typeArgument) => for {
        polymorphicValue <- polymorphicTerm.evalTramp
        result <- polymorphicValue match {
          case Value.TypeClosure(closureEnv, typeParam, body, _) =>
            val expandedTypeArg = typeArgument.normalize // Type expansion would happen here if needed
            tailcall(body.evalTramp(using closureEnv.addTypeVar(typeParam, expandedTypeArg), unfoldingSuppressor))
          case other => throw new RuntimeException(s"Runtime type error: expected type-polymorphic value, got $other")
        }
      } yield result

      case Term.TypeLambda(typeParam, body) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }
        val returnType = body.infer(using termEnv.addTypeVar(typeParam, Type.Var(typeParam)))
        done(Value.TypeClosure(env, typeParam, body, returnType))
      }

      case Term.Tuple(elements) => {
        def evalElements(remaining: List[Term], accumulated: List[Value]): TailRec[Value] = remaining match {
          case Nil => done(Value.Tuple(accumulated))
          case head :: tail => for {
            headValue <- head.evalTramp
            result <- tailcall(evalElements(tail, accumulated :+ headValue))
          } yield result
        }
        tailcall(evalElements(elements, List.empty))
      }

      case Term.Merge(leftTerm, rightTerm, mergeBias) => mergeBias match {
        case MergeBias.Left => for {
          leftValue <- leftTerm.evalTramp
          rightValue <- rightTerm.evalTramp
          diffValue = rightValue.diff(leftValue)(using env)
          result = leftValue.merge(diffValue)(using env)
        } yield result

        case MergeBias.Right => for {
          leftValue <- leftTerm.evalTramp
          rightValue <- rightTerm.evalTramp
          diffValue = leftValue.diff(rightValue)(using env)
          result = diffValue.merge(rightValue)(using env)
        } yield result

        case MergeBias.Neutral => for {
          leftValue <- leftTerm.evalTramp
          rightValue <- rightTerm.evalTramp
          result = leftValue.merge(rightValue)(using env)
        } yield result
      }

      case Term.Diff(leftTerm, rightTerm) => for {
        leftValue <- leftTerm.evalTramp
        rightValue <- rightTerm.evalTramp
        result = leftValue.diff(rightValue)(using env)
      } yield result

      case Term.IfThenElse(conditionTerm, thenBranch, elseBranch) => for {
        conditionValue <- conditionTerm.evalTramp
        result <- conditionValue match {
          case Value.Primitive(PrimitiveValue.BoolValue(true)) => tailcall(thenBranch.evalTramp)
          case Value.Primitive(PrimitiveValue.BoolValue(false)) => tailcall(elseBranch.evalTramp)
          case other => throw new RuntimeException(s"Runtime type error: condition must be boolean, got $other")
        }
      } yield result

      case Term.ArrayLiteral(elements) => {
        def evalElements(remaining: List[Term], accumulated: List[Value]): TailRec[Value] = remaining match {
          case Nil => done(Value.Array(accumulated))
          case head :: tail => for {
            headValue <- head.evalTramp
            result <- tailcall(evalElements(tail, accumulated :+ headValue))
          } yield result
        }
        tailcall(evalElements(elements, List.empty))
      }

      case Term.FoldFixpoint(fixpointType, bodyTerm) => for {
        bodyValue <- bodyTerm.evalTramp
      } yield bodyValue // Folding is essentially wrapping, represented by the value itself

      case Term.UnfoldFixpoint(fixpointType, foldedTerm) => {
        if fixpointType.isTopLike then {
          done(Value.Primitive(PrimitiveValue.UnitValue))
        } else for {
          foldedValue <- foldedTerm.evalTramp
          result <- foldedValue match {
            case value => {
              // Unfold by substituting the fixpoint type variable with the fixpoint itself
              val unfoldedType = fixpointType match {
                case Type.Fixpoint(fixpointName, fixpointBody) => fixpointBody.subst(fixpointName, fixpointType)
                case _ => fixpointType
              }
              // Cast the value to the unfolded type
              value.cast(unfoldedType) match {
                case Some(castedValue) => done(castedValue)
                case None => throw new RuntimeException(s"Cannot unfold $value to type $unfoldedType")
              }
            }
          }
        } yield result
      }

      case Term.Do(exprTerm, bodyTerm) => for {
        _ <- exprTerm.evalTramp  // Evaluate expression for side effects
        result <- bodyTerm.evalTramp  // Return the body value
      } yield result

      case Term.RefAddr(refType, address) => {
        // Reference addresses are runtime values that cannot be further evaluated
        done(Value.Primitive(PrimitiveValue.IntValue(BigInt(address))))
      }

      case Term.NativeFunctionCall(nativeFunction, args) => {
        def evalArgs(remaining: Seq[Term], accumulated: Seq[Value]): TailRec[Value] = {
          if remaining.isEmpty then {
            if accumulated.length == nativeFunction.arity then {
              // Full application: call the native function
              done(nativeFunction.call(accumulated))
            } else {
              // Partial application: return a closure
              done(Value.Neutral(NeutralValue.NativeCall(nativeFunction, accumulated.toList)))
            }
          } else {
            val head = remaining.head
            val tail = remaining.tail
            for {
              headValue <- head.evalTramp
              result <- tailcall(evalArgs(tail, accumulated :+ headValue))
            } yield result
          }
        }
        tailcall(evalArgs(args, Seq.empty))
      }

      case Term.NativeProcedureCall(nativeProcedure, args) => {
        // Native procedures are similar to native functions but may have side effects
        // They are only evaluated in full eval mode
        def evalArgs(remaining: Seq[Term], accumulated: Seq[Value]): TailRec[Value] = {
          if remaining.isEmpty then {
            if accumulated.length == nativeProcedure.arity then {
              // Full application: call the native procedure
              done(nativeProcedure.call(accumulated))
            } else {
              // Partial application: return a closure
              done(Value.Neutral(NeutralValue.NativeCall(nativeProcedure, accumulated.toList)))
            }
          } else {
            val head = remaining.head
            val tail = remaining.tail
            for {
              headValue <- head.evalTramp
              result <- tailcall(evalArgs(tail, accumulated :+ headValue))
            } yield result
          }
        }
        tailcall(evalArgs(args, Seq.empty))
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
    ): Option[TailRec[Value]] = {
      val argType = arg.infer(using env)
      value match {
        case Value.Closure(closureEnv, param, paramType, body, returnType, isCoe) => {
          // Check if argument type matches parameter type
          if argType <:< paramType then {
            // Extend the closure environment with the argument
            val extendedEnv = closureEnv.addValueVar(param, arg)
            // Evaluate the body in the extended environment
            Some(for {
              result <- body.evalTramp(using extendedEnv, unfoldingSuppressor)
              castedResult <- result.cast(returnType) match {
                case Some(casted) => done(casted)
                case None => throw new RuntimeException(s"Type cast error: cannot cast result $result to return type $returnType")
              }
            } yield castedResult)
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
          Some(for {
            fn <- body.evalTramp(using newEnv, unfoldingSuppressor)
            result <- fn.applyTo(arg)(using env, unfoldingSuppressor) match {
              case Some(computation) => computation
              case None => throw new RuntimeException(s"Cannot apply fixpoint result $fn to argument $arg")
            }
          } yield result)
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
            Some(done(function.call(args)))
          } else {
            // Partial application
            Some(done(Value.Neutral(NeutralValue.NativeCall(function, args))))
          }
        }

        case Value.Neutral(NeutralValue.Merge(left, right)) => {
          val leftApplication = Value.Neutral(left).applyTo(arg)(using env, unfoldingSuppressor)
          val rightApplication = right.applyTo(arg)(using env, unfoldingSuppressor)
          (leftApplication, rightApplication) match {
            case (Some(leftComp), Some(rightComp)) => {
              Some(for {
                leftResult <- leftComp
                rightResult <- rightComp
              } yield leftResult.merge(rightResult)(using env))
            }
            case (Some(leftComp), None) => Some(leftComp)
            case (None, Some(rightComp)) => Some(rightComp)
            case (None, None) => None
          }
        }

        case Value.Merge(values) => {
          val applications = values.flatMap { v =>
            v.applyTo(arg)(using env, unfoldingSuppressor)
          }
          if applications.isEmpty then None
          else Some(done(applications.map(_.result).reduce((left, right) => left.merge(right)(using env))))
        }

        case Value.Neutral(nv) => Some(done(Value.Neutral(NeutralValue.Apply(nv, arg))))

        case _ => None
      }
    }
  }
}

