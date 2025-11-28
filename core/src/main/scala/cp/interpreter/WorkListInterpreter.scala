package cp.interpreter

import cp.common.Environment
import cp.core.{MergeBias, Module, NeutralValue, PrimitiveType, PrimitiveValue, Term, Type, Value}
import cp.core.matchValue
import cp.util.{WorkList, Workable}
import cp.util.WorkList.*

class WorkListInterpreter(initialModules: Module*) extends Interpreter(initialModules*) {

  sealed trait BigStepTask[T] {
    def step: WorkList[BigStepTask, T]
  }

  given Workable[BigStepTask] with {
    // We don't have to implement `completed` for big-step evaluation
    //  because the evaluation always finishes in one step.
    // For small-step evaluation, we would need to implement this method.
    def completed[A](task: BigStepTask[A]): Option[A] = None
    def step[A](task: BigStepTask[A]): WorkList[BigStepTask, A] = task.step
  }

  override def eval(term: Term)(using env: Env): Value = {
    WorkList.TaskNode(Eval(term)(using env, UnfoldingSuppressor())).run
  }

  private case class Eval(term: Term)(
    using env: Env, unfoldingSuppressor: UnfoldingSuppressor
  ) extends BigStepTask[Value] {

    override def step: WorkList[BigStepTask, Value] = term match {

      case Term.Var(name) => env.getValue(name) match {
        case Some(value) => Completed(value)
        case None => throw new RuntimeException(s"Variable $name not found in environment")
      }

      case Term.Primitive(value) => Completed(Value.Primitive(value))

      case Term.Lambda(param, paramType, body, isCoe) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }.addValueVar(param, Term.Annotated(Term.Var(param), paramType))
        val returnType = body.infer(using termEnv).normalize
        Completed(Value.Closure(env, param, paramType.normalize, body, returnType, isCoe))
      }

      case Term.Apply(fnTerm, argTerm) => for {
        fnValue <- TaskNode(Eval(fnTerm))
        argValue <- TaskNode(Eval(argTerm))
        result <- fnValue.applyTo(argValue) match {
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
        TaskNode(Eval(body)(using newEnv, unfoldingSuppressor))
      }

      case Term.Record(fields) => {
        def evalFields(rest: List[(String, Term)], acc: Map[String, Value]): WorkList[BigStepTask, Value] = rest match {
          case Nil => Completed(Value.Record(acc))
          case (name, term) :: rest => for {
            value <- TaskNode(Eval(term))
            result <- evalFields(rest, acc + (name -> value))
          } yield result
        }
        evalFields(fields.toList, Map.empty)
      }

      case Term.Projection(recordTerm, field) => {
        def projectFromValue(value: Value): Option[WorkList[BigStepTask, Value]] = value match {
          case Value.Record(fields) => fields.get(field).map(Completed(_))
          case Value.Merge(values) => {
            // Try to project from each value in the merge set
            val results = values.flatMap(projectFromValue)
            if results.isEmpty then None
            else Some(Completed(results.map(_.run).reduce((left, right) => left.merge(right)(using env))))
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
          case Value.Neutral(nv) => Some(Completed(Value.Neutral(NeutralValue.Project(nv, field))))
          case fixThunk @ Value.FixThunk(fixEnv, annotatedType, name, body) => {
            // Evaluate the fix thunk first
            val fixValue = if unfoldingSuppressor.isSuppressed(fixThunk -> field) then {
              // Unfolding is suppressed for this fixpoint projection
              Value.Neutral(NeutralValue.UnfoldingThunk(fixEnv, annotatedType, name, body))
            } else fixThunk
            val envWithFix = fixEnv.addValueVar(name, fixValue)
            Some(for {
              projectValue <- TaskNode(Eval(body)(using envWithFix, unfoldingSuppressor + (fixThunk, field)))
              result <- projectFromValue(projectValue) match {
                case Some(comp) => comp
                case None => throw new RuntimeException(s"Field $field not found in record")
              }
            } yield result)
          }
          case _ => None
        }

        for {
          recordValue <- TaskNode(Eval(recordTerm))
          result <- projectFromValue(recordValue) match {
            case Some(comp) => comp
            case None => throw new RuntimeException(s"Field $field not found in record $recordValue")
          }
        } yield result
      }

      case Term.Symbol(symbolName, symbolType) => modules.get(symbolName.namespace) match {
        case Some(module) => module.terms.get(symbolName.localName) match {
          case Some(term) => TaskNode(Eval(term))
          case None => throw new RuntimeException(s"Term ${symbolName.localName} not found in module ${symbolName.namespace}")
        }
        case None => throw new RuntimeException(s"Module ${symbolName.namespace} not found")
      }

      case Term.Annotated(annotatedTerm, annotationType) => for {
        termValue <- TaskNode(Eval(annotatedTerm))
        result <- termValue.cast(annotationType.normalize)(using env) match {
          case Some(castedValue) => Completed(castedValue)
          case None => throw new RuntimeException(s"Cannot cast value $termValue to type $annotationType")
        }: WorkList[BigStepTask, Value]
      } yield result

      case Term.TypeApply(polymorphicTerm, typeArgument) => for {
        polymorphicValue <- TaskNode(Eval(polymorphicTerm))
        result <- polymorphicValue match {
          case Value.TypeClosure(closureEnv, typeParam, body, _) =>
            val expandedTypeArg = typeArgument.normalize // Type expansion would happen here if needed
            TaskNode(Eval(body)(using closureEnv.addTypeVar(typeParam, expandedTypeArg), unfoldingSuppressor))
          case other => throw new RuntimeException(s"Runtime type error: expected type-polymorphic value, got $other")
        }
      } yield result

      case Term.TypeLambda(typeParam, body) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }
        val returnType = body.infer(using termEnv.addTypeVar(typeParam, Type.Var(typeParam)))
        Completed(Value.TypeClosure(env, typeParam, body, returnType))
      }

      case Term.Tuple(elements) => {
        def evalElements(remaining: List[Term], accumulated: List[Value]): WorkList[BigStepTask, Value] = remaining match {
          case Nil => Completed(Value.Tuple(accumulated))
          case head :: tail => for {
            headValue <- TaskNode(Eval(head))
            result <- evalElements(tail, accumulated :+ headValue)
          } yield result
        }
        evalElements(elements, List.empty)
      }

      case Term.Merge(leftTerm, rightTerm, mergeBias) => mergeBias match {
        case MergeBias.Left => for {
          leftValue <- TaskNode(Eval(leftTerm))
          rightValue <- TaskNode(Eval(rightTerm))
          diffValue = rightValue.diff(leftValue)(using env)
          result = leftValue.merge(diffValue)(using env)
        } yield result

        case MergeBias.Right => for {
          leftValue <- TaskNode(Eval(leftTerm))
          rightValue <- TaskNode(Eval(rightTerm))
          diffValue = leftValue.diff(rightValue)(using env)
          result = diffValue.merge(rightValue)(using env)
        } yield result

        case MergeBias.Neutral => for {
          leftValue <- TaskNode(Eval(leftTerm))
          rightValue <- TaskNode(Eval(rightTerm))
          result = leftValue.merge(rightValue)(using env)
        } yield result
      }

      case Term.Diff(leftTerm, rightTerm) => for {
        leftValue <- TaskNode(Eval(leftTerm))
        rightValue <- TaskNode(Eval(rightTerm))
        result = leftValue.diff(rightValue)(using env)
      } yield result

      case Term.IfThenElse(conditionTerm, thenBranch, elseBranch) => for {
        conditionValue <- TaskNode(Eval(conditionTerm))
        result <- conditionValue match {
          case Value.Primitive(PrimitiveValue.BoolValue(true)) => TaskNode(Eval(thenBranch))
          case Value.Primitive(PrimitiveValue.BoolValue(false)) => TaskNode(Eval(elseBranch))
          case other => throw new RuntimeException(s"Runtime type error: condition must be boolean, got $other")
        }
      } yield result

      case Term.Match(scrutineeTerm, clauses) => for {
        scrutineeValue <- TaskNode(Eval(scrutineeTerm))
        result <- {
          // Try each clause in order until one matches
          val matchedClause = clauses.find { clause =>
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
              TaskNode(Eval(clause.body)(using extendedEnv, unfoldingSuppressor))
            case None =>
              throw new RuntimeException(s"No matching pattern for value: $scrutineeValue")
          }
        }
      } yield result

      case Term.ArrayLiteral(elements) => {
        def evalElements(
          remaining: List[Term], accumulated: List[Value]
        ): WorkList[BigStepTask, Value] = remaining match {
          case Nil => Completed(Value.Array(accumulated))
          case head :: tail => for {
            headValue <- TaskNode(Eval(head))
            result <- evalElements(tail, accumulated :+ headValue)
          } yield result
        }
        evalElements(elements, List.empty)
      }

      case Term.Do(exprTerm, bodyTerm) => for {
        _ <- TaskNode(Eval(exprTerm))  // Evaluate expression for side effects
        result <- TaskNode(Eval(bodyTerm))  // Return the body value
      } yield result

      case Term.RefAddr(refType, address) => {
        // Reference addresses are runtime values that cannot be further evaluated
        Completed(Value.Primitive(PrimitiveValue.IntValue(BigInt(address))))
      }

      case Term.NativeFunctionCall(nativeFunction, args) => {
        def evalArgs(remaining: Seq[Term], accumulated: Seq[Value]): WorkList[BigStepTask, Value] = {
          if remaining.isEmpty then {
            if accumulated.length == nativeFunction.arity then {
              // Full application: call the native function
              Completed(nativeFunction.call(accumulated)(using env))
            } else {
              // Partial application: return a closure
              Completed(Value.Neutral(NeutralValue.NativeCall(nativeFunction, accumulated.toList)))
            }
          } else {
            val head = remaining.head
            val tail = remaining.tail
            for {
              headValue <- TaskNode(Eval(head))
              result <- evalArgs(tail, accumulated :+ headValue)
            } yield result
          }
        }
        evalArgs(args, Seq.empty)
      }

      case Term.NativeProcedureCall(nativeProcedure, args) => {
        // Native procedures are similar to native functions but may have side effects
        // They are only evaluated in full eval mode
        def evalArgs(remaining: Seq[Term], accumulated: Seq[Value]): WorkList[BigStepTask, Value] = {
          if remaining.isEmpty then {
            if accumulated.length == nativeProcedure.arity then {
              // Full application: call the native procedure
              Completed(nativeProcedure.call(accumulated)(using env))
            } else {
              // Partial application: return a closure
              Completed(Value.Neutral(NeutralValue.NativeCall(nativeProcedure, accumulated.toList)))
            }
          } else {
            val head = remaining.head
            val tail = remaining.tail
            for {
              headValue <- TaskNode(Eval(head))
              result <- evalArgs(tail, accumulated :+ headValue)
            } yield result
          }
        }
        evalArgs(args, Seq.empty)
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
    ): Option[WorkList[BigStepTask, Value]] = {
      val argType = arg.infer(using env)
      value match {
        case Value.Closure(closureEnv, param, paramType, body, returnType, isCoe) => {
          // Check if argument type matches parameter type
          if argType <:< paramType then {
            // Extend the closure environment with the argument
            val extendedEnv = closureEnv.addValueVar(param, arg)
            // Evaluate the body in the extended environment
            Some(for {
              result <- TaskNode(Eval(body)(using extendedEnv, unfoldingSuppressor))
              castedResult <- result.cast(returnType)(using env) match {
                case Some(casted) => Completed(casted)
                case None => throw new RuntimeException(s"Type cast error: cannot cast result $result to return type $returnType")
              }: WorkList[BigStepTask, Value]
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
            fn <- TaskNode(Eval(body)(using newEnv, unfoldingSuppressor))
            result <- fn.applyTo(arg) match {
              case Some(computation) => computation
              case None => throw new RuntimeException(s"Runtime type error: cannot apply function to argument")
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
            Some(Completed(function.call(args)(using env)))
          } else {
            // Partial application
            Some(Completed(Value.Neutral(NeutralValue.NativeCall(function, args))))
          }
        }

        case Value.Neutral(NeutralValue.Merge(left, right)) => {
          val leftApplication = Value.Neutral(left).applyTo(arg)
          val rightApplication = right.applyTo(arg)
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
            v.applyTo(arg)
          }
          if applications.isEmpty then None
          else Some(Completed(applications.map(_.run).reduce((left, right) => left.merge(right)(using env))))
        }

        case Value.Neutral(nv) => Some(Completed(Value.Neutral(NeutralValue.Apply(nv, arg))))

        case _ => None
      }
    }
  }

  // TODO: For now, we just use the direct `infer/check` method in `Term` class.
  //  In the future, we can implement these as separate tasks to fit into the WorkList framework.
  //
  // case class Infer(term: Term)(using env: Env) extends BigStepTask[Type] {
  //   override def step[A]: WorkList[BigStepTask, A] = ???
  // }
  //
  // case class Check(term: Term, expectedType: Type)(using env: Env) extends BigStepTask[Unit] {
  //   override def step[A]: WorkList[BigStepTask, A] = ???
  // }
}
