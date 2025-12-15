package cp.interpreter

import cp.common.Environment
import cp.core.{Clause, MergeBias, Module, NeutralValue, PrimitiveType, PrimitiveValue, Term, Type, Value}
import cp.core.{matchValue, matchBindingTypes}
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

      case Term.Lambda(param, paramType, body, isCoe) => for {
        termEnv <- {
          val valueEntries = env.values.toList
          def buildTermEnv(remaining: List[(String, Value)], acc: Environment[String, Type, Term]): WorkList[BigStepTask, Environment[String, Type, Term]] = {
            remaining match {
              case Nil => Completed(acc.addValueVar(param, Term.Annotated(Term.Var(param), paramType)))
              case (name, value) :: rest => for {
                valueType <- TaskNode(InferValue(value)(using env))
                result <- buildTermEnv(rest, acc.addValueVar(name, Term.Annotated(Term.Var(name), valueType)))
              } yield result
            }
          }
          buildTermEnv(valueEntries, Environment[String, Type, Term](types = env.types, values = Map.empty))
        }
        returnType <- TaskNode(InferTerm(body)(using termEnv))
        result = Value.Closure(env, param, paramType.normalize, body, returnType.normalize, isCoe)
      } yield result

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
            val results = values.values.flatMap(projectFromValue)
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

      case Term.TypeLambda(typeParam, body) => for {
        termEnv <- {
          val valueEntries = env.values.toList
          def buildTermEnv(remaining: List[(String, Value)], acc: Environment[String, Type, Term]): WorkList[BigStepTask, Environment[String, Type, Term]] = {
            remaining match {
              case Nil => Completed(acc.addTypeVar(typeParam, Type.Var(typeParam)))
              case (name, value) :: rest => for {
                valueType <- TaskNode(InferValue(value)(using env))
                result <- buildTermEnv(rest, acc.addValueVar(name, Term.Annotated(Term.Var(name), valueType)))
              } yield result
            }
          }
          buildTermEnv(valueEntries, Environment[String, Type, Term](types = env.types, values = Map.empty))
        }
        returnType <- TaskNode(InferTerm(body)(using termEnv))
        result = Value.TypeClosure(env, typeParam, body, returnType)
      } yield result

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
      val argType = TaskNode(InferValue(arg)(using env)).run
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
            case Type.Arrow(paramType, returnType, _) if argType <:< paramType =>
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
            case _ => None
          }
        }

        case Value.Neutral(func @ NeutralValue.NativeCall(function, prevArgs)) => {
          function.paramTypes.lift(prevArgs.length) match {
            case Some(paramType) if argType <:< paramType =>
              val args = prevArgs :+ arg
              if args.length == function.arity && !args.exists(_.isNeutral) then {
                // Last argument, perform full application
                Some(Completed(function.call(args)(using env)))
              } else {
                // Partial application
                Some(Completed(Value.Neutral(NeutralValue.NativeCall(function, args))))
              }
            case _ => None
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
          val applications = values.values.flatMap { v => v.applyTo(arg) }
          if applications.isEmpty then None
          else Some(Completed(applications.map(_.run).reduce((left, right) => left.merge(right)(using env))))
        }

        case Value.Neutral(nv) => Some(Completed(Value.Neutral(NeutralValue.Apply(nv, arg))))

        case _ => None
      }
    }
  }

  private case class InferTerm(term: Term)(
    using termEnv: Environment[String, Type, Term]
  ) extends BigStepTask[Type] {
    override def step: WorkList[BigStepTask, Type] = {
      val inferredType: WorkList[BigStepTask, Type] = term match {
        case Term.Var(name) => termEnv.values.get(name) match {
          case Some(term) => TaskNode(InferTerm(term))
          case None => throw new RuntimeException(s"Variable '$name' is not bound in the environment.")
        }

        case Term.Symbol(_, ty) => Completed(ty)

        case Term.Annotated(_, ty) => Completed(ty)

        case Term.Primitive(value) => Completed(Type.Primitive(value.ty))

        case Term.Apply(func, arg) => for {
          fnType <- TaskNode(InferTerm(func))
          argType <- TaskNode(InferTerm(arg))
          result <- (fnType match {
            case Type.Arrow(paramType, returnType, _) =>
              if !(argType <:< paramType) then {
                throw new RuntimeException(s"Expected argument type: ${paramType}, but got: ${argType}")
              } else Completed(returnType)
            case fnType @ Type.Intersection(_, _) =>
              fnType.testApplicationReturn(argType) match {
                case Some(returnType) => Completed(returnType)
                case None => throw new RuntimeException(
                  s"Function type ${fnType} cannot be applied to argument type ${argType}"
                )
              }
            case other => throw new RuntimeException(s"Expected function type, but got: ${other}")
          }): WorkList[BigStepTask, Type]
        } yield result

        case Term.TypeApply(term, tyArg) => for {
          termType <- TaskNode(InferTerm(term))
          result <- (termType match {
            case Type.Forall(param, body, _) => Completed(body.subst(param, tyArg))
            case other => throw new RuntimeException(s"Expected polymorphic type, but got: ${other}")
          }): WorkList[BigStepTask, Type]
        } yield result

        case Term.Lambda(param, paramType, body, isCoe) => {
          val extendedEnv = termEnv.addValueVar(param, Term.Annotated(Term.Var(param), paramType))
          for {
            bodyType <- TaskNode(InferTerm(body)(using extendedEnv))
            result = Type.Arrow(paramType, bodyType, isCoe)
          } yield result
        }

        case Term.TypeLambda(param, body) => {
          val extendedEnv = termEnv.addTypeVar(param, Type.Var(param))
          for {
            bodyType <- TaskNode(InferTerm(body)(using extendedEnv))
            result = Type.Forall(param, bodyType)
          } yield result
        }

        case Term.Fixpoint(name, ty, body) => {
          val extendedEnv = termEnv.addValueVar(name, Term.Annotated(Term.Var(name), ty))
          for {
            bodyType <- TaskNode(InferTerm(body)(using extendedEnv))
            result <- ({
              if !(bodyType <:< ty) then {
                throw new RuntimeException(
                  s"Body type `${bodyType}` does not match annotated type `${ty}` in fixpoint"
                )
              } else Completed(bodyType)
            }): WorkList[BigStepTask, Type]
          } yield result
        }

        case Term.Projection(record, field) => for {
          recordType <- TaskNode(InferTerm(record))
          result <- (recordType match {
            case Type.Record(fieldTypes) => fieldTypes.get(field) match {
              case Some(fieldType) => Completed(fieldType)
              case None => throw new RuntimeException(
                s"Field '${field}' does not exist in record type"
              )
            }
            case other => throw new RuntimeException(s"Expected record type, but got: ${other}")
          }): WorkList[BigStepTask, Type]
        } yield result

        case Term.Record(fields) => {
          if fields.isEmpty then Completed(Type.unit)
          else {
            val fieldList = fields.toList
            def inferFields(remaining: List[(String, Term)], acc: Map[String, Type]): WorkList[BigStepTask, Type] = {
              remaining match {
                case Nil => Completed(Type.Record(acc))
                case (name, term) :: rest => for {
                  fieldType <- TaskNode(InferTerm(term))
                  result <- inferFields(rest, acc + (name -> fieldType))
                } yield result
              }
            }
            inferFields(fieldList, Map.empty)
          }
        }

        case Term.Tuple(elements) => {
          if elements.isEmpty then Completed(Type.unit)
          else {
            def inferElements(remaining: List[Term], acc: List[Type]): WorkList[BigStepTask, Type] = {
              remaining match {
                case Nil => Completed(Type.Tuple(acc))
                case head :: tail => for {
                  elementType <- TaskNode(InferTerm(head))
                  result <- inferElements(tail, acc :+ elementType)
                } yield result
              }
            }
            inferElements(elements, List.empty)
          }
        }

        case Term.Merge(left, right, MergeBias.Left) =>
          TaskNode(InferTerm(Term.Merge(left, Term.Diff(right, left), MergeBias.Neutral)))

        case Term.Merge(left, right, MergeBias.Right) =>
          TaskNode(InferTerm(Term.Merge(Term.Diff(left, right), right, MergeBias.Neutral)))

        case Term.Merge(left, right, MergeBias.Neutral) => for {
          leftType <- TaskNode(InferTerm(left))
          rightType <- TaskNode(InferTerm(right))
          result = leftType merge rightType
        } yield result

        case Term.Diff(left, right) => for {
          leftType <- TaskNode(InferTerm(left))
          rightType <- TaskNode(InferTerm(right))
          result = leftType diff rightType
        } yield result

        case Term.IfThenElse(_, thenBranch, elseBranch) => for {
          thenType <- TaskNode(InferTerm(thenBranch))
          elseType <- TaskNode(InferTerm(elseBranch))
          result <- ({
            if thenType == elseType then Completed(thenType)
            else if thenType <:< elseType then Completed(elseType)
            else if elseType <:< thenType then Completed(thenType)
            else throw new RuntimeException(
              s"Branches of IfThenElse have incompatible types: ${thenType} and ${elseType}"
            )
          }): WorkList[BigStepTask, Type]
        } yield result

        case Term.Match(scrutinee, clauses) => {
          if clauses.isEmpty then {
            throw new RuntimeException("Match expression must have at least one clause")
          }
          for {
            scrutineeType <- TaskNode(InferTerm(scrutinee))
            clauseTypes <- {
              def inferClauses(remaining: List[Clause[Type, Term]], acc: List[Type]): WorkList[BigStepTask, List[Type]] = {
                remaining match {
                  case Nil => Completed(acc)
                  case clause :: rest => {
                    val bindings = clause.patterns.head.matchBindingTypes(scrutineeType)(using termEnv)
                    val extendedEnv = bindings.foldLeft(termEnv) { case (accEnv, (name, ty)) =>
                      accEnv.addValueVar(name, Term.Annotated(Term.Var(name), ty))
                    }
                    for {
                      clauseType <- TaskNode(InferTerm(clause.body)(using extendedEnv))
                      result <- inferClauses(rest, acc :+ clauseType)
                    } yield result
                  }
                }
              }
              inferClauses(clauses, List.empty)
            }
            result <- ({
              val commonType = clauseTypes.reduce { (ty1, ty2) =>
                if ty1 == ty2 then ty1
                else if ty1 <:< ty2 then ty2
                else if ty2 <:< ty1 then ty1
                else throw new RuntimeException(
                  s"Match clauses have incompatible types: ${ty1} and ${ty2}"
                )
              }
              Completed(commonType)
            }): WorkList[BigStepTask, Type]
          } yield result
        }

        case Term.ArrayLiteral(elements) => {
          if elements.isEmpty then {
            Completed(Type.Array(Type.Primitive(PrimitiveType.TopType)))
          } else {
            def inferElements(remaining: List[Term], accType: Option[Type]): WorkList[BigStepTask, Type] = {
              remaining match {
                case Nil => accType match {
                  case Some(ty) => Completed(Type.Array(ty))
                  case None => Completed(Type.Array(Type.Primitive(PrimitiveType.TopType)))
                }
                case head :: tail => for {
                  elementType <- TaskNode(InferTerm(head))
                  commonType = accType match {
                    case None => elementType
                    case Some(ty) =>
                      if ty == elementType then ty
                      else if ty <:< elementType then elementType
                      else if elementType <:< ty then ty
                      else throw new RuntimeException(
                        s"Array elements have incompatible types: ${ty} and ${elementType}"
                      )
                  }
                  result <- inferElements(tail, Some(commonType))
                } yield result
              }
            }
            inferElements(elements, None)
          }
        }

        case Term.Do(expr, body) => for {
          _ <- TaskNode(InferTerm(expr))
          bodyType <- TaskNode(InferTerm(body))
        } yield bodyType

        case Term.RefAddr(refType, address) => Completed(refType)

        case Term.NativeFunctionCall(function, args) => {
          if args.length > function.arity then {
            throw new RuntimeException(
              s"Too many arguments for native function: expected at most ${function.arity}, got ${args.length}"
            )
          }
          val paramTypes = function.paramTypes
          def checkArgs(remaining: List[(Term, Type)]): WorkList[BigStepTask, Unit] = {
            remaining match {
              case Nil => Completed(())
              case (arg, paramType) :: rest => for {
                argType <- TaskNode(InferTerm(arg))
                _ <- ({
                  if !(argType <:< paramType) then {
                    throw new RuntimeException(s"Expected argument type: ${paramType}, but got: ${argType}")
                  } else Completed(())
                }): WorkList[BigStepTask, Unit]
                result <- checkArgs(rest)
              } yield result
            }
          }
          for {
            _ <- checkArgs(args.zip(paramTypes).toList)
            result = {
              if args.length == function.arity then {
                function.returnType
              } else {
                paramTypes.drop(args.length).foldRight(function.returnType) {
                  (paramType, acc) => Type.Arrow(paramType, acc)
                }
              }
            }
          } yield result
        }

        case Term.NativeProcedureCall(procedure, args) => {
          if args.length > procedure.arity then {
            throw new RuntimeException(
              s"Too many arguments for native procedure: expected at most ${procedure.arity}, got ${args.length}"
            )
          }
          val paramTypes = procedure.paramTypes
          def checkArgs(remaining: List[(Term, Type)]): WorkList[BigStepTask, Unit] = {
            remaining match {
              case Nil => Completed(())
              case (arg, paramType) :: rest => for {
                argType <- TaskNode(InferTerm(arg))
                _ <- ({
                  if !(argType <:< paramType) then {
                    throw new RuntimeException(s"Expected argument type: ${paramType}, but got: ${argType}")
                  } else Completed(())
                }): WorkList[BigStepTask, Unit]
                result <- checkArgs(rest)
              } yield result
            }
          }
          for {
            _ <- checkArgs(args.zip(paramTypes).toList)
            result = {
              if args.length == procedure.arity then {
                procedure.returnType
              } else {
                paramTypes.drop(args.length).foldRight(procedure.returnType) {
                  (paramType, acc) => Type.Arrow(paramType, acc)
                }
              }
            }
          } yield result
        }
      }

      // Normalize the inferred type
      for {
        ty <- inferredType
        normalized = ty.normalize(using termEnv.typeEnv)
      } yield normalized
    }
  }

  private case class InferValue(value: Value)(
    using env: Env
  ) extends BigStepTask[Type] {
    override def step: WorkList[BigStepTask, Type] = value match {
      case Value.Primitive(primitiveValue) => Completed(Type.Primitive(primitiveValue.ty))

      case Value.Closure(_, param, paramType, _, returnType, _) =>
        Completed(Type.Arrow(paramType, returnType))

      case Value.TypeClosure(_, typeParam, _, returnType) =>
        Completed(Type.Forall(typeParam, returnType, Set.empty))

      case Value.Record(fields) => {
        val fieldNames = fields.keys.toList
        def inferFields(remaining: List[String], acc: Map[String, Type]): WorkList[BigStepTask, Type] = {
          remaining match {
            case Nil => Completed(Type.Record(acc))
            case name :: rest => for {
              fieldType <- TaskNode(InferValue(fields(name)))
              result <- inferFields(rest, acc + (name -> fieldType))
            } yield result
          }
        }
        inferFields(fieldNames, Map.empty)
      }

      case Value.Tuple(elements) => {
        def inferElements(remaining: List[Value], acc: List[Type]): WorkList[BigStepTask, Type] = {
          remaining match {
            case Nil => Completed(Type.Tuple(acc))
            case head :: tail => for {
              elementType <- TaskNode(InferValue(head))
              result <- inferElements(tail, acc :+ elementType)
            } yield result
          }
        }
        inferElements(elements, List.empty)
      }

      case Value.Array(elements) if elements.nonEmpty => for {
        elementType <- TaskNode(InferValue(elements.head))
        result = Type.Array(elementType)
      } yield result

      case Value.Array(_) => Completed(Type.Array(Type.Primitive(PrimitiveType.TopType)))

      case Value.FixThunk(_, annotatedType, _, _) => Completed(annotatedType)

      case Value.Merge(values) => {
        val valueList = values.values.toList
        def inferMerge(remaining: List[Value], acc: Option[Type]): WorkList[BigStepTask, Type] = {
          remaining match {
            case Nil => acc match {
              case Some(ty) => Completed(ty)
              case None => throw new RuntimeException("Empty merge set")
            }
            case head :: tail => for {
              headType <- TaskNode(InferValue(head))
              result <- inferMerge(tail, Some(acc match {
                case Some(accType) => Type.Intersection(accType, headType)
                case None => headType
              }))
            } yield result
          }
        }
        inferMerge(valueList, None)
      }

      case Value.Neutral(neutralValue) => TaskNode(InferNeutral(neutralValue))
    }
  }

  private case class InferNeutral(neutralValue: NeutralValue)(
    using env: Env
  ) extends BigStepTask[Type] {
    override def step: WorkList[BigStepTask, Type] = neutralValue match {
      case NeutralValue.Var(name) => env.getValue(name) match {
        case Some(value) => TaskNode(InferValue(value))
        case None => throw new RuntimeException(s"Unbound variable in neutral value: $name")
      }

      case NeutralValue.Apply(func, _) => for {
        funcType <- TaskNode(InferNeutral(func))
        result <- funcType match {
          case Type.Arrow(_, returnType, _) => Completed(returnType)
          case other => throw new RuntimeException(s"Attempted to apply a non-function type: $other")
        }
      } yield result

      case NeutralValue.Project(record, field) => for {
        recordType <- TaskNode(InferNeutral(record))
        result <- recordType match {
          case Type.Record(fields) => fields.get(field) match {
            case Some(fieldType) => Completed(fieldType)
            case None => throw new RuntimeException(s"Field '$field' not found in record type")
          }
          case other => throw new RuntimeException(s"Attempted to project a field from a non-record type: $other")
        }
      } yield result

      case NeutralValue.Merge(left, right) => for {
        leftType <- TaskNode(InferNeutral(left))
        rightType <- TaskNode(InferValue(right))
        result = Type.Intersection(leftType, rightType)
      } yield result

      case NeutralValue.Annotated(_, annotatedType) => Completed(annotatedType)

      case NeutralValue.NativeCall(function, args) => {
        val remainingParams = function.paramTypes.drop(args.length)
        if remainingParams.isEmpty then {
          Completed(function.returnType)
        } else {
          val arrowType = remainingParams.foldRight(function.returnType) {
            (paramType, accType) => Type.Arrow(paramType, accType, function.isPure)
          }
          Completed(arrowType)
        }
      }

      case NeutralValue.UnfoldingThunk(_, annotatedType, _, _) => Completed(annotatedType)
    }
  }
}
