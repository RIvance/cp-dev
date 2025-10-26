package cp.runtime

import cp.common.Environment
import cp.core.{MergeBias, Module, Namespace, NeutralValue, PrimitiveType, PrimitiveValue, Term, Type, Value}

import scala.util.control.TailCalls.{TailRec, done, tailcall}
import scala.util.{Failure, Success, Try}

class Interpreter(initialModules: Module*) {

  private type Env = Environment[String, Type, Value]

  private var modules: Map[Namespace, Module] = initialModules.map { mod => mod.namespace -> mod }.toMap

  def loadModule(module: Module): Unit = {
    modules += (module.namespace -> module)
  }

  implicit def globalEnvironment: Environment[String, Type, Term] = {
    modules.values.foldLeft(Environment.empty[String, Type, Term]) { (envAcc, module) =>
      envAcc.merge(module.importEnvironment)
    }
  }

  def check(term: Term, expectedType: Type): Boolean = term.check(expectedType)

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value = term.evalTramp.result

  extension (term: Term) {

    private def evalTramp(using env: Env): TailRec[Value] = term match {

      case Term.Var(name) => env.getValue(name) match {
        case Some(fix @ Value.FixThunk(captured, annotatedType, name, body)) =>
          // When a FixThunk is looked up, evaluate its body in the captured environment
          //  and since we cannot have self-reference when building the captured env,
          //  we now put the thunk itself for self-reference
          val unfoldedEnv = captured.addValueVar(name, fix)
          tailcall(body.evalTramp(using unfoldedEnv))
        case Some(value) => tailcall(done(value))
        case None => throw new RuntimeException(s"Variable $name not found in environment")
      }

      case Term.Primitive(value) => tailcall(done(Value.Primitive(value)))

      case Term.Lambda(param, paramType, body, isCoe) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }
        val returnType = body.infer(using termEnv).normalize
        tailcall(done(Value.Closure(env, param, paramType.normalize, body, returnType, isCoe)))
      }

      case Term.Apply(fnTerm, argTerm) => for {
        fnValue <- fnTerm.evalTramp
        argValue <- argTerm.evalTramp
        result <- fnValue.applyTo(argValue)(using env) match {
          case Some(computation) => tailcall(computation)
          case None => throw new RuntimeException(s"Runtime type error: cannot apply $fnValue to argument $argValue")
        }
      } yield result

      case Term.Fixpoint(name, annotatedType, body) => {
        // Create a thunk that will be used for self-reference
        lazy val newEnv = env.addValueVar(name, fixThunk)
        // Fixpoint itself will be added to the environment when evaluating the thunk
        lazy val fixThunk: Value = Value.FixThunk(env, annotatedType.normalize, name, body)
        tailcall(body.evalTramp(using newEnv))
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

      case Term.Projection(recordTerm, field) => recordTerm match {
        // Special case: projection from a merge term - distribute the projection
        case Term.Merge(leftTerm, rightTerm, _) => {
          // Try projecting from both sides first, and if both fail, evaluate normally
          val leftProjection  = Try { Term.Projection(leftTerm , field).evalTramp.result }
          val rightProjection = Try { Term.Projection(rightTerm, field).evalTramp.result }

          (leftProjection, rightProjection) match {
            case (Success(leftResult), Success(rightResult)) => tailcall(done(leftResult.merge(rightResult)))
            case (Success(leftResult), _) => tailcall(done(leftResult))
            case (_, Success(rightResult)) => tailcall(done(rightResult))
            case _ => {
              // Both failed, evaluate normally
              for {
                mergedValue <- recordTerm.evalTramp
                result <- mergedValue match {
                  case Value.Record(fields) => fields.get(field) match {
                    case Some(value) => tailcall(done(value))
                    case None => throw new RuntimeException(s"Field $field not found in record $mergedValue")
                  }
                  case Value.Neutral(nv) => tailcall(done(Value.Neutral(NeutralValue.Project(nv, field))))
                  case _ => throw new RuntimeException(s"Cannot project from non-record value $recordTerm")
                }
              } yield result
            }
          }
        }

        // Normal case: evaluate the record term first, then project
        case _ => {
          def projectFromValue(value: Value): TailRec[Value] = value match {
            case Value.Record(fields) => fields.get(field) match {
              case Some(fieldValue) => done(fieldValue)
              case None => throw new RuntimeException(s"Field $field not found in record $value")
            }
            case Value.Neutral(NeutralValue.Merge(left, right)) => {
              // Try to project from merge - try left first, then right
              Try(projectFromValue(left).result) match {
                case Success(result) => done(result)
                case Failure(_) => Try(projectFromValue(right).result) match {
                  case Success(result) => done(result)
                  case Failure(_) => done(Value.Neutral(NeutralValue.Project(NeutralValue.Merge(left, right), field)))
                }
              }
            }
            case Value.Neutral(nv) => done(Value.Neutral(NeutralValue.Project(nv, field)))
            case fixThunk: Value.FixThunk => {
              // Try projecting from the fixpoint body term directly
              val projectionTerm = Term.Projection(
                Term.Annotated(
                  Term.Apply(
                    Term.Lambda(fixThunk.name, fixThunk.annotatedType, fixThunk.body),
                    Term.Fixpoint(fixThunk.name, fixThunk.annotatedType, fixThunk.body)
                  ),
                  fixThunk.annotatedType
                ),
                field
              )
              val newEnv = fixThunk.env.addValueVar(fixThunk.name, fixThunk)
              tailcall(projectionTerm.evalTramp(using newEnv))
            }
            case _ => throw new RuntimeException(s"Cannot project from non-record value: $value")
          }

          for {
            recordValue <- recordTerm.evalTramp
            result <- tailcall(projectFromValue(recordValue))
          } yield result
        }
      }

      case Term.Symbol(symbolName, symbolType) => modules.get(symbolName.namespace) match {
        case Some(module) => module.terms.get(symbolName.localName) match {
          case Some(term) => tailcall(term.evalTramp(using env))
          case None => throw new RuntimeException(s"Term ${symbolName.localName} not found in module ${symbolName.namespace}")
        }
        case None => throw new RuntimeException(s"Module ${symbolName.namespace} not found")
      }

      case Term.Annotated(annotatedTerm, annotationType) => for {
        termValue <- annotatedTerm.evalTramp
        result <- termValue.cast(annotationType.normalize) match {
          case Some(castedValue) => tailcall(done(castedValue))
          case None => throw new RuntimeException(s"Cannot cast value $termValue to type $annotationType")
        }
      } yield result

      case Term.TypeApply(polymorphicTerm, typeArgument) => for {
        polymorphicValue <- polymorphicTerm.evalTramp
        expandedTypeArg <- tailcall(done(typeArgument.normalize)) // Type expansion would happen here if needed
        result <- polymorphicValue match {
          case Value.TypeClosure(closureEnv, typeParam, body, _) =>
            tailcall(body.evalTramp(using closureEnv.addTypeVar(typeParam, expandedTypeArg)))
          case other => throw new RuntimeException(s"Runtime type error: expected type-polymorphic value, got $other")
        }
      } yield result

      case Term.TypeLambda(typeParam, body) => {
        val termEnv: Environment[String, Type, Term] = env.mapValues {
          case (name, value) => Term.Annotated(Term.Var(name), value.infer(using env))
        }
        val returnType = body.infer(using termEnv.addTypeVar(typeParam, Type.Var(typeParam)))
        tailcall(done(Value.TypeClosure(env, typeParam, body, returnType)))
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
          diffValue <- tailcall(done(rightValue.diff(leftValue)(using env)))
          result <- tailcall(done(leftValue.merge(diffValue)(using env)))
        } yield result

        case MergeBias.Right => for {
          leftValue <- leftTerm.evalTramp
          rightValue <- rightTerm.evalTramp
          diffValue <- tailcall(done(leftValue.diff(rightValue)(using env)))
          result <- tailcall(done(diffValue.merge(rightValue)(using env)))
        } yield result

        case MergeBias.Neutral => for {
          leftValue <- leftTerm.evalTramp
          rightValue <- rightTerm.evalTramp
          result <- tailcall(done(leftValue.merge(rightValue)(using env)))
        } yield result
      }

      case Term.Diff(leftTerm, rightTerm) => for {
        leftValue <- leftTerm.evalTramp
        rightValue <- rightTerm.evalTramp
        result <- tailcall(done(leftValue.diff(rightValue)(using env)))
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
        result <- tailcall(done(bodyValue)) // Folding is essentially wrapping, represented by the value itself
      } yield result

      case Term.UnfoldFixpoint(fixpointType, foldedTerm) => {
        if fixpointType.isTopLike then tailcall(done(Value.Primitive(PrimitiveValue.UnitValue)))
        else for {
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
                case Some(castedValue) => tailcall(done(castedValue))
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
        tailcall(done(Value.Primitive(PrimitiveValue.IntValue(BigInt(address)))))
      }

      case Term.NativeFunctionCall(nativeFunction, args) => {
        def evalArgs(remaining: Seq[Term], accumulated: Seq[Value]): TailRec[Value] = {
          if remaining.isEmpty then {
            if accumulated.length == nativeFunction.arity then {
              // Full application: call the native function
              val result = nativeFunction.call(accumulated)
              tailcall(done(result))
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
              val result = nativeProcedure.call(accumulated)
              tailcall(done(result))
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
  extension (tpe: Type) {
    private def isTopLike: Boolean = tpe match {
      case Type.Primitive(PrimitiveType.TopType) => true
      case _ => false
    }
  }

  extension (value: Value) {
    def applyTo(arg: Value)(using env: Env): Option[TailRec[Value]] = ???
  }
}

