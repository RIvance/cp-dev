package cp.syntax

import cp.core.*
import cp.core.PrimitiveType.*
import cp.error.{CoreError, SpannedError, UnknownError}
import cp.error.CoreErrorKind.*
import cp.util.{OptionalSpanned, SourceSpan}

enum ExprTerm extends OptionalSpanned[ExprTerm] {
  
  private type Env = Environment[String, Type, Term]

  case Primitive(value: PrimitiveValue)

  case Var(name: String)

  case Typed(expr: ExprTerm, expectedType: ExprType)

  case Apply(fn: ExprTerm, args: List[ExprTerm])

  case Lambda(paramName: String, paramType: Option[ExprType], body: ExprTerm)

  // Λα.M -- type-level lambda abstraction
  case TypeLambda(paramName: String, body: ExprTerm)
  
  // fix f: A -> B = λx: A. M -- term-level lambda abstraction with recursion
  case Fixpoint(name: String, ty: ExprType, recursiveBody: ExprTerm)

  case IfThenElse(condition: ExprTerm, thenBranch: ExprTerm, elseBranch: ExprTerm)

  case LetIn(name: String, value: ExprTerm, ty: Option[ExprType], body: ExprTerm)

  case Record(fields: List[(String, RecordField)])

  case Tuple(elements: List[ExprTerm])

  case Merge(left: ExprTerm, right: ExprTerm, bias: MergeBias = MergeBias.Neutral)

  // case Match(scrutinee: ExprTerm, cases: List[(Pattern, ExprTerm)])

  case Projection(record: ExprTerm, field: String)

  case TypeApply(term: ExprTerm, tyArgs: List[ExprType])

  case OpenIn(record: ExprTerm, body: ExprTerm)

  case Update(record: ExprTerm, updates: Map[String, ExprTerm])

  case Trait(
    selfAnno: SelfAnnotation[ExprType],
    // `implements` are just for type checking, they do not appear in the runtime term.
    implements: Option[ExprType],
    inherits: Option[ExprTerm],
    body: ExprTerm,
  )

  case New(traitObject: ExprTerm)

  case Forward(left: ExprTerm, right: ExprTerm)

  case Exclude(left: ExprTerm, right: ExprType)

  case Remove(record: ExprTerm, fields: Set[String])

  case Diff(left: ExprTerm, right: ExprTerm)

  case Rename(record: ExprTerm, renames: Map[String, String])

  case FoldFixpoint(fixpointType: ExprType, body: ExprTerm)

  case UnfoldFixpoint(fixpointType: ExprType, term: ExprTerm)

  // See `cp.core.Term.Do` for explanation.
  case Do(expr: ExprTerm, body: ExprTerm)

  case ArrayLiteral(elements: List[ExprTerm])

  case Span(term: ExprTerm, span: SourceSpan)

  override def withSpan(span: SourceSpan): ExprTerm = this match {
    case ExprTerm.Span(_, _) => this
    case _ => ExprTerm.Span(this, span)
  }

  def synthesize(using env: Env)(
    using constraints: Set[Constraint[ExprType]] = Set.empty
  ): (Term, Type) = synthesize(using None)(using env)

  private def synthesize(using expectedType: Option[Type])(using env: Env)(
    using constraints: Set[Constraint[ExprType]]
  ): (Term, Type) = this match {
    case ExprTerm.Primitive(value) => {
      val ty = value match {
        case PrimitiveValue.IntValue(_) => PrimitiveType.IntType
        case PrimitiveValue.FloatValue(_) => PrimitiveType.FloatType
        case PrimitiveValue.BoolValue(_) => PrimitiveType.BoolType
        case PrimitiveValue.RuneValue(_) => PrimitiveType.RuneType
        case PrimitiveValue.StringValue(_) => PrimitiveType.StringType
        case PrimitiveValue.UnitValue => PrimitiveType.UnitType
      }
      (Term.Primitive(value), Type.Primitive(ty))
    }

    case ExprTerm.Var(name) => {
      env.values.get(name) match {
        case Some(term) => (term, term.infer)
        case None => UnresolvedReference.raise(s"Undefined symbol: $name")
      }
    }

    case ExprTerm.Typed(expr, expectedTypeExpr) => {
      val (term, ty) = expr.synthesize
      val expectedType = expectedTypeExpr.synthesize
      if !(ty <:< expectedType) then TypeNotMatch.raise {
        s"Expected type: $expectedType, found: $ty"
      } else (term, ty)
    }

    case ExprTerm.Apply(fn, args) => {
      val (fnTerm: Term, fnType: Type) = fn.synthesize
      val (fnAppTerm, fnAppType) = args.foldLeft((fnTerm, fnType)) {
        case ((accFnTerm: Term, accFnType: Type), arg) => {
          accFnType match {
            case Type.Arrow(domain, codomain) => {
              // When arg is a lambda without annotation, 
              //  we need to infer its type from accFnType
              val (argTerm: Term, argType: Type) = arg.synthesize(using Some(domain))
              if !argTerm.check(domain) then TypeNotMatch.raise {
                s"Expected argument type: $domain, found: $argType"
              }
              (Term.Apply(accFnTerm, argTerm), codomain)
            }
            case Type.Intersection(_, _) => {
              val (argTerm: Term, argType: Type) = arg.synthesize
              fnType.testApplicationReturn(argType) match {
                case Some(returnType) =>
                  if !argTerm.check(argType) then TypeNotMatch.raise {
                    s"Expected argument type: $argType, found: $argType"
                  } else (Term.Apply(accFnTerm, argTerm), returnType)
                case None => TypeNotMatch.raise {
                  s"Function application type mismatch: $accFnType cannot accept argument of type $argType"
                }
              }
            }
            case _ => TypeNotMatch.raise {
              s"Function application on a non-function type: $accFnType"
            }
          }
        }
      }
      if !fnAppTerm.check(fnAppType) then TypeNotMatch.raise {
        s"Function application term does not check against its type: $fnAppTerm : $fnAppType"
      } else (fnAppTerm, fnAppType)
    }

    case ExprTerm.Lambda(paramName, paramTypeOpt, body) => {
      val paramType: Type = paramTypeOpt match {
        case Some(tyExpr) => tyExpr.synthesize
        case None => expectedType match {
          case Some(Type.Arrow(domain, _)) => domain
          case _ => TypeNotMatch.raise {
            s"Cannot infer parameter type of lambda without annotation: $this"
          }
        }
      }
      env.withValueVar(paramName, Term.Typed(Term.Var(paramName), paramType)) { env =>
        val (bodyTerm, bodyType) = body.synthesize(using env)
        val lambdaTerm = Term.Lambda(paramName, paramType, bodyTerm)
        val lambdaType = Type.Arrow(paramType, bodyType)
        if !lambdaTerm.check(lambdaType) then TypeNotMatch.raise {
          s"Lambda term does not check against its type: $lambdaTerm : $lambdaType"
        } else (lambdaTerm, lambdaType)
      }
    }
    
    case ExprTerm.TypeLambda(paramName, body) => {
      env.withTypeVar(paramName, Type.Var(paramName)) { env =>
        val (bodyTerm, bodyType) = body.synthesize(using env)
        val typeLambdaTerm = Term.TypeLambda(paramName, bodyTerm)
        val typeLambdaType = Type.Forall(paramName, bodyType)
        if !typeLambdaTerm.check(typeLambdaType) then TypeNotMatch.raise {
          s"Type lambda term does not check against its type: $typeLambdaTerm : $typeLambdaType"
        } else (typeLambdaTerm, typeLambdaType)
      }
    }
      
    case ExprTerm.Fixpoint(name, tyExpr, recursiveBody) => {
      val ty = tyExpr.synthesize
      env.withValueVar(name, Term.Typed(Term.Var(name), ty)) { implicit env =>
        val (bodyTerm, bodyType) = recursiveBody.synthesize(using env)
        if !(bodyType <:< ty) then TypeNotMatch.raise {
          s"Fixpoint body type does not match annotated type: $bodyType vs $ty"
        }
        val fixTerm = Term.Fixpoint(name, ty, bodyTerm)
        if !fixTerm.check(ty) then TypeNotMatch.raise {
          s"Fixpoint term does not check against its type: $fixTerm : $ty"
        } else (fixTerm, ty)
      }
    }
    
    case ExprTerm.IfThenElse(condition, thenBranch, elseBranch) => {
      val (condTerm, condType) = condition.synthesize
      if !(condType unify Type.Primitive(PrimitiveType.BoolType)) then TypeNotMatch.raise {
        s"Condition of if-then-else must be of type Bool, but got: $condType"
      }
      val (thenTerm, thenType) = thenBranch.synthesize
      val (elseTerm, elseType) = elseBranch.synthesize
      if !(thenType <:< elseType) && !(elseType <:< thenType) then TypeNotMatch.raise {
        s"Branches of if-then-else must have compatible types, but got: $thenType and $elseType"
      }
      val resultType = if (thenType <:< elseType) elseType else thenType
      val ifTerm = Term.IfThenElse(condTerm, thenTerm, elseTerm)
      if !ifTerm.check(resultType) then TypeNotMatch.raise {
        s"If-then-else term does not check against its type: $ifTerm : $resultType"
      } else (ifTerm, resultType)
    }
    
    case ExprTerm.LetIn(name, value, tyExprOpt, body) => {
      val (valueTerm, valueType) = value.synthesize
      val ty = tyExprOpt match {
        case Some(tyExpr) => {
          val expectedType = tyExpr.synthesize
          if !valueTerm.check(expectedType) then TypeNotMatch.raise {
            s"Let-binding value type does not match annotated type: $valueType vs $expectedType"
          } else expectedType
        }
        case None => valueType
      }
      // Convert let-in to application of lambda
      env.withValueVar(name, Term.Typed(Term.Var(name), ty)) { env =>
        val (bodyTerm, bodyType) = body.synthesize(using env)
        val letTerm = Term.Apply(Term.Lambda(name, ty, bodyTerm), valueTerm)
        if !letTerm.check(bodyType) then TypeNotMatch.raise {
          s"Let-in term does not check against its type: $letTerm : $bodyType"
        } else (letTerm, bodyType)
      }
    }
    
    case ExprTerm.Record(fields) => {

      // Group fields by name to merge method patterns
      val fieldGroup: Map[String, List[RecordField]] = fields.groupMap(_._1)(_._2)
      val (
        valueFields: Map[String, (RecordFieldValue, Option[Type])],
        consFields: Map[String, (List[RecordFieldMethod], Option[Type])],
      ) = {
        fieldGroup.foldLeft((
          Map.empty[String, (RecordFieldValue, Option[Type])],
          Map.empty[String, (List[RecordFieldMethod], Option[Type])],
        )) {
          case ((values, cons), (fieldName, fieldList)) => {
            val (valueList, consList) = fieldList.partition(_.isInstanceOf[RecordFieldValue])

            // Error if multiple value fields for same name
            if (valueList.size > 1) {
              throw new RuntimeException(s"Multiple value bindings for field '$fieldName'")
            }

            // Error if both value and constructor fields for same name
            if (valueList.nonEmpty && consList.nonEmpty) {
              throw new RuntimeException(s"Field '$fieldName' has both value and constructor bindings")
            }

            val expectedFieldType = expectedType match {
              case Some(Type.Record(fieldTypes)) => fieldTypes.get(fieldName)
              case _ => None
            }

            // For value field, take the single value
            val updatedValues = valueList.headOption.map(v =>
              values + (fieldName -> (v.asInstanceOf[RecordFieldValue], expectedFieldType))
            ).getOrElse(values)

            // For constructor fields, merge into a list
            val updatedCons = if (consList.nonEmpty) {
              cons + (fieldName -> (consList.map(_.asInstanceOf[RecordFieldMethod]), expectedFieldType))
            } else cons

            (updatedValues, updatedCons)
          }
        }
      }

      val synthesizedValueFields: Map[String, (Term, Type)] = valueFields.map {
        case (fieldName, (RecordFieldValue(fieldExpr, _), expectedTypeOpt)) => {
          val (fieldTerm: Term, fieldType: Type) = fieldExpr.synthesize
          expectedTypeOpt match {
            case Some(expectedType) =>
              if !fieldTerm.check(expectedType) then TypeNotMatch.raise {
                s"Field '$fieldName' type does not match expected type: $fieldType vs $expectedType"
              } else (fieldName, (fieldTerm, expectedType))
            case None => (fieldName, (fieldTerm, fieldType))
          }
        }
      }

      // Merge method patterns into single function per field; e.g.,
      //  ```
      //  trait implements NegSig<Eval & Print> => {
      //    (Neg e).eval = -e.eval;
      //    (Neg e).print = "-(" ++ e.print ++ ")";
      //  };
      //  ```
      // becomes
      //  ```
      //  trait implements NegSig<Eval & Print> => {
      //    Neg = fun e -> new trait => {
      //      eval = -e.eval;
      //      print = "-(" ++ e.print ++ ")";
      //    };
      //  };
      val synthesizedConsFields: Map[String, (Term, Type)] = consFields.iterator.map {
        case (consName, (methodPatterns: List[RecordFieldMethod], expectedTypeOpt: Option[Type])) => {
          // e.g., For type `{ Neg: Eval&Print -> out Eval&Print };`,
          //  `Eval&Print -> out Eval&Print` is the expected method type
          val expectedConsTypeOpt = expectedTypeOpt match {
            case Some(Type.Record(expectedFieldTypes)) => expectedFieldTypes.get(consName)
            case _ => None
          }

          // We need to extract the expected parameter types and return type
          val expectedConsTypeListOpt: Option[(List[Type], Type)] = expectedTypeOpt.map(_.getParamAndReturnTypes)

          // e.g., For `{ Neg: Eval&Print -> out Eval&Print }`
          //  where `out Eval&Print = { eval: Int; print: String }`,
          //  `eval: Int` and `print: String` are the expected method parameter types
          val expectedMethodTypes: Map[String, Type] = expectedConsTypeListOpt.map {
            case (_, Type.Record(fieldTypes)) => fieldTypes
            case _ => Map.empty[String, Type]
          }.getOrElse(Map.empty[String, Type])

          // Now synthesize the constructor parameters
          //  the parameters are store in each method pattern
          //  for each method pattern, the parameters are expected to be the same
          //  so we can just take the first one
          // e.g., For `(Neg e).eval = -e.eval;` and `(Neg e).print = "-(" ++ e.print ++ ")";`
          //  the constructor parameter is always a single `e` with type `Eval&Print`
          //  (maybe with different names)
          // So we need to ensure that all method patterns have the same constructor parameters
          //  (with same types and maybe different names)
          // We firstly check whether the types are provided by the expected type passed in
          //  if so, we use those types
          //  otherwise, we expect the types to be provided in the method patterns
          // If we have to use the types from method patterns,
          //  we need to ensure that all method patterns have the same types
          val expectedConstructorParamTypes: List[Type] = expectedConsTypeListOpt match {
            case Some((paramTypes, _)) => paramTypes
            case None => {

              // Helper function to get parameter types from method parameters
              extension (params: Seq[(String, Option[ExprType])]) def getParamTypes: List[Type] = params.map {
                case (paramName, paramTypeOpt) => paramTypeOpt match {
                  case Some(tyExpr) => tyExpr.synthesize
                  case None => MissingTypeAnnotation.raise {
                    s"Cannot infer constructor parameter type for method pattern without annotation: $paramName in $this"
                  }
                }
              }.toList

              // We take the types from the first method pattern and check others against it
              // If the first method pattern has no type annotation, we cannot infer the types
              //  therefore raise an error
              // We can ensure that there is at least one method pattern
              val firstPatternParamTypes: List[Type] = methodPatterns.head.fieldParams.getParamTypes
              
              // Now check other method patterns against the first one
              methodPatterns.tail.foreach { pattern =>
                val patternParamTypes: List[Type] = pattern.fieldParams.getParamTypes
                if patternParamTypes.length != firstPatternParamTypes.length then TypeNotMatch.raise {
                  s"Constructor parameter count mismatch in method patterns for '$consName'"
                }
                patternParamTypes.zip(firstPatternParamTypes).foreach { (t1, t2) =>
                  if !(t1 unify t2) then TypeNotMatch.raise {
                    s"Constructor parameter type mismatch in method patterns for '$consName': $t1 vs $t2"
                  }
                }
              }
              // If all checks pass, we can use the first pattern's parameter types
              firstPatternParamTypes
            }
          }

          // In this step, we need to check whether the param names are the same
          //  if they are the same, we can use that name
          //  otherwise, we need to generate a fresh name and substitute it in the method bodies
          val allConstructorParamNames: List[String] = {
            val firstPatternParamNames: List[String] = methodPatterns.head.fieldParams.map(_._1).toList
            val allSame = methodPatterns.forall { pattern =>
              val paramNames: List[String] = pattern.fieldParams.map(_._1).toList
              paramNames == firstPatternParamNames
            }
            // For now, we require all param names to be the same
            //  as we do not have a good way to generate a set of fresh names without clashing
            if allSame then firstPatternParamNames else UnsupportedFeature.raise {
              s"Constructor parameter names must be the same across all method patterns for '$consName'"
            }
          }
          
          assert(allConstructorParamNames.length == expectedConstructorParamTypes.length)
          val constructorParams: List[(String, Type)] = allConstructorParamNames.zip(expectedConstructorParamTypes)
          val constructorParamVars = constructorParams.map {
            (paramName, paramType) => paramName -> Term.Typed(Term.Var(paramName), paramType)
          }.toMap
          
          // Now we can synthesize the constructor body
          val consBody = env.withValueVars(constructorParamVars) { implicit newEnv =>
            val methods = methodPatterns.map { case RecordFieldMethod(_, methodName, body, _) =>
              // Synthesize each method pattern
              //  e.g., `eval = -e.eval;`
              methodName -> body.synthesize(using expectedMethodTypes.get(methodName))(using newEnv)(using constraints)
            }.toMap
            // The constructor body is `new trait => { methods... }`
            Term.Coercion(
              param = "self",
              paramType = Type.Primitive(PrimitiveType.TopType),
              body = Term.Record(methods.map {
                case (methodName, (methodTerm, methodType)) => methodName -> methodTerm
              })
            ).new_
          }

          // Fold the constructor parameters into a lambda
          val constructorBody = constructorParams.foldRight(consBody) {
            case ((paramName, paramType), accBody: Term) => {
              Term.Lambda(paramName, paramType, accBody)
            }
          }
          
          val constructorType: Type = expectedConsTypeOpt.getOrElse {
            // If we do not have an expected type, we construct one from the parameter types
            val returnType = Type.Record(expectedMethodTypes)
            expectedConstructorParamTypes.foldRight(returnType) {
              (paramType, accReturnType) => Type.Arrow(paramType, accReturnType)
            }
          }

          consName -> (constructorBody, constructorType)
        }
      }.toMap

      val synthesizedFields: Map[String, (Term, Type)] = synthesizedValueFields ++ synthesizedConsFields

      val recordTerm = Term.Record(synthesizedFields.map {
        case (fieldName, (fieldTerm, _)) => (fieldName, fieldTerm)
      })
      val recordType = Type.Record(synthesizedFields.map {
        case (fieldName, (_, fieldType)) => (fieldName, fieldType)
      })
      if !recordTerm.check(recordType) then TypeNotMatch.raise {
        s"Record term does not check against its type: $recordTerm : $recordType"
      } else (recordTerm, recordType)
    }
    
    case ExprTerm.Tuple(elements) => {
      val synthesizedElements: List[(Term, Type)] = elements.map { elemExpr =>
        elemExpr.synthesize
      }
      val tupleTerm = Term.Tuple(synthesizedElements.map(_._1))
      val tupleType = Type.Tuple(synthesizedElements.map(_._2))
      if !tupleTerm.check(tupleType) then TypeNotMatch.raise {
        s"Tuple term does not check against its type: $tupleTerm : $tupleType"
      } else (tupleTerm, tupleType)
    }

    case ExprTerm.Merge(left, right, MergeBias.Left) => ExprTerm.Merge(
      left, ExprTerm.Diff(right, left), MergeBias.Neutral
    ).synthesize

    case ExprTerm.Merge(left, right, MergeBias.Right) => ExprTerm.Merge(
      ExprTerm.Diff(left, right), right, MergeBias.Neutral
    ).synthesize
    
    case ExprTerm.Merge(left, right, MergeBias.Neutral) => {
      val (leftTerm, leftType) = left.synthesize
      val (rightTerm, rightType) = right.synthesize
      (leftType, rightType) match {
        case (Type.Primitive(PrimitiveType.TopType), _) => (rightTerm, rightType)
        case (_, Type.Primitive(PrimitiveType.TopType)) => (leftTerm, leftType)
        case (Type.Trait(leftDomain, leftCodomain), Type.Trait(rightDomain, rightCodomain)) => {
          // Merging two traits
          if !leftType.disjointWith(rightType) then TypeNotMatch.raise {
            s"Cannot merge two overlapping traits: $leftType and $rightType"
          }
          val mergedDomain = (leftDomain, rightDomain) match {
            case (Type.Primitive(PrimitiveType.TopType), _) => rightDomain
            case (_, Type.Primitive(PrimitiveType.TopType)) => leftDomain
            case _ => Type.Intersection(leftDomain, rightDomain).normalize
          }
          Term.Merge(leftTerm, rightTerm) -> Type.Trait(
            domain = mergedDomain,
            codomain = Type.Intersection(leftCodomain, rightCodomain).normalize
          )
        }
        case _ => 
          if !leftType.disjointWith(rightType) then TypeNotMatch.raise {
            s"Cannot merge two overlapping types: $leftType and $rightType"
          }
          val mergedTerm = Term.Merge(leftTerm, rightTerm, MergeBias.Neutral)
          val mergedType = Type.Intersection(leftType, rightType)
          if !mergedTerm.check(mergedType) then TypeNotMatch.raise {
            s"Merged term does not check against its type: $mergedTerm : $mergedType"
          } else (mergedTerm, mergedType)
      }
    }
      
    case ExprTerm.Projection(record, field) => {
      
      val (term: Term, ty: Type) = record.synthesize

      lazy val tryApplication: Option[(Term, Type)] = env.values.get(field) match {
        case Some(fn) => fn.infer.testApplicationReturn(ty) match {
          case Some(returnType) => Some((Term.Apply(fn, term), returnType))
          case None => None
        }
        case _ => None
      }
      
      ty.normalize match {
        case Type.Record(fieldTypes) => 
          fieldTypes.get(field) match {
            case Some(fieldType) => 
              val projTerm = Term.Projection(term, field)
              if !projTerm.check(fieldType) then TypeNotMatch.raise {
                s"Projection term does not check against its type: $projTerm : $fieldType"
              } else (projTerm, fieldType)
            case None => tryApplication match {
              case Some((application, returnType)) => (application, returnType)
              case None => TypeNotMatch.raise(s"Field $field not found in record type: $ty")
            }
          }
        case _ => tryApplication match {
          case Some((application, returnType)) => (application, returnType)
          case None => TypeNotMatch.raise(s"Projection on a non-record type: $ty")
        }
      }
    }
      
    case ExprTerm.TypeApply(term, tyArgs) => {
      val (fnTerm, fnType) = term.synthesize
      val (fnAppTerm, fnAppType) = tyArgs.foldLeft((fnTerm, fnType)) {
        case ((accFnTerm: Term, accFnType: Type), tyArg) => {
          accFnType match {
            case Type.Forall(paramName, body, constraints) => {
              val tyArgType = tyArg.synthesize
              constraints.forall { constraint =>
                if !constraint.verify(tyArgType) then ConstraintNotSatisfied.raise {
                  s"Type argument $tyArgType does not satisfy constraint $constraint"
                } else true
              }
              val substitutedBody = body.subst(paramName, tyArgType)
              (Term.TypeApply(accFnTerm, tyArgType), substitutedBody)
            }
            case _ => TypeNotMatch.raise {
              s"Type application on a non-polymorphic type: $accFnType"
            }
          }
        }
      }
      if !fnAppTerm.check(fnAppType) then TypeNotMatch.raise {
        s"Type application term does not check against its type: $fnAppTerm : $fnAppType"
      } else (fnAppTerm, fnAppType)
    }
    
    // open e in M, where e: { l1: A1, l2: A2, ..., ln: An }
    // is equivalent to let x = e in M [x.l1 / l1, x.l2 / l2, ..., x.ln / ln]
    case ExprTerm.OpenIn(record, body) => {
      val (recordTerm, recordType) = record.synthesize
      // 1. Create a fresh variable to hold the record value,
      //    so that the record expression is only evaluated once.
      val freshVarName = "$open_record"
      val freshVarTerm = Term.Var(freshVarName)
      val freshVarTermExpr = ExprTerm.Var(freshVarName)
      // 2. Create the body term with field projections substituted.
      val (bodyTerm, bodyType) = recordType match {
        case Type.Record(fieldTypes) => {
          val substitutedBody = fieldTypes.foldLeft(body) {
            case (accBody, (fieldName, _)) => {
              accBody.subst(fieldName, ExprTerm.Projection(freshVarTermExpr, fieldName))
            }
          }
          val newEnv: Env = env.addValueVar(freshVarName, Term.Typed(freshVarTerm, recordType))
          substitutedBody.synthesize(using newEnv)
        }
        case _ => TypeNotMatch.raise(s"Opening a non-record type: $recordType")
      }
      // 3. Create the let-in term to bind the fresh variable to the record expression.
      val letTerm = Term.Apply(
        Term.Lambda(freshVarName, recordType, bodyTerm),
        recordTerm
      )
      if !letTerm.check(bodyType) then TypeNotMatch.raise {
        s"Open-in term does not check against its type: $letTerm : $bodyType"
      } else (letTerm, bodyType)
    }
    
    case ExprTerm.Update(_, _) => ???
    
    case ExprTerm.Trait(SelfAnnotation(selfName, selfTypeExprOpt), implementExprOpt, inheritExprOpt, body) => {
      val selfType: Type = selfTypeExprOpt.map(_.synthesize).getOrElse(Type.top)
      val implTypeOpt: Option[Type] = implementExprOpt.map(_.synthesize)
      inheritExprOpt match {
        // There is an inherited trait
        case Some(inheritExpr) => {
          val (inheritTerm: Term, inheritType: Type) = inheritExpr.synthesize
          inheritType match {
            case Type.Trait(inheritDomain, inheritCodomain) => {
              if !(selfType <:< inheritDomain) then TypeNotMatch.raise {
                s"Trait self type $selfType is not a subtype of inherited trait domain $inheritDomain"
              }
              env.withValueVars( // Term.Env with self and super bindings
                selfName -> Term.Typed(Term.Var(selfName), selfType),
                "super" -> Term.Typed(Term.Var("super"), inheritCodomain),
              ) { implicit env =>
                // Synthesize body in the new environment
                val (bodyTerm: Term, bodyType: Type) = body.synthesize(using implTypeOpt)
                // Identify which fields in bodyType override fields in inheritCodomain
                val overrideLabels: Set[String] = body.collectOverrideFields

                extension (ty: Type) def getFieldTypes: Map[String, Type] = ty match {
                  case Type.Record(fieldTypes) => fieldTypes
                  case Type.Intersection(lhs, rhs) =>
                    lhs.getFieldTypes ++ rhs.getFieldTypes
                  case _ => Map.empty
                }
                
                // Check that overridden fields are subtypes of inherited fields
                val overriddenFields: Map[String, Type] = bodyType.getFieldTypes.filter {
                  case (label, _) => overrideLabels.contains(label)
                }
                val fieldsInInherit: Map[String, Type] = inheritCodomain.getFieldTypes.filter {
                  case (label, _) => overrideLabels.contains(label)
                }
                overriddenFields.foreach {
                  case (label, overriddenType) => fieldsInInherit.get(label) match {
                    case Some(inheritedType) => if !(overriddenType <:< inheritedType) then TypeNotMatch.raise {
                      s"Overridden field $label type $overriddenType is not a subtype of inherited field type $inheritedType"
                    }
                    case None => Unreachable.raise {
                      s"Field $label is marked as overridden but not found in inherited trait codomain"
                    }
                  }
                }
                // Prepare codomain for type intersection by removing overridden fields from inheritCodomain
                // So that we don't have duplicate fields in the intersection.
                // TODO: Maybe we can just move this logic to Type.Intersection. So that
                //  we can get rid of the stupid `prepareIntersection`.
                extension (ty: Type) def prepareIntersection: Type = ty match {
                  case Type.Intersection(lhs, rhs) =>
                    val lhsPrepared = lhs.prepareIntersection
                    val rhsPrepared = rhs.prepareIntersection
                    (lhsPrepared, rhsPrepared) match {
                      case (Type.Primitive(TopType), Type.Primitive(TopType)) => Type.top
                      case (Type.Primitive(TopType), _) => rhsPrepared
                      case (_, Type.Primitive(TopType)) => lhsPrepared
                      case _ => Type.Intersection(lhsPrepared, rhsPrepared)
                    }
                  case Type.Record(fieldTypes) =>
                    Type.Record(fieldTypes.filterNot { case (label, _) => overrideLabels.contains(label) })
                  case _ => ty
                }

                // Override codomain with body definitions
                val preparedCodomain = inheritCodomain.prepareIntersection
                if !preparedCodomain.disjointWith(bodyType) then TypeNotMatch.raise {
                  s"Trait body type $bodyType is not disjoint with inherited trait codomain $preparedCodomain"
                }
                
                // Do the intersection to get the final codomain where overridden fields are replaced
                val overriddenCodomain = Type.Intersection(preparedCodomain, bodyType).normalize
                val resultTerm = Term.Apply(
                  func = Term.Lambda(
                    param = "super",
                    paramType = inheritCodomain,
                    body = Term.Merge(
                      Term.Typed(Term.Var("super"), preparedCodomain),
                      bodyTerm,
                      MergeBias.Right,
                    )
                  ),
                  arg = Term.Apply(inheritTerm, Term.Var(selfName)),
                )

                if !resultTerm.check(overriddenCodomain) then TypeNotMatch.raise {
                  s"Result term does not check against overridden codomain: $resultTerm : $overriddenCodomain"
                }

                val traitTerm = Term.Coercion(selfName, selfType, resultTerm)
                val traitType = Type.Trait(selfType, overriddenCodomain)

                if implTypeOpt.exists(implType => !(overriddenCodomain <:< implType)) then TypeNotMatch.raise {
                  s"Trait codomain $overriddenCodomain does not implement declared interface ${implTypeOpt.get}"
                } else if !traitTerm.check(traitType) then TypeNotMatch.raise {
                  s"Trait term does not check against its type: $traitTerm : $traitType"
                } else (traitTerm, traitType)
              }
            }
            // Match inheritType - Inherited expression is not a trait
            case _ => TypeNotMatch.raise {
              s"Inherited expression must be of a trait type, but got: $inheritType"
            }
          }
        }
        // Match inheritExprOpt - There is no inherited trait
        case None => {
          env.withValueVar(selfName, Term.Typed(Term.Var(selfName), selfType)) { implicit env =>
            val (bodyTerm: Term, bodyType: Type) = body.synthesize(using implTypeOpt)
            val traitCodomain = bodyType.normalize
            val traitTerm = Term.Coercion(
              param = selfName,
              paramType = selfType,
              body = bodyTerm,
            )
            val traitType = Type.Trait(selfType, traitCodomain)
            if implTypeOpt.exists(implType => !(bodyType <:< implType)) then TypeNotMatch.raise {
              s"Trait codomain $traitCodomain does not implement declared interface ${implTypeOpt.get}"
            } else if !traitTerm.check(traitType) then TypeNotMatch.raise {
              s"Trait term does not check against its type: $traitTerm : $traitType"
            } else (traitTerm, traitType)
          }
        }
      }
    }
    
    case ExprTerm.New(traitTermExpr) => {
      val (traitTerm: Term, traitType: Type) = traitTermExpr.synthesize
      traitTerm.new_(env, traitType)
    }
    
    case ExprTerm.Forward(_, _) => ???
    case ExprTerm.Exclude(_, _) => ???
    case ExprTerm.Remove(_, _) => ???
    
    case ExprTerm.Diff(left, right) => {
      val (leftTerm, leftType) = left.synthesize
      val (_, rightType) = right.synthesize
      (leftType, rightType) match {
        case (Type.Trait(leftDomain, leftCodomain), Type.Trait(rightDomain, rightCodomain)) => {
          // trait [self: A] { a } \ trait [self: B] { b }  ==  trait [self: A \ B] { a }
          val typeDiff = leftDomain.diff(rightDomain)
          // TODO: find out why we need to apply the leftTerm to self, instead of just leftTerm
          //  https://github.com/yzyzsun/CP-next/blob/1d19de29ff/src/CP/Typing.purs#L475
          val coeBody = Term.Typed(Term.Apply(leftTerm, Term.Var("self")), typeDiff)
          (Term.Coercion("self", leftDomain, coeBody), Type.Trait(leftDomain, typeDiff))
        }
        case _ => {
          val typeDiff = leftType.diff(rightType)
          (Term.Typed(leftTerm, typeDiff), typeDiff)
        }
      }
    }
    
    case ExprTerm.Rename(_, _) => ???
    case ExprTerm.FoldFixpoint(_, _) => ???
    case ExprTerm.UnfoldFixpoint(_, _) => ???
    case ExprTerm.ArrayLiteral(_) => ???
    
    case ExprTerm.Do(expr, body) => {
      val (exprTerm, exprType) = expr.synthesize
      val (bodyTerm, bodyType) = body.synthesize
      val doTerm = Term.Do(exprTerm, bodyTerm)
      if !doTerm.check(bodyType) then TypeNotMatch.raise {
        s"Do term does not check against its type: $doTerm : $bodyType"
      } else (doTerm, bodyType)
    }

    case ExprTerm.Span(term, span) => {
      try term.synthesize catch {
        case e: CoreError => throw e.withSpan(span)
        case e: SpannedError => throw e
        case e: Throwable => throw UnknownError(e, span)
      }
    }
  }
  
  def contains(name: String): Boolean = this match {
    case Var(n) => n == name
    case Typed(expr, _) => expr.contains(name)
    case Apply(fn, args) => fn.contains(name) || args.exists(_.contains(name))
    case Lambda(paramName, _, body) => paramName != name && body.contains(name)
    case TypeLambda(paramName, body) => paramName != name && body.contains(name)
    case Fixpoint(fixName, _, recursiveBody) => fixName != name && recursiveBody.contains(name)
    case IfThenElse(cond, thenBr, elseBr) => cond.contains(name) || thenBr.contains(name) || elseBr.contains(name)
    case LetIn(letName, value, _, body) => value.contains(name) || (letName != name && body.contains(name))
    case Record(fields) => fields.exists { case (_, field) => field.contains(name) }
    case Tuple(elements) => elements.exists(_.contains(name))
    case Merge(left, right, _) => left.contains(name) || right.contains(name)
    case Projection(record, _) => record.contains(name)
    case TypeApply(term, _) => term.contains(name)
    case OpenIn(record, body) => record.contains(name) || body.contains(name)
    case Update(record, updates) => record.contains(name) || updates.values.exists(_.contains(name))
    case Trait(_, _, inheritOpt, body) => inheritOpt.exists(_.contains(name)) || body.contains(name)
    case New(body) => body.contains(name)
    case Forward(left, right) => left.contains(name) || right.contains(name)
    case Exclude(left, _) => left.contains(name)
    case Remove(record, _) => record.contains(name)
    case Diff(left, right) => left.contains(name) || right.contains(name)
    case Rename(record, _) => record.contains(name)
    case FoldFixpoint(_, body) => body.contains(name)
    case UnfoldFixpoint(_, term) => term.contains(name)
    case Do(expr, body) => expr.contains(name) || body.contains(name)
    case ArrayLiteral(elements) => elements.exists(_.contains(name))
    case Span(term, _) => term.contains(name)
    case Primitive(_) => false
  }
  
  def subst(name: String, replacement: ExprTerm): ExprTerm = this match {
    
    case ExprTerm.Primitive(_) => this
    
    case ExprTerm.Var(n) => if n == name then replacement else this
    
    case ExprTerm.Typed(expr, ty) => ExprTerm.Typed(expr.subst(name, replacement), ty)
    
    case ExprTerm.Apply(fn, args) => ExprTerm.Apply(fn.subst(name, replacement), args.map(_.subst(name, replacement)))
    
    case ExprTerm.Lambda(paramName, paramType, body) => {
      if paramName == name then this 
      else ExprTerm.Lambda(paramName, paramType, body.subst(name, replacement))
    }
    
    case ExprTerm.TypeLambda(paramName, body) => {
      if paramName == name then this
      else ExprTerm.TypeLambda(paramName, body.subst(name, replacement))
    }
    
    case ExprTerm.Fixpoint(fixName, ty, recursiveBody) => {
      if fixName == name then this
      else ExprTerm.Fixpoint(fixName, ty, recursiveBody.subst(name, replacement))
    }
    
    case ExprTerm.IfThenElse(cond, thenBr, elseBr) => {
      ExprTerm.IfThenElse(
        cond.subst(name, replacement), 
        thenBr.subst(name, replacement), 
        elseBr.subst(name, replacement)
      )
    }
    
    case ExprTerm.LetIn(letName, value, tyOpt, body) => {
      if letName == name then ExprTerm.LetIn(letName, value.subst(name, replacement), tyOpt, body)
      else ExprTerm.LetIn(letName, value.subst(name, replacement), tyOpt, body.subst(name, replacement))
    }
    
    case ExprTerm.Record(fields) => {
      ExprTerm.Record(fields.map {
        case (fieldName, field) => (fieldName, field.subst(name, replacement))
      })
    }
    
    case ExprTerm.Tuple(elements) => {
      ExprTerm.Tuple(elements.map(_.subst(name, replacement)))
    }
    
    case ExprTerm.Merge(left, right, bias) => {
      ExprTerm.Merge(left.subst(name, replacement), right.subst(name, replacement), bias)
    }
    
    case ExprTerm.Projection(record, field) => {
      ExprTerm.Projection(record.subst(name, replacement), field)
    }
    
    case ExprTerm.TypeApply(term, tyArgs) => {
      ExprTerm.TypeApply(term.subst(name, replacement), tyArgs)
    }
    
    case ExprTerm.OpenIn(record, body) => {
      ExprTerm.OpenIn(record.subst(name, replacement), body.subst(name, replacement))
    }
    
    case ExprTerm.Update(record, updates) => {
      ExprTerm.Update(record.subst(name, replacement), updates.view.mapValues(_.subst(name, replacement)).toMap)
    }
    
    case ExprTerm.Trait(selfAnno, impl, inherit, body) => {
      ExprTerm.Trait(
        selfAnno, impl,
        inherit.map(_.subst(name, replacement)),
        body.subst(name, replacement)
      )
    }
      
    case ExprTerm.New(body) => ExprTerm.New(body.subst(name, replacement))
    
    case ExprTerm.Forward(left, right) => {
      ExprTerm.Forward(left.subst(name, replacement), right.subst(name, replacement))
    }
    
    case ExprTerm.Exclude(left, right) => {
      ExprTerm.Exclude(left.subst(name, replacement), right)
    }
    
    case ExprTerm.Remove(record, fields) => {
      ExprTerm.Remove(record.subst(name, replacement), fields)
    }
    
    case ExprTerm.Diff(left, right) => {
      ExprTerm.Diff(left.subst(name, replacement), right.subst(name, replacement))
    }
    
    case ExprTerm.Rename(record, renames) => {
      ExprTerm.Rename(record.subst(name, replacement), renames)
    }
    
    case ExprTerm.FoldFixpoint(fixTy, body) => {
      ExprTerm.FoldFixpoint(fixTy, body.subst(name, replacement))
    }
    
    case ExprTerm.UnfoldFixpoint(fixTy, term) => {
      ExprTerm.UnfoldFixpoint(fixTy, term.subst(name, replacement))
    }
    
    case ExprTerm.Do(expr, body) => {
      ExprTerm.Do(expr.subst(name, replacement), body.subst(name, replacement))
    }
    
    case ExprTerm.ArrayLiteral(elements) => {
      ExprTerm.ArrayLiteral(elements.map(_.subst(name, replacement)))
    }

    case ExprTerm.Span(term, span) => {
      ExprTerm.Span(term.subst(name, replacement), span)
    }
  }

  private def collectOverrideFields: Set[String] = this match {
    case ExprTerm.Span(term, _) => term.collectOverrideFields
    case ExprTerm.OpenIn(_, body) => body.collectOverrideFields
    case ExprTerm.Merge(lhs, rhs, _) => lhs.collectOverrideFields ++ rhs.collectOverrideFields
    // Fields marked as overriding with 'true' flag
    // TODO: only override the inner field if it's a method pattern
    case ExprTerm.Record(fields) => fields.collect {
      case (fieldName, field) if field.isOverride => fieldName
    }.toSet
    case _ => Set.empty
  }

  // Add parentheses where necessary to ensure correct parsing.
  def toStringAtom: String = this match {
    case _: Typed => s"(${this.toString})"
    case _: Apply => s"(${this.toString})"
    case _: Lambda => s"(${this.toString})"
    case _: TypeLambda => s"(${this.toString})"
    case _: Fixpoint => s"(${this.toString})"
    case _: IfThenElse => s"(${this.toString})"
    case _: LetIn => s"(${this.toString})"
    case _: Merge => s"(${this.toString})"
    case _: TypeApply => s"(${this.toString})"
    case _: OpenIn => s"(${this.toString})"
    case _: Update => s"(${this.toString})"
    case _: Trait => s"(${this.toString})"
    case _: New => s"(${this.toString})"
    case _: Forward => s"(${this.toString})"
    case _: Exclude => s"(${this.toString})"
    case _: Remove => s"(${this.toString})"
    case _: Diff => s"(${this.toString})"
    case _: Rename => s"(${this.toString})"
    case _: FoldFixpoint => s"(${this.toString})"
    case _: UnfoldFixpoint => s"(${this.toString})"
    case _: Do => s"(${this.toString})"
    case _ => this.toString
  }

  // TODO: this pretty print is generated by Copilot, need review
  override def toString: String = this match {
    case ExprTerm.Primitive(value) => value.toString
    case ExprTerm.Var(name) => name
    case ExprTerm.Typed(expr, expectedType) => s"${expr.toStringAtom} : $expectedType"
    case ExprTerm.Apply(fn, args) => s"${fn.toStringAtom} ${args.map(_.toStringAtom).mkString(" ")}"
    case ExprTerm.Lambda(paramName, paramTypeOpt, body) => paramTypeOpt match {
      case Some(paramType) => s"λ$paramName: $paramType. ${body.toString}"
      case None => s"λ$paramName. ${body.toString}"
    }
    case ExprTerm.TypeLambda(paramName, body) => s"Λ$paramName. ${body.toString}"
    case ExprTerm.Fixpoint(name, ty, recursiveBody) => s"fix $name: $ty = λx. ${recursiveBody.toString}"
    case ExprTerm.IfThenElse(condition, thenBranch, elseBranch) => 
      s"if ${condition.toString} then ${thenBranch.toString} else ${elseBranch.toString}"
    case ExprTerm.LetIn(name, value, tyOpt, body) => tyOpt match {
      case Some(ty) => s"let $name: $ty = ${value.toString} in ${body.toString}"
      case None => s"let $name = ${value.toString} in ${body.toString}"
    }
    case ExprTerm.Record(fields) => 
      s"{ ${fields.map { case (k, v) => s"$k = ${v.toString}" }.mkString("; ")} }"
    case ExprTerm.Tuple(elements) => 
      s"( ${elements.map(_.toString).mkString(", ")} )"
    case ExprTerm.Merge(left, right, MergeBias.Neutral) => 
      s"${left.toStringAtom} ,, ${right.toStringAtom}"
    case ExprTerm.Merge(left, right, MergeBias.Left) => 
      s"${left.toStringAtom} ,+ ${right.toStringAtom}"
    case ExprTerm.Merge(left, right, MergeBias.Right) => 
      s"${left.toStringAtom} +, ${right.toStringAtom}"
    case ExprTerm.Projection(record, field) => 
      s"${record.toStringAtom}.$field"
    case ExprTerm.TypeApply(term, tyArgs) => 
      s"${term.toStringAtom}[${tyArgs.map(_.toString).mkString(", ")}]"
    case ExprTerm.OpenIn(record, body) => 
      s"open ${record.toString} in ${body.toString}"
    case ExprTerm.Update(record, updates) => 
      s"${record.toStringAtom} with { ${updates.map { case (k, v) => s"$k = ${v.toString}" }.mkString("; ")} }"
    case ExprTerm.Trait(selfAnno, impl, inherit, body) => {
      // trait [self: SelfType] implements ImplType inherits InheritExpr { body }
      val selfStr = selfAnno match {
        case SelfAnnotation(name, Some(ty)) => s"[$name: $ty]"
        case SelfAnnotation(name, None) => s"[$name]"
      }
      val implStr = impl match {
        case Some(ty) => s" implements $ty"
        case None => ""
      }
      val inheritStr = inherit match {
        case Some(expr) => s" inherits ${expr.toStringAtom}"
        case None => ""
      }
      s"trait $selfStr$implStr$inheritStr { ${body.toString} }"
    }
    case ExprTerm.New(body) => s"new ${body.toStringAtom}"
    case ExprTerm.Forward(left, right) => s"${left.toStringAtom} ^ ${right.toStringAtom}"
    case ExprTerm.Exclude(left, right) => s"${left.toStringAtom} \\\\ ${right.toString}"
    case ExprTerm.Remove(record, fields) => s"${record.toStringAtom} - { ${fields.mkString(", ")} }"
    case ExprTerm.Diff(left, right) => s"${left.toStringAtom} \\ ${right.toStringAtom}"
    case ExprTerm.Rename(record, renames) => 
      s"${record.toStringAtom} rename { ${renames.map { case (k, v) => s"$k -> $v" }.mkString(", ")} }"
    case ExprTerm.FoldFixpoint(fixTy, body) => 
      s"fold $fixTy ${body.toStringAtom}"
    case ExprTerm.UnfoldFixpoint(fixTy, term) => 
      s"unfold $fixTy ${term.toStringAtom}"
    case ExprTerm.Do(expr, body) => 
      s"do ${expr.toStringAtom} in ${body.toString}"
    case ExprTerm.ArrayLiteral(elements) => 
      s"[ ${elements.map(_.toString).mkString(", ")} ]"
    case ExprTerm.Span(term, _) => term.toString
  }
}

object ExprTerm {
  def int(value: Int): ExprTerm = ExprTerm.Primitive(PrimitiveValue.IntValue(value))
  def str(value: String): ExprTerm = ExprTerm.Primitive(PrimitiveValue.StringValue(value))
  def bool(value: Boolean): ExprTerm = ExprTerm.Primitive(PrimitiveValue.BoolValue(value))
  def unit: ExprTerm = ExprTerm.Primitive(PrimitiveValue.UnitValue)
}

