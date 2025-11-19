package cp.core

import cp.util.IdentifiedByString

enum NeutralValue extends IdentifiedByString {
  case Var(name: String)
  case Apply(func: NeutralValue, arg: Value)
  case Project(record: NeutralValue, field: String)
  case Merge(left: Value, right: Value)
  case Annotated(value: NeutralValue, annotatedType: Type)
  case NativeCall(function: NativeCallable, args: List[Value])
  // Represents a fixpoint thunk that may cause infinite unfolding
  case UnfoldingThunk(env: ValueEnv, annotatedType: Type, name: String, body: Term)

  override def contains(name: String): Boolean = this match {
    case NeutralValue.Var(n) => n == name
    case NeutralValue.Apply(func, arg) => func.contains(name) || arg.contains(name)
    case NeutralValue.Project(record, field) => record.contains(name)
    case NeutralValue.Merge(left, right) => left.contains(name) || right.contains(name)
    case NeutralValue.Annotated(value, annotatedType) => value.contains(name)
    case NeutralValue.NativeCall(_, args) => args.exists(_.contains(name))
    case NeutralValue.UnfoldingThunk(env, _, n, body) => {
      n != name && env.values.values.exists(_.contains(name)) || body.contains(name)
    }
  }

  // Convert neutral value back to term representation
  def toTerm: Term = this match {
    case NeutralValue.Var(varName) => Term.Var(varName)
    case NeutralValue.Apply(func, arg) => Term.Apply(func.toTerm, arg.toTerm)
    case NeutralValue.Project(record, field) => Term.Projection(record.toTerm, field)
    case NeutralValue.Merge(left, right) => Term.Merge(left.toTerm, right.toTerm, MergeBias.Neutral)
    case NeutralValue.Annotated(value, annotatedType) => Term.Annotated(value.toTerm, annotatedType)
    case NeutralValue.NativeCall(function, args) => Term.NativeFunctionCall(function.asInstanceOf[NativeFunction], args.map(_.toTerm))
    case NeutralValue.UnfoldingThunk(_, ty, name, body) => Term.Fixpoint(name, ty, body)
  }

  def infer(using env: ValueEnv): Type = this match {
    case NeutralValue.Var(name) => env.getValue(name) match {
      case Some(value) => value.infer
      case None => throw new RuntimeException(s"Unbound variable in neutral value: $name")
    }
    case NeutralValue.Apply(func, _) => func.infer match {
      case Type.Arrow(_, returnType, _) => returnType
      case other => throw new RuntimeException(s"Attempted to apply a non-function type: $other")
    }
    case NeutralValue.Project(record, field) => record.infer match {
      case Type.Record(fields) => fields.get(field) match {
        case Some(fieldType) => fieldType
        case None => throw new RuntimeException(s"Field '$field' not found in record type: $this")
      }
      case other => throw new RuntimeException(s"Attempted to project a field from a non-record type: $other")
    }
    case NeutralValue.Merge(left, right) => Type.Intersection(left.infer, right.infer)
    case NeutralValue.Annotated(_, annotatedType) => annotatedType
    case NeutralValue.NativeCall(function, args) => {
      val remainingParams = function.paramTypes.drop(args.length)
      if remainingParams.isEmpty then {
        // All parameters applied, return the return type
        function.returnType
      } else {
        // Partially applied, build arrow type for remaining parameters
        remainingParams.foldRight(function.returnType) { (paramType, accType) =>
          Type.Arrow(paramType, accType, function.isPure)
        }
      }
    }
    case NeutralValue.UnfoldingThunk(_, annotatedType, _, _) => annotatedType
  }

  def toAtomicString: String = this match {
    case NeutralValue.Var(name) => name
    case _ => s"(${this.toString})"
  }

  override def toString: String = this match {
    case NeutralValue.Var(name) => name
    case NeutralValue.Apply(func, arg) => s"(${func.toAtomicString} ${arg.toString})"
    case NeutralValue.Project(record, field) => s"${record.toAtomicString}.$field"
    case NeutralValue.Merge(left, right) => s"(${left.toString} ,, ${right.toString})"
    case NeutralValue.Annotated(value, annotatedType) => s"(${value.toString} : $annotatedType)"
    case NeutralValue.NativeCall(function, args) => function.toStringWithArgs(args)
    case NeutralValue.UnfoldingThunk(_, ty, name, body) => s"fix $name : $ty = ${body.toString}"
  }
}
