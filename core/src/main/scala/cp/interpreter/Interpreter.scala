package cp.interpreter

import cp.common.Environment
import cp.core.{Module, Namespace, Term, Type}

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class Interpreter(initialModules: Set[Module]) {

  private type Env = Environment[String, Type, Value]

  private var modules: Map[Namespace, Module] = initialModules.map { mod => mod.namespace -> mod }.toMap

  def eval(term: Term)(using env: Env = Environment.empty[String, Type, Value]): Value = term.evalTramp.result

  extension (term: Term) {

    private def evalTramp(using env: Env): TailRec[Value] = term match {

      case Term.Var(name) => env.getValue(name) match {
        case Some(fix @ Value.FixThunk(annotatedType, name, body, captured)) =>
          // When a FixThunk is looked up, evaluate its body in the captured environment
          //  and since we cannot have self-reference when building the captured env,
          //  we now put the thunk itself for self-reference
          tailcall(body.evalTramp(using captured.addValueVar(name, fix)))
        case Some(value) => tailcall(done(value))
        case None => throw new RuntimeException(s"Variable $name not found in environment")
      }

      case Term.Primitive(value) => tailcall(done(Value.Primitive(value)))

      case Term.Lambda(param, paramType, body, isCoe) => {
        tailcall(done(Value.Closure(env, param, paramType, body, isCoe)))
      }

      case Term.Apply(fnTerm, argTerm) => for {
        fnValue <- fnTerm.evalTramp
        argValue <- argTerm.evalTramp
        result <- fnValue match {
          case Value.Closure(closureEnv, param, paramType, body, _) =>
            val newEnv = closureEnv.addValueVar(param, argValue)
            tailcall(body.evalTramp(using newEnv))
          case other => throw new RuntimeException(s"Runtime type error: expected closure, got $other")
        }
      } yield result

      case Term.Fixpoint(name, annotatedType, body) => {
        // Create a thunk that will be used for self-reference
        lazy val newEnv = env.addValueVar(name, fixThunk)
        // Fixpoint itself will be added to the environment when evaluating the thunk
        lazy val fixThunk: Value = Value.FixThunk(annotatedType, name, body, env)
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

      case Term.Projection(recordTerm, field) => for {
        recordValue <- recordTerm.evalTramp
        result <- recordValue match {
          case Value.Record(fields) => fields.get(field) match {
            case Some(value) => tailcall(done(value))
            case None => throw new RuntimeException(s"Field $field not found in record")
          }
          case _ => throw new RuntimeException("Cannot project from non-record value")
        }
      } yield result

      case Term.Symbol(name, ty) => modules.get(name.namespace) match {
        case Some(module) => module.terms.get(name.localName) match {
          case Some(term) => tailcall(term.evalTramp(using env))
          case None => throw new RuntimeException(s"Term ${name.localName} not found in module ${name.namespace}")
        }
        case None => throw new RuntimeException(s"Module ${name.namespace} not found")
      }

      // TODO: We need to consider whether we should use type annotation for record fields filtering or not.
      //  and when we should preserve type annotations (e.g., fixpoint).
      case Term.Typed(_, _) => ???

      case Term.TypeApply(_, _) => ???

      case Term.TypeLambda(_, _) => ???

      case Term.Tuple(_) => ???

      case Term.Merge(_, _, _) => ???

      case Term.Diff(_, _) => ???

      case Term.IfThenElse(_, _, _) => ???

      case Term.ArrayLiteral(_) => ???

      case Term.FoldFixpoint(_, _) => ???

      case Term.UnfoldFixpoint(_, _) => ???

      case Term.Do(_, _) => ???

      case Term.RefAddr(_, _) => ???

      case Term.NativeFunctionCall(_, _) => ???

      case Term.NativeProcedureCall(_, _) => ???

    }
  }
}
