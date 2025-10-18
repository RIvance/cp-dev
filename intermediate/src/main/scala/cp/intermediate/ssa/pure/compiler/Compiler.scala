package cp.intermediate.ssa.pure.compiler

import cp.core.Environment
import cp.intermediate.{BuiltInFunction, NativePrototype, NativePrototypePure, TypeValue as Type}
import cp.intermediate.calculus.RcTerm as Term
import cp.intermediate.ssa.pure.{Block, Function, Instruction, Program, SSAConstruct, Value}
import cp.intermediate.ssa.{BlockId, FuncId, VarId}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

type CompileEnv = Environment[Int, Type, Value]

trait SSABuilder[C <: SSAConstruct] {
  def build(): C
}

private class BlockBuilder(
  val id: BlockId, val function: FunctionBuilder
) extends SSABuilder[Block] {
  private var instructions = List.empty[Instruction]
  private[compiler] var predecessors = Set.empty[BlockId]

  def +=(instruction: Instruction): Unit = {
    instructions = instructions :+ instruction
  }

  def freshVar(): VarId = function.freshVar()

  override def build(): Block = Block(id, instructions, predecessors)
}

private class FunctionBuilder(
  val name: String,
  val parameters: List[Type],
  val returnType: Type,
  val program: ProgramBuilder,
) extends SSABuilder[Function] {

  private[compiler] var blocks: Map[BlockId, BlockBuilder] = Map.empty
  val entryBlock: BlockBuilder = this.createBlock()

  private var nextVarId: VarId = VarId(parameters.length)
  private var nextBlockId: BlockId = BlockId(0)

  def freshVar(): VarId = nextVarId.tap(_ => nextVarId = nextVarId.next)

  private def freshBlock(): BlockId = nextBlockId.tap(_ => nextBlockId = nextBlockId.next)

  def createBlock(predecessors: BlockId*): BlockBuilder = {
    BlockBuilder(freshBlock(), this).tap { builder =>
      predecessors.foreach(pred => builder.predecessors += pred)
      blocks = blocks + (builder.id -> builder)
    }
  }

  def getBlock(id: BlockId): Option[BlockBuilder] = blocks.get(id)

  override def build(): Function = Function(
    name,
    parameters,
    blocks.view.mapValues(_.build()).toMap,
    entryBlock.id,
    returnType
  )

  def compileTerm(term: Term, block: BlockBuilder)(using env: CompileEnv): (Value, BlockBuilder) = term match {

    case Term.Var(index) => (env.value(index), block)

    case Term.Primitive(lit) => (Value.Const(lit), block)

    case Term.If(condition, thenBranch, elseBranch) => {
      val (condValue, condFinalBlock) = this.compileTerm(term, block)

      val thenInitialBlock = this.createBlock(condFinalBlock.id)
      val (thenValue, thenFinalBlock) = this.compileTerm(thenBranch, thenInitialBlock)

      val elseInitialBlock = this.createBlock(condFinalBlock.id)
      val (elseValue, elseFinalBlock) = this.compileTerm(elseBranch, elseInitialBlock)

      condFinalBlock += Instruction.Branch(condValue, thenInitialBlock.id, elseInitialBlock.id)

      val mergeBlock = this.createBlock(thenFinalBlock.id, elseFinalBlock.id)

      thenFinalBlock += Instruction.Jump(mergeBlock.id)
      elseFinalBlock += Instruction.Jump(mergeBlock.id)

      val resultVar = mergeBlock.freshVar()
      mergeBlock += Instruction.Phi(List(
        (thenValue, thenFinalBlock.id),
        (elseValue, elseFinalBlock.id)
      ), resultVar)

      (Value.Var(resultVar), mergeBlock)
    }

    case lambda @ Term.Lambda(_, _) => (this.program.compileLambda(lambda), block)

    case fixpoint @ Term.Fixpoint(_, _) => (this.program.compileFixpoint(fixpoint), block)

    case Term.Apply(func, arg) => {
      val (funcValue, funcFinalBlock) = this.compileTerm(func, block)
      val (argValue, argFinalBlock) = this.compileTerm(arg, funcFinalBlock)

      val resultVar = argFinalBlock.freshVar()
      argFinalBlock += Instruction.Call(funcValue, List(argValue), resultVar)

      (Value.Var(resultVar), argFinalBlock)
    }

    case Term.Merge(_, _) => ???

    case Term.FunctionCall(fn: NativePrototype[Type], args) => {
      val funcIdent = if fn.isPure
        then s"@pure#${fn.name}"
        else s"@impure#${fn.name}"
      val (argValues, finalBlock) = args.foldLeft((List.empty[Value], block)) {
        case ((accValues, currBlock), argTerm) => {
          val (argValue, argFinalBlock) = this.compileTerm(argTerm, currBlock)
          (accValues :+ argValue, argFinalBlock)
        }
      }
      val resultVar = finalBlock.freshVar()
      finalBlock += Instruction.Call(Value.Closure(fn.name), argValues, resultVar)
      (Value.Var(resultVar), finalBlock)
    }

    case Term.FunctionCall(BuiltInFunction(name, argTypes, returnType, fn), args) => ???

    case Term.Record(fields) => {
      val (fieldValues, finalBlock) = fields.foldLeft((Map.empty[String, Value], block)) {
        case ((accValues, currBlock), (fieldName, fieldTerm)) => {
          val (fieldValue, fieldFinalBlock) = this.compileTerm(fieldTerm, currBlock)
          (accValues + (fieldName -> fieldValue), fieldFinalBlock)
        }
      }
      val resultVar = finalBlock.freshVar()
      finalBlock += Instruction.RecordCreate(fieldValues, resultVar)
      (Value.Var(resultVar), finalBlock)
    }

    case Term.Project(record, field) => {
      val (recordValue, recordFinalBlock) = this.compileTerm(record, block)
      val resultVar = recordFinalBlock.freshVar()
      recordFinalBlock += Instruction.RecordProject(recordValue, field, resultVar)
      (Value.Var(resultVar), recordFinalBlock)
    }
  }
}

private class ProgramBuilder extends SSABuilder[Program] {
  private var functions: Map[String, FunctionBuilder] = Map.empty
  private var freshFuncId: FuncId = FuncId(0)

  override def build(): Program = Program(functions.view.mapValues(_.build()).toMap, "main")

  def compile(term: Term)(using env: CompileEnv): Unit = {
    val mainFunc = FunctionBuilder("main", Nil, term.infer, this)
    mainFunc.compileTerm(term, mainFunc.entryBlock)
  }

  def compileLambda(lambda: Term.Lambda)(using env: CompileEnv): Value = {
    val funcName = s"lambda_${freshFuncId}"
    val (paramTypes, body) = lambda.uncurry
    val returnType = paramTypes.foldLeft(body.infer) {
      case (Type.Function(_, to), _) => to
      case _ => throw new RuntimeException("Lambda type must be a function type")
    }
    val captures: List[(Value, Type)] = (0 until body.maxVarIndex).map(env.apply).toList

    this.compileFunction(funcName, paramTypes, returnType, body, captures, None)
  }

  def compileFixpoint(fixpoint: Term.Fixpoint)(using env: CompileEnv): Value = {
    val funcName = s"fixpoint_${freshFuncId}"
    // Here, `body` is actually the inner-most body of the lambda chain
    // e.g., for `fix f. λx. λy. ...`, `body` is `...`
    val (paramTypes, body) = fixpoint.body match {
      case lambda: Term.Lambda => lambda.uncurry
      case other => (List.empty[Type], other)
    }
    val captures: List[(Value, Type)] = (0 until (body.maxVarIndex - paramTypes.length)).map(env.apply).toList
    val bodyEnv = (List(fixpoint.fixpointType) ++ paramTypes ++ captures.map(_._2)).zipWithIndex.map(_.swap).toMap
    val returnType = body.infer(using bodyEnv)

    this.compileFunction(funcName, paramTypes, returnType, body, captures, Some((funcName, fixpoint.fixpointType)))
  }

  def compileFunction(
    name: String,
    paramTypes: List[Type],
    returnType: Type,
    body: Term,
    captures: List[(Value, Type)],
    fixpointBinding: Option[(String, Type)],
  ): Value = {
    val numParams = paramTypes.length
    val maxVarIndex = body.maxVarIndex

    val targetParamTypes = captures.map(_._2) ++ paramTypes

    val functionBuilder = this.createFunction(name, targetParamTypes, returnType)

    given CompileEnv = fixpointBinding match {
      case Some((fixName, fixType)) =>
        // If it's a recursive function, we need to add the fixpoint itself to the environment
        val fixClosure = Value.Closure(fixName, captures.map(_._1))
        buildFuncEnv(body, paramTypes, captures.map(_._2), Some((fixClosure, fixType)))
      case None =>
        // Non-recursive function
        buildFuncEnv(body, paramTypes, captures.map(_._2))
    }

    val (bodyValue, finalBlock) = functionBuilder.compileTerm(body, functionBuilder.entryBlock)
    finalBlock += Instruction.Return(bodyValue)
    Value.Closure(name, captures.map(_._1))
  }

  private def buildFuncEnv(
    body: Term, paramTypes: List[Type], captureTypes: List[Type],
    fixpointBinding: Option[(Value, Type)] = None
  ): CompileEnv = {
    val numParams = paramTypes.length
    val (values, types) = (0 to body.maxVarIndex).map { debruijnIndex =>
      // Create environment for lambda body
      // De Bruijn index 0 corresponds to the innermost parameter (last in params list)
      // De Bruijn index i (where i < numParams) corresponds to params(numParams - 1 - i)
      // For fixpoint, De Bruijn index numParams corresponds to the recursive function itself
      // Captured variables start from index numParams (or numParams + 1 if fixpoint)
      val (value, ty) = if debruijnIndex < numParams then {
        // Is a parameter
        val indexInParams = numParams - 1 - debruijnIndex  // index in `paramTypes` list
        val varId = VarId(captureTypes.length + indexInParams)
        (Value.Var(varId), paramTypes(indexInParams))
      } else if fixpointBinding.isDefined && debruijnIndex == numParams then {
        // Is the fixpoint itself
        fixpointBinding.get
      } else {
        // Is a captured variable
        val captureIndex = debruijnIndex - numParams - (if fixpointBinding.isDefined then 1 else 0)
        (Value.Var(VarId(captureIndex)), captureTypes(captureIndex))
      }
      (debruijnIndex -> value, debruijnIndex -> ty)
    }.unzip
    Environment(types.toMap, values.toMap)
  }

  def createFunction(
    name: String,
    parameters: List[Type],
    returnType: Type
  ): FunctionBuilder = FunctionBuilder(name, parameters, returnType, this).tap { builder =>
    functions = functions + (name -> builder)
    freshFuncId = freshFuncId.next
  }
}

extension (lambda: Term.Lambda) {
  private def uncurry: (List[Type], Term) = {
    @tailrec
    def loop(lambda: Term.Lambda, acc: List[Type]): (List[Type], Term) = lambda.body match {
      case innerLambda: Term.Lambda => loop(innerLambda, acc :+ lambda.paramType)
      case body => (acc :+ lambda.paramType, body)
    }
    loop(lambda, Nil)
  }
}

extension (term: Term) {
  def maxVarIndex: Int = term match {
    case Term.Var(index) => index
    case Term.Primitive(_) => 0
    case Term.Lambda(_, body) => Math.max(0, body.maxVarIndex - 1)
    case Term.Apply(func, arg) => Math.max(func.maxVarIndex, arg.maxVarIndex)
    case Term.If(cond, thenBr, elseBr) => List(cond, thenBr, elseBr).map(_.maxVarIndex).max
    case Term.Fixpoint(_, body) => body.maxVarIndex
    case Term.Record(fields) => if fields.isEmpty then 0 else fields.values.map(_.maxVarIndex).max
    case Term.Project(record, _) => record.maxVarIndex
    case Term.Merge(left, right) => Math.max(left.maxVarIndex, right.maxVarIndex)
    case Term.FunctionCall(_, args) => if args.isEmpty then 0 else args.map(_.maxVarIndex).max
    case Term.Thunk(_, body, _) => body.maxVarIndex
  }
}
