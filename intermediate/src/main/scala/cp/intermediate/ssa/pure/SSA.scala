package cp.intermediate.ssa.pure

import cp.intermediate.ssa.{BlockId, FuncId, VarId}
import BlockId.*
import VarId.*
import FuncId.*
import cp.core.Literal
import cp.intermediate.calculus.RcTerm as Term
import cp.intermediate.TypeValue as Type
import cp.util.LateInit

import scala.util.chaining.scalaUtilChainingOps

trait SSAConstruct

enum Value extends SSAConstruct {
  case Var(id: VarId)
  case Const(value: Literal)
  case Closure(func: String, env: List[Value] = Nil)
}

enum Instruction extends SSAConstruct {
  case Branch(condition: Value, thenBlock: BlockId, elseBlock: BlockId)
  case Jump(target: BlockId)
  case Return(value: Value)
  case Call(func: Value, args: List[Value], result: VarId)
  case Phi(options: List[(Value, BlockId)], result: VarId)
  case RecordCreate(fields: Map[String, Value], result: VarId)
  case RecordProject(record: Value, field: String, result: VarId)
}

case class Block(
  id: BlockId,
  instructions: List[Instruction],
  predecessors: Set[BlockId] = Set.empty
) extends SSAConstruct {
  override def toString: String = {
    val header = s"block_$id:"
    val predStr = if (predecessors.nonEmpty) {
      s" // predecessors: ${predecessors.map(p => s"block_$p").mkString(", ")}\n"
    } else ""
    s"$header\n$predStr${instructions.map(i => s"  $i").mkString("\n")}"
  }
}

case class Function(
  name: String,
  parameters: List[Type],
  blocks: Map[BlockId, Block],
  entryBlock: BlockId,
  returnType: Type
) extends SSAConstruct {
  override def toString: String = {
    val paramsStr = parameters.zipWithIndex.map { (ty, id) => s"%$id: $ty" }.mkString(", ")
    val blocksStr = blocks.values.map(_.toString).mkString("\n\n")
    s"fn $name($paramsStr) -> $returnType {\n$blocksStr\n}"
  }
}

case class Program(
  functions: Map[String, Function], entryPoint: String
) extends SSAConstruct {
  override def toString: String = {
    val funcsStr = functions.values.map(_.toString).mkString("\n\n")
    s"// entry: $entryPoint\n$funcsStr"
  }
}
  
