package cp.intermediate.ssa

private trait IdType[Id <: Int] {
  def apply(id: Int): Id = id.asInstanceOf[Id]
  def unapply(id: Id): Option[Int] = Some(id.asInstanceOf[Int])

  extension (id: Id) {
    def value: Int = unapply(id).get
    def next: Id = apply(id.value + 1)
  }
}

opaque type BlockId = Int
object BlockId extends IdType[BlockId]

opaque type VarId = Int
object VarId extends IdType[VarId]

opaque type FuncId = Int
object FuncId extends IdType[FuncId]
