package cp.core

class Program(
  val types: Map[String, Type],
  val terms: Map[String, Term],
  val main: Term,
  val dependencies: Set[ExportedModule] = Set.empty,
)
