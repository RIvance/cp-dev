package cp.core

class Module(
  val types: Map[String, Type],
  val terms: Map[String, Term],
  val dependencies: Set[ExportedModule] = Set.empty
) {
  def exportSymbols(exported: Set[String]): ExportedModule = {
    val exportedTypes = types.filter { case (name, _) => exported.contains(name) }
    val exportedTerms = terms.filter { case (name, _) => exported.contains(name) }
    ExportedModule(this, exportedTypes.keySet ++ exportedTerms.keySet)
  }
}

case class ExportedModule(
  module: Module,
  exportedSymbols: Set[String]
) {
  
  lazy val types: Map[String, Type] = {
    module.types.filter { case (name, _) => exportedSymbols.contains(name) }
  }

  lazy val terms: Map[String, Term] = {
    module.terms.filter { case (name, _) => exportedSymbols.contains(name) }
  }
}
