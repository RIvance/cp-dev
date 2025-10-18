package cp.core

class Module(
  val types: Map[String, Type],
  val terms: Map[String, Term],
  val submodules: Map[String, Module] = Map.empty,
  val dependencies: Set[Dependency] = Set.empty
) {
  def exportSymbols(exported: Set[String]): ExportedModule = {
    val exportedTypes = types.filter { case (name, _) => exported.contains(name) }
    val exportedTerms = terms.filter { case (name, _) => exported.contains(name) }
    ExportedModule(this, exportedTypes.keySet ++ exportedTerms.keySet)
  }
  
  def toEnv = Environment[String, Type, Term](types, terms)
}

case class Dependency(
  modulePath: List[String],
  exportedModule: ExportedModule,
  symbolAlias: Map[String, String] = Map.empty
)

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
  
  lazy val submodules: Map[String, Module] = {
    module.submodules.filter { case (name, _) => exportedSymbols.contains(name) }
  }
}
