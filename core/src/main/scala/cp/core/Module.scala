package cp.core

class Module(
  val types: Map[String, Type],
  val terms: Map[String, Term],
  val submodules: Map[String, Module] = Map.empty,
  val dependencies: Set[Dependency] = Set.empty
) extends ModuleRef {
  def exportSymbols(exported: Set[String]): ExportedModule = {
    val exportedTypes = types.filter { case (name, _) => exported.contains(name) }
    val exportedTerms = terms.filter { case (name, _) => exported.contains(name) }
    ExportedModule(this, exportedTypes.keySet ++ exportedTerms.keySet)
  }
  
  def toEnv = Environment[String, Type, Term](types, terms)

  override lazy val symbols: Map[String, Type] = {
    given Environment[String, Type, Term] = this.toEnv
    terms.map { (name, term) => (name, term.infer) }
  }

  def header: ModuleHeader = ModuleHeader(types, symbols)
}

case class Dependency(
  modulePath: List[String],
  moduleRef: ModuleRef,
  symbolAlias: Map[String, String] = Map.empty
)

trait ModuleRef {
  def types: Map[String, Type]
  def symbols: Map[String, Type]
}

case class ModuleHeader(
  override val types: Map[String, Type],
  override val symbols: Map[String, Type],
) extends ModuleRef

case class ExportedModule(
  module: Module,
  exportedSymbols: Set[String]
) extends ModuleRef {
  
  lazy val types: Map[String, Type] = {
    module.types.filter { case (name, _) => exportedSymbols.contains(name) }
  }

  lazy val terms: Map[String, Term] = {
    module.terms.filter { case (name, _) => exportedSymbols.contains(name) }
  }
  
  lazy val submodules: Map[String, Module] = {
    module.submodules.filter { case (name, _) => exportedSymbols.contains(name) }
  }

  override lazy val symbols: Map[String, Type] = {
    given Environment[String, Type, Term] = module.toEnv
    terms.map { (name, term) => (name, term.infer) }
  }
}
