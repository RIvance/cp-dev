package cp.core

trait Dependency {
  def namespace: Namespace
  def types: Map[String, Type]
  def symbols: Map[String, Type]

  def toEnv: Environment[String, Type, Term] = {
    val terms = symbols.map { (name, ty) => name -> Term.Symbol(namespace, name, ty) }
    Environment[String, Type, Term](types, terms)
  }
}

trait Module extends Dependency {

  def terms: Map[String, Term]
  def submodules: Map[String, CoreModule] = Map.empty
  def dependencies: Set[Dependency] = Set.empty

  override def symbols: Map[String, Type] = {
    given Environment[String, Type, Term] = this.unfoldEnv
    terms.map { (name, term) => (name, term.infer) }
  }

  def exportSymbols(exported: Set[String]): ExportedModule = {
    val exportedTypes = types.filter { case (name, _) => exported.contains(name) }
    val exportedTerms = terms.filter { case (name, _) => exported.contains(name) }
    ExportedModule(this, exportedTypes.keySet ++ exportedTerms.keySet)
  }

  def header: ModuleHeader = ModuleHeader(namespace, types, symbols)

  def unfoldEnv: Environment[String, Type, Term] = Environment(types, terms)
}

class CoreModule(
  override val namespace: Namespace,
  override val types: Map[String, Type],
  override val terms: Map[String, Term],
  override val submodules: Map[String, CoreModule] = Map.empty,
  override val dependencies: Set[Dependency] = Set.empty
) extends Module {
  override lazy val symbols: Map[String, Type] = super.symbols
}

case class ModuleDependency(
  module: Module,
  symbolAlias: Map[String, String] = Map.empty,
) extends Dependency {

  override def types: Map[String, Type] = module.types

  override def symbols: Map[String, Type] = module.symbols

  override def namespace: Namespace = module.namespace

  override def toEnv: Environment[String, Type, Term] = {
    val types = module.types.map { (name, ty) => symbolAlias.getOrElse(name, name) -> ty }
    val terms = module.symbols.map { (name, ty) => symbolAlias.getOrElse(name, name) -> Term.Symbol(namespace, name, ty) }
    Environment[String, Type, Term](types, terms)
  }
}

case class ModuleHeader(
  override val namespace: Namespace,
  override val types: Map[String, Type],
  override val symbols: Map[String, Type],
) extends Dependency

case class ExportedModule(
  module: Module,
  exportedSymbols: Set[String]
) extends Dependency {

  override def namespace: Namespace = module.namespace

  lazy val types: Map[String, Type] = {
    module.types.filter { case (name, _) => exportedSymbols.contains(name) }
  }

  lazy val terms: Map[String, Term] = {
    module.terms.filter { case (name, _) => exportedSymbols.contains(name) }
  }

  lazy val submodules: Map[String, CoreModule] = {
    module.submodules.filter { case (name, _) => exportedSymbols.contains(name) }
  }

  override lazy val symbols: Map[String, Type] = {
    given Environment[String, Type, Term] = module.toEnv
    terms.map { (name, term) => (name, term.infer) }
  }
}
