package cp.syntax

import cp.core.*
import cp.util.Graph

case class RawModule(
  terms: Map[String, ExprTerm],
  types: Map[String, ExprType],
  submodules: Map[String, RawModule],
  dependencies: Set[String] = Set.empty,
) {
  private type Env = Environment[String, Type, Term]
  
  def synthesize(namespace: Namespace, compiledDependencies: Set[Dependency] = Set.empty): CoreModule = {
    
    // We should synthesize types first, so that terms can refer to them.
    val sortedTypes = sortByDependency(types, (ty: ExprType, name: String) => ty.contains(name)) match {
      case Some(sorted) => sorted
      case None => throw new RuntimeException("Cyclic dependency in type definitions")
    }

    val env = compiledDependencies.foldLeft(Environment.empty[String, Type, Term]) { 
      (envAcc, dependency) => envAcc.merge(dependency.importEnvironment)
    }

    val synthesizedTypes = sortedTypes.foldLeft(env) { case (envAcc, (name, exprType)) =>
      val ty = exprType.synthesize(using envAcc)(using Set.empty).normalize(using envAcc)
      envAcc.addTypeVar(name, ty)
    }

    val sortedTerms = sortByDependency(terms, (term: ExprTerm, name: String) => term.contains(name)) match {
      case Some(sorted) => sorted
      case None => throw new RuntimeException("Cyclic dependency in term definitions")
    }
    
    val synthesizedTerms = sortedTerms.foldLeft(synthesizedTypes) { case (envAcc, (name, exprTerm)) =>
      val (term, _) = exprTerm.synthesize(using envAcc)(using Set.empty)
      envAcc.addValueVar(name, term)
    }
    
    val mainModule = CoreModule(
      namespace = namespace,
      terms = synthesizedTerms.values.filter { case (name, _) => terms.contains(name) },
      types = synthesizedTypes.types.filter { case (name, _) => types.contains(name) },
      dependencies = compiledDependencies,
    )
    
    CoreModule(
      namespace = mainModule.namespace,
      terms = mainModule.terms,
      types = mainModule.types,
      submodules = submodules.map { (name, rawSubmod) =>
        name -> rawSubmod.synthesize(
          namespace.join(name),
          compiledDependencies + ModuleDependency(mainModule)
        )
      },
      dependencies = compiledDependencies,
    )
  }
  
  /**
   * Sort elements by their dependencies.
   *
   * @param elements    Map of named elements to sort
   * @param containsRef Function to check if an element references a name
   * @return Option containing sorted elements, or None if there's a cyclic dependency
   */
  private def sortByDependency[T](
    elements: Map[String, T], containsRef: (T, String) => Boolean
  ): Option[Seq[(String, T)]] = {
    val initGraph = Graph.directed[String].addVertices(elements.keySet)
    elements.foldLeft(initGraph) { case (graph0, (name, elem)) =>
      elements.keySet.foldLeft(graph0) { (graph1, otherName) =>
        if containsRef(elem, otherName) then graph1.addEdge(name, otherName)
        else graph1
      }
    }.topologicalSort.map { sorted =>
      sorted.collect { case name if elements.contains(name) => name -> elements(name) }
    }
  }
}

object RawModule {
  
  def empty: RawModule = RawModule(Seq.empty)

  def apply(definitions: Seq[Definition]): RawModule = {
    val (termDefs, typeDefs, submodDefs) = definitions.reverse.foldLeft((
      List.empty[Definition.TermDef], 
      List.empty[Definition.TypeDef], 
      List.empty[Definition.SubmodDef],
    )) {
      case ((terms, types, submods), definition) => definition match {
        case term: Definition.TermDef => (term :: terms, types, submods)
        case typeDef: Definition.TypeDef => (terms, typeDef :: types, submods)
        case submod: Definition.SubmodDef => (terms, types, submod :: submods)
        case Definition.Spanned(_, defn) => defn match {
          case term: Definition.TermDef => (term :: terms, types, submods)
          case typeDef: Definition.TypeDef => (terms, typeDef :: types, submods)
          case submod: Definition.SubmodDef => (terms, types, submod :: submods)
          case Definition.Spanned(_, _) => throw new RuntimeException("Nested spans are not allowed")
        }
      }
    }

    RawModule(
      terms = termDefs.map(defn => defn.name -> defn.term).toMap,
      types = typeDefs.map(defn => defn.name -> defn.typeDef).toMap,
      submodules = submodDefs.map(defn => defn.name -> defn.module).toMap
    )
  }
}

case class RawProgram(defaultModule: RawModule, main: ExprTerm)
