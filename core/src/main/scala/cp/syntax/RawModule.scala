package cp.syntax

import cp.common.Environment
import cp.core.*
import cp.syntax.Definition.{TermDef, TypeDef}
import cp.util.Graph

case class RawModule(
  types: Map[String, Definition.TypeDef],
  terms: Map[String, Definition.TermDef],
  submodules: Map[String, RawModule],
  dependencies: Set[Namespace] = Set.empty,
) {
  private type Env = Environment[String, Type, Term]
  
  def synthesize(namespace: Namespace, compiledDependencies: Set[Dependency] = Set.empty): CoreModule = {
    
    // We should synthesize types first, so that terms can refer to them.
    val sortedTypes = sortByDependency(types, (defn: TypeDef, name: String) => defn.typ.contains(name)) match {
      case Some(sorted) => sorted
      case None => throw new RuntimeException("Cyclic dependency in type definitions")
    }

    val env = compiledDependencies.foldLeft(Environment.empty[String, Type, Term]) { 
      (envAcc, dependency) => envAcc.merge(dependency.importEnvironment)
    }

    val (synthTypes, typeEnv) = sortedTypes.foldLeft((Map.empty[String, Type], env)) { 
      case ((types, envAcc), (name, typeDef)) => {
        val ty = typeDef.typ.synthesize(using envAcc)(using Set.empty).normalize(using envAcc)
        (types + (name -> ty), envAcc.addTypeVar(name, ty))
      }
    }

    val sortedTerms = sortByDependency(terms, (defn: TermDef, name: String) => defn.term.contains(name)) match {
      case Some(sorted) => sorted
      case None => throw new RuntimeException("Cyclic dependency in term definitions")
    }
    
    val (synthTerms, _) = sortedTerms.foldLeft((Map.empty[String, Term], typeEnv)) { 
      case ((terms, envAcc), (name, exprTerm)) => {
        val (term, ty) = exprTerm.term.synthesize(using envAcc)(using Set.empty)
        val symbol = Term.Symbol(namespace.qualified(name), ty.normalize(using envAcc))
        (terms + (name -> symbol), envAcc.addValueVar(name, symbol))
      }
    }
    
    val mainModule = CoreModule(
      namespace = namespace,
      types = synthTypes,
      terms = synthTerms,
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

//  extension (term: Term) {
//    // Map module-level local symbols to a fully qualified name
//    //  and inline all types from the environment
//    def resolve(namespace: Namespace, terms: Map[String, Term])(using env: Env): Term = term match {
//      case Term.Var(name) => {
//        if env.values.contains(name) then term
//        else terms.get(name) match {
//          case Some(resolvedTerm) => Term.Symbol(namespace.qualified(name), resolvedTerm.infer)
//          case None => throw new RuntimeException(s"Unresolved symbol: $name in namespace ${namespace}")
//        }
//      }
//      case Term.Lambda(param, paramType, body, isCoe) => {
//        // add param to env
//        val newEnv = env.addValueVar(param, Term.Var(param))
//        Term.Lambda(param, paramType.normalize, body.resolve(namespace, terms)(using newEnv), isCoe)
//      }
//      case Term.Fixpoint(name, annotatedType, body) => {
//        // add name to env
//        val newEnv = env.addValueVar(name, Term.Var(name))
//        Term.Fixpoint(name, annotatedType.normalize, body.resolve(namespace, terms)(using newEnv))
//      }
//      case _ => term.normalizeTypes.mapSubterms(_.resolve(namespace))
//    }
//  }
  
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
      types = typeDefs.map(defn => defn.name -> defn).toMap,
      terms = termDefs.map(defn => defn.name -> defn).toMap,
      submodules = submodDefs.map(defn => defn.name -> defn.module).toMap
    )
  }
}

case class RawProgram(defaultModule: RawModule, main: ExprTerm)
