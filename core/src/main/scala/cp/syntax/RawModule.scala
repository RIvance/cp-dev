package cp.syntax

import cp.core.{Environment, Module}

case class RawModule(
  terms: Map[String, ExprTerm],
  types: Map[String, ExprType],
  submodules: Map[String, RawModule],
) {
  def synthesize(using env: Environment = Environment.empty): Module = {
    
    // We should synthesize types first, so that terms can refer to them.
    val synthesizedTypes = types.foldLeft(env) { case (envAcc, (name, exprType)) =>
      val ty = exprType.synthesize(using envAcc)(using Set.empty).normalize
      envAcc.addTypeVar(name, ty)
    }
    
    val synthesizedTerms = terms.foldLeft(synthesizedTypes) { case (envAcc, (name, exprTerm)) =>
      val (term, _) = exprTerm.synthesize(using envAcc)(using Set.empty)
      envAcc.addTermVar(name, term)
    }
    
    Module(
      terms = synthesizedTerms.termVars,
      types = synthesizedTypes.typeVars,
      submodules = submodules.map { case (name, rawMod) => 
        name -> rawMod.synthesize(using synthesizedTerms)
      }
    )
  }
}

object RawModule {
  
  def empty: RawModule = RawModule(Map.empty, Map.empty, Map.empty)
  
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
