package cp.core

case class Environment(
  typeVars: Map[String, Type] = Map.empty,
  termVars: Map[String, Term] = Map.empty,
) {
  def addTypeVar(name: String, ty: Type): Environment =
    copy(typeVars = typeVars + (name -> ty))

  def addTermVar(name: String, term: Term): Environment =
    copy(termVars = termVars + (name -> term))

  def withTypeVar[T](name: String, ty: Type)(f: Environment => T): T =
    f(addTypeVar(name, ty))

  def withTermVar[T](name: String, term: Term)(f: Environment => T): T =
    f(addTermVar(name, term))
    
  def freshTypeName: String = 
    Iterator.from(0).map(n => s"_T$n").find(!typeVars.contains(_)).get

  def freshVarName: String =
    Iterator.from(0).map(n => s"_v$n").find(!typeVars.contains(_)).get
}
