package cp.core

case class Environment(
  typeVars: Map[String, Type] = Map.empty,
  termVars: Map[String, Term] = Map.empty,
) {
  def addTypeVar(name: String, ty: Type): Environment = {
    copy(typeVars = typeVars + (name -> ty))
  }

  def addTermVar(name: String, term: Term): Environment = {
    copy(termVars = termVars + (name -> term))
  }

  def withTypeVar[T](name: String, ty: Type)(f: Environment => T): T = {
    f(addTypeVar(name, ty))
  }

  def withTermVar[T](name: String, term: Term)(f: Environment => T): T = {
    f(addTermVar(name, term))
  }
    
  def freshTypeName: String = {
    Iterator.from(0).map(n => s"$$Type$$$n").find(!typeVars.contains(_)).get
  }

  def freshVarName: String = {
    Iterator.from(0).map(n => s"$$value$$$n").find(!typeVars.contains(_)).get
  }

  def withFreshTypeVar[T](f: (Type.Var, Environment) => T): T = {
    val name = freshTypeName
    val freshVar: Type.Var = Type.Var(name)
    f(freshVar, addTypeVar(name, Type.Var(name)))
  }

  def withFreshTypeBinding[T](ty: Type)(f: (Type.Var, Environment) => T): T = {
    val name = freshTypeName
    f(Type.Var(name), addTypeVar(name, ty))
  }

  def withFreshTermVar[T](ty: Type)(f: (Term.Var, Environment) => T): T = {
    val name = freshVarName
    val freshVar: Term.Var = Term.Var(name)
    f(freshVar, addTermVar(name, Term.Typed(Term.Var(name), ty)))
  }
}

object Environment {
  def empty: Environment = Environment()
}
