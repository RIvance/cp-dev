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
  
  def withTermVars[T](vars: Map[String, Term])(f: Environment => T): T = {
    f(copy(termVars = termVars ++ vars))
  }

  def withTermVars[T](vars: (String, Term)*)(f: Environment => T): T = {
    f(copy(termVars = termVars ++ vars.toMap))
  }

  def freshTypeName(bodies: Type*): String = {
    Iterator.from(0).map(n => s"$$Type$$$n").find {
      name => !typeVars.contains(name) && !bodies.exists(_.contains(name))
    }.get
  }

  def freshVarName(bodies: Term*): String = {
    Iterator.from(0).map(n => s"$$value$$$n").find {
      name => !termVars.contains(name) && !bodies.exists(_.contains(name))
    }.get
  }

  def withFreshTypeVar[T](bodies: Type*)(f: (Type.Var, Environment) => T): T = {
    val name = freshTypeName(bodies*)
    val freshVar: Type.Var = Type.Var(name)
    f(freshVar, addTypeVar(name, Type.Var(name)))
  }

  def withFreshTypeBinding[T](ty: Type)(bodies: Type*)(f: (Type.Var, Environment) => T): T = {
    val name = freshTypeName(bodies*)
    f(Type.Var(name), addTypeVar(name, ty))
  }

  def withFreshTermVar[T](ty: Type)(bodies: Term*)(f: (Term.Var, Environment) => T): T = {
    val name = freshVarName(bodies*)
    val freshVar: Term.Var = Term.Var(name)
    f(freshVar, addTermVar(name, Term.Typed(Term.Var(name), ty)))
  }
}

object Environment {

  def empty: Environment = Environment()

  def builder: Builder = Builder()

  class Builder {
    private var env: Environment = Environment.empty

    def typeVar(name: String, ty: Type): Builder = {
      env = env.addTypeVar(name, ty)
      this
    }

    def termVar(name: String, term: Term): Builder = {
      env = env.addTermVar(name, term)
      this
    }

    def build(): Environment = env

    def buildWith(f: Builder => Unit): Environment = {
      f(this)
      this.build()
    }
  }
}
