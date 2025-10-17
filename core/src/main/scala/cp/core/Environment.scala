package cp.core

import cp.util.RecNamed

trait TypeEnvironment[T <: RecNamed] { self =>
  
  type Self <: TypeEnvironment[T]
  
  def types: Map[String, T]
  
  def addTypeVar[T1 <: T](name: String, ty: T1): Self
  
  def withTypeVar[T1 <: T, R](name: String, ty: T1)(f: Self => R): R = f(addTypeVar(name, ty))
  
  def withTypeVars[T1 <: T, R](vars: Map[String, T1])(f: Self => R): R = {
    f(vars.foldLeft(this.asInstanceOf[Self]) { case (env, (name, ty)) => env.addTypeVar(name, ty).asInstanceOf[Self] })
  }
  
  def withTypeVars[T1 <: T, R](vars: (String, T1)*)(f: Self => R): R = {
    withTypeVars(vars.toMap)(f)
  }
  
  def freshTypeName[T1 <: T](bodies: T1*): String = {
    Iterator.from(0).map(n => s"$$Type$$$n").find {
      name => !types.contains(name) && !bodies.exists(_.contains(name))
    }.get
  }
  
  def withFreshTypeName[R](bodies: T*)(f: String => R): R = {
    val name = freshTypeName(bodies *)
    f(name)
  }
  
  def withFreshTypeBinding[R](ty: T)(bodies: T*)(f: (String, TypeEnvironment[T]) => R): R = {
    val name = freshTypeName(bodies *)
    f(name, addTypeVar(name, ty))
  }
}

case class TypeOnlyEnvironment[T <: RecNamed](
  override val types: Map[String, T],
) extends TypeEnvironment[T] {

  type Self = TypeOnlyEnvironment[T]
  
  override def addTypeVar[T1 <: T](name: String, ty: T1): TypeOnlyEnvironment[T] = {
    copy(types = types + (name -> ty))
  }
}

extension (env: TypeEnvironment[Type]) {
  def withFreshTypeVar[R](bodies: Type*)(f: (Type.Var, TypeEnvironment[Type]) => R): R = {
    val name = env.freshTypeName(bodies *)
    val freshVar: Type.Var = Type.Var(name)
    f(freshVar, env.addTypeVar(name, Type.Var(name)))
  }
}

case class Environment[T <: RecNamed, V <: RecNamed](
  types: Map[String, T] = Map.empty,
  values: Map[String, V] = Map.empty,
) extends TypeEnvironment[T] {

  type Self = Environment[T, V]
  
  def typeEnv: TypeEnvironment[T] = TypeOnlyEnvironment(types)
  
  override def addTypeVar[T1 <: T](name: String, ty: T1): Environment[T, V] = {
    copy(types = types + (name -> ty))
  }

  def addValueVar[V1 <: V](name: String, term: V1): Environment[T, V] = {
    copy(values = values + (name -> term))
  }

  override def withTypeVar[T1 <: T, R](name: String, ty: T1)(f: Environment[T, V] => R): R = f(addTypeVar(name, ty))
  
  override def withTypeVars[T1 <: T, R](vars: Map[String, T1])(f: Environment[T, V] => R): R = {
    f(copy(types = types ++ vars))
  }

  override def withTypeVars[T1 <: T, R](vars: (String, T1)*)(f: Environment[T, V] => R): R = {
    f(copy(types = types ++ vars.toMap))
  }

  def withValueVar[V1 <: V, R](name: String, term: V1)(f: Environment[T, V] => R): R = {
    f(addValueVar(name, term))
  }
  
  def withValueVars[V1 <: V, R](vars: Map[String, V1])(f: Environment[T, V] => R): R = {
    f(copy(values = values ++ vars))
  }

  def withValueVars[V1 <: V, R](vars: (String, V1)*)(f: Environment[T, V] => R): R = {
    f(copy(values = values ++ vars.toMap))
  }

  def freshVarName[V1 <: V](bodies: V1*): String = {
    Iterator.from(0).map(n => s"$$value$$$n").find {
      name => !values.contains(name) && !bodies.exists(_.contains(name))
    }.get
  }
}

object Environment {

  def empty[T <: RecNamed, V <: RecNamed]: Environment[T, V] = Environment(
    types = Map.empty[String, T],
    values = Map.empty[String, V],
  )

  def builder[T <: RecNamed, V <: RecNamed]: Builder[T, V] = Builder()

  class Builder[T <: RecNamed, V <: RecNamed] {
    private var env : Environment[T, V] = Environment.empty

    def typeVar(name: String, ty: T): Builder[T, V] = {
      env = env.addTypeVar(name, ty)
      this
    }

    def valueVar(name: String, term: V): Builder[T, V] = {
      env = env.addValueVar(name, term)
      this
    }

    def build(): Environment[T, V] = env

    def buildWith(f: Builder[T, V] => Unit): Environment[T, V] = {
      f(this)
      this.build()
    }
  }
}
