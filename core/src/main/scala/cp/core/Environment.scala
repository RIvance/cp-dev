package cp.core

import cp.util.{IdentifiedByString, Identified}

trait TypeEnvironment[Id, T <: Identified[Id]] { self =>
  
  type Self <: TypeEnvironment[Id, T]

  def types: Map[Id, T]

  def ty(id: Id): T = types(id)

  def getType(id: Id): Option[T] = types.get(id)
  
  def addTypeVar[T1 <: T](id: Id, ty: T1): Self
  
  def withTypeVar[T1 <: T, R](id: Id, ty: T1)(f: Self => R): R = f(addTypeVar(id, ty))
  
  def withTypeVars[T1 <: T, R](vars: Map[Id, T1])(f: Self => R): R = {
    f(vars.foldLeft(this.asInstanceOf[Self]) { case (env, (id, ty)) => env.addTypeVar(id, ty).asInstanceOf[Self] })
  }
  
  def withTypeVars[T1 <: T, R](vars: (Id, T1)*)(f: Self => R): R = {
    withTypeVars(vars.toMap)(f)
  }
}

extension [T <: IdentifiedByString](env: TypeEnvironment[String, T]) {
  def freshTypeName[T1 <: T](bodies: T1*): String = {
    Iterator.from(0).map(n => s"$$Type$$$n").find {
      id => !env.types.contains(id) && !bodies.exists(_.contains(id))
    }.get
  }

  def withFreshTypeName[R](bodies: T*)(f: String => R): R = {
    val id = env.freshTypeName(bodies *)
    f(id)
  }

  def withFreshTypeBinding[R](ty: T)(bodies: T*)(f: (String, TypeEnvironment[String, T]) => R): R = {
    val id = freshTypeName(bodies *)
    f(id, env.addTypeVar(id, ty))
  }
}

case class TypeOnlyEnvironment[Id, T <: Identified[Id]](
  override val types: Map[Id, T],
) extends TypeEnvironment[Id, T] {

  type Self = TypeOnlyEnvironment[Id, T]
  
  override def addTypeVar[T1 <: T](id: Id, ty: T1): TypeOnlyEnvironment[Id, T] = {
    copy(types = types + (id -> ty))
  }
}

extension (env: TypeEnvironment[String, Type]) {
  def withFreshTypeVar[R](bodies: Type*)(f: (Type.Var, TypeEnvironment[String, Type]) => R): R = {
    val id = env.freshTypeName(bodies *)
    val freshVar: Type.Var = Type.Var(id)
    f(freshVar, env.addTypeVar(id, Type.Var(id)))
  }
}

case class Environment[Id, T <: Identified[Id], V <: Identified[Id]](
  types: Map[Id, T] = Map.empty,
  values: Map[Id, V] = Map.empty,
) extends TypeEnvironment[Id, T] {

  type Self = Environment[Id, T, V]

  def value(id: Id): V = values(id)

  def getValue(id: Id): Option[V] = values.get(id)

  def get(id: Id): Option[(V, T)] = for {
    value <- getValue(id)
    ty <- getType(id)
  } yield (value, ty)

  def apply(id: Id): (V, T) = (value(id), ty(id))
  
  def typeEnv: TypeEnvironment[Id, T] = TypeOnlyEnvironment(types)
  
  override def addTypeVar[T1 <: T](id: Id, ty: T1): Environment[Id, T, V] = {
    copy(types = types + (id -> ty))
  }

  def addValueVar[V1 <: V](id: Id, term: V1): Environment[Id, T, V] = {
    copy(values = values + (id -> term))
  }

  override def withTypeVar[T1 <: T, R](id: Id, ty: T1)(f: Environment[Id, T, V] => R): R = f(addTypeVar(id, ty))
  
  override def withTypeVars[T1 <: T, R](vars: Map[Id, T1])(f: Environment[Id, T, V] => R): R = {
    f(copy(types = types ++ vars))
  }

  override def withTypeVars[T1 <: T, R](vars: (Id, T1)*)(f: Environment[Id, T, V] => R): R = {
    f(copy(types = types ++ vars.toMap))
  }

  def withValueVar[V1 <: V, R](id: Id, term: V1)(f: Environment[Id, T, V] => R): R = {
    f(addValueVar(id, term))
  }
  
  def withValueVars[V1 <: V, R](vars: Map[Id, V1])(f: Environment[Id, T, V] => R): R = {
    f(copy(values = values ++ vars))
  }

  def withValueVars[V1 <: V, R](vars: (Id, V1)*)(f: Environment[Id, T, V] => R): R = {
    f(copy(values = values ++ vars.toMap))
  }
}

extension [T <: IdentifiedByString, V <: IdentifiedByString](env: Environment[String, T, V]) {
  def freshVarName[V1 <: V](bodies: V1*): String = {
    Iterator.from(0).map(n => s"$$value$$$n").find {
      id => !env.values.contains(id) && !bodies.exists(_.contains(id))
    }.get
  }
}

object Environment {

  def empty[Id, T <: Identified[Id], V <: Identified[Id]]: Environment[Id, T, V] = Environment(
    types = Map.empty[Id, T],
    values = Map.empty[Id, V],
  )

  def builder[Id, T <: Identified[Id], V <: Identified[Id]]: Builder[Id, T, V] = Builder()

  class Builder[Id, T <: Identified[Id], V <: Identified[Id]] {
    private var env : Environment[Id, T, V] = Environment.empty

    def typeVar(id: Id, ty: T): Builder[Id, T, V] = {
      env = env.addTypeVar(id, ty)
      this
    }

    def valueVar(id: Id, term: V): Builder[Id, T, V] = {
      env = env.addValueVar(id, term)
      this
    }

    def build(): Environment[Id, T, V] = env

    def buildWith(f: Builder[Id, T, V] => Unit): Environment[Id, T, V] = {
      f(this)
      this.build()
    }
  }
}
