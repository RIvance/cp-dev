package cp.core

import cp.common.{Environment, TypeEnvironment}
import cp.util.IdentifiedByString

extension (env: TypeEnvironment[String, Type]) {
  def withFreshTypeVar[R](bodies: Type*)(f: (Type.Var, TypeEnvironment[String, Type]) => R): R = {
    val id = env.freshTypeName(bodies *)
    val freshVar: Type.Var = Type.Var(id)
    f(freshVar, env.addTypeVar(id, Type.Var(id)))
  }
}

extension [T <: IdentifiedByString, V <: IdentifiedByString](env: Environment[String, T, V]) {
  def freshVarName[V1 <: V](bodies: V1*): String = {
    Iterator.from(0).map(n => s"$$value$$$n").find {
      id => !env.values.contains(id) && !bodies.exists(_.contains(id))
    }.get
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