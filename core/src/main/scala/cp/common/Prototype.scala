package cp.common

import cp.common.{Environment, NativePrototype, Prototype}
import cp.util.Identified

trait Prototype[Id, T <: Identified[Id]] {
  def name: String
  def paramTypes: Seq[T]
  def returnType: T
  def isPure: Boolean
  final def arity: Int = paramTypes.length
}

trait CallablePrototype[Id, T <: Identified[Id], V <: Identified[Id]] extends Prototype[Id, T] {
  def call(args: Seq[V])(env: Environment[Id, T, V]): V
}

trait NativePrototype[Id, T <: Identified[Id]] extends Prototype[Id, T]

case class NativePrototypePure[Id, T <: Identified[Id]](
  override val name: String,
  override val paramTypes: Seq[T],
  override val returnType: T
) extends NativePrototype[Id, T] {
  override def isPure: Boolean = true
}

case class NativePrototypeImpure[Id, T <: Identified[Id]](
  override val name: String,
  override val paramTypes: Seq[T],
  override val returnType: T
) extends NativePrototype[Id, T] {
  override def isPure: Boolean = false
}
