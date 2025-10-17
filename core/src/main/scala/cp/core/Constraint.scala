package cp.core

import cp.syntax.ExprType

import scala.annotation.tailrec

enum Constraint[T <: Type | ExprType] {
  
  case Disjoint(
    override val targetTypeParam: String,
    disjoint: T,
  )

  case UpperBound(
    override val targetTypeParam: String,
    upperBound: T,
  )
  
  case LowerBound(
    override val targetTypeParam: String,
    lowerBound: T,
  )
  
  def targetTypeParam: String
  
  def map[U <: Type | ExprType](f: T => U): Constraint[U] = this match {
    case Constraint.Disjoint(param, disjoint) =>
      Constraint.Disjoint(param, f(disjoint))
    case Constraint.UpperBound(param, upperBound) =>
      Constraint.UpperBound(param, f(upperBound))
    case Constraint.LowerBound(param, lowerBound) =>
      Constraint.LowerBound(param, f(lowerBound))
  }
  
  def subject: T = this match {
    case Constraint.Disjoint(_, disjoint) => disjoint
    case Constraint.UpperBound(_, upperBound) => upperBound
    case Constraint.LowerBound(_, lowerBound) => lowerBound
  }
  
  def rename(paramName: String): Constraint[T] = this match {
    case Constraint.Disjoint(_, disjoint) =>
      Constraint.Disjoint(paramName, disjoint)
    case Constraint.UpperBound(_, upperBound) =>
      Constraint.UpperBound(paramName, upperBound)
    case Constraint.LowerBound(_, lowerBound) =>
      Constraint.LowerBound(paramName, lowerBound)
  }
}

object Constraint {
  type Env = TypeEnvironment[Type]
}

extension (constraint: Constraint[ExprType]) {
  def synthesize(using env: Constraint.Env): Constraint[Type] = {
    constraint.map(_.synthesize(using env)(using Set.empty))
  }
}

extension (constraint: Constraint[Type]) {
  def verify(ty: Type)(using env: Constraint.Env): Boolean = constraint match {
    case Constraint.Disjoint(_, disjoint) => ty.disjointWith(disjoint)
    case Constraint.UpperBound(_, upperBound) => ty <:< upperBound
    case Constraint.LowerBound(_, lowerBound) => lowerBound <:< ty
  }
  
  def unify(otherConstraint: Constraint[Type])(using env: Constraint.Env): Boolean = {
    (constraint, otherConstraint) match {
      case (
        Constraint.Disjoint(_, disjoint1),
        Constraint.Disjoint(_, disjoint2),
      ) => disjoint1 == disjoint2
      case (
        Constraint.UpperBound(_, upperBound1),
        Constraint.UpperBound(_, upperBound2),
      ) => upperBound1 unify upperBound2
      case (
        Constraint.LowerBound(_, lowerBound1),
        Constraint.LowerBound(_, lowerBound2),
      ) => lowerBound1 unify lowerBound2
      case _ => false
    }
  }
  
  infix def weakerThan(otherConstraint: Constraint[Type])(using env: Constraint.Env): Boolean = {
    (constraint, otherConstraint) match {
      case (
        Constraint.Disjoint(_, disjoint1),
        Constraint.Disjoint(_, disjoint2),
      ) => disjoint1 <:< disjoint2
      case (
        Constraint.UpperBound(_, upperBound1),
        Constraint.UpperBound(_, upperBound2),
      ) => upperBound2 <:< upperBound1
      case (
        Constraint.LowerBound(_, lowerBound1),
        Constraint.LowerBound(_, lowerBound2),
      ) => lowerBound1 <:< lowerBound2
      case _ => false
    }
  }
  
  // Judge whether type satisfying `constraint` guarantees to be disjoint with type satisfying `otherConstraint`
  // In other words, there is no type that satisfies both `constraint` and `otherConstraint`
  @tailrec
  infix def disjointWith(otherConstraint: Constraint[Type])(using env: Constraint.Env): Boolean = {
    // TODO: double check this logic
    (constraint, otherConstraint) match {
      case (
        Constraint.Disjoint(_, disjoint1),
        Constraint.Disjoint(_, disjoint2),
      ) => false
      case (_, Constraint.Disjoint(_, _)) => otherConstraint.disjointWith(constraint)
      case (
        Constraint.Disjoint(_, disjoint),
        Constraint.UpperBound(_, upperBound),
      ) => disjoint <:< upperBound
      case (
        Constraint.Disjoint(_, disjoint),
        Constraint.LowerBound(_, lowerBound),
      ) => lowerBound <:< disjoint
      // If one's upper bound is lower than another's lower bound, they are disjoint
      case (
        Constraint.UpperBound(_, upperBound),
        Constraint.LowerBound(_, lowerBound),
      ) => upperBound <:< lowerBound
      case (
        Constraint.LowerBound(_, lowerBound),
        Constraint.UpperBound(_, upperBound), 
      ) => upperBound <:< lowerBound
      case _ => false
    }
  }
}

extension (constraints: Set[Constraint[Type]]) {
  def compact(using env: Constraint.Env): Set[Constraint[Type]] = {
    var upperBound: Option[Type] = None
    var lowerBound: Option[Type] = None
    var disjoint: Option[Type] = None
    constraints.foreach {
      case Constraint.UpperBound(_, upperbound) => {
        upperBound = upperBound match {
          case Some(existingUpperbound) => Some(
            if existingUpperbound <:< upperbound then existingUpperbound
            else if upperbound <:< existingUpperbound then upperbound
            else Type.Intersection(existingUpperbound, upperbound)
          )
          case None => Some(upperbound.normalize)
        }
      }
      case Constraint.LowerBound(_, lowerbound) => {
        lowerBound = lowerBound match {
          case Some(existingLowerbound) => Some(
            if existingLowerbound <:< lowerbound then lowerbound
            else if lowerbound <:< existingLowerbound then existingLowerbound
            else Type.Union(existingLowerbound, lowerbound)
          )
          case None => Some(lowerbound.normalize)
        }
      }
      case Constraint.Disjoint(_, disjointType) => {
        disjoint = disjoint match {
          case Some(existingDisjoint) => Some(
            if existingDisjoint <:< disjointType then existingDisjoint
            else if disjointType <:< existingDisjoint then disjointType
            else Type.Union(existingDisjoint, disjointType)
          )
          case None => Some(disjointType.normalize)
        }
      }
    }
    Set(
      upperBound.map(Constraint.UpperBound("$T", _)),
      lowerBound.map(Constraint.LowerBound("$T", _)),
      disjoint.map(Constraint.Disjoint("$T", _)),
    ).flatten
  }
  
  // TODO: We need to implement a more sophisticated algorithm to compute the intersection, union
  //  and difference of constraints (especially difference), instead of just taking the union of the sets.
  
  def intersectWith(other: Set[Constraint[Type]])(using env: Constraint.Env): Set[Constraint[Type]] = ???
  
  def unionWith(other: Set[Constraint[Type]])(using env: Constraint.Env): Set[Constraint[Type]] = ???
  
  def diffWith(other: Set[Constraint[Type]])(using env: Constraint.Env): Set[Constraint[Type]] = ???
}
