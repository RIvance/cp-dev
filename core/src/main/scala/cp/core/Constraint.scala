package cp.core

import cp.syntax.ExprType

import scala.annotation.tailrec

enum Constraint[T <: Type | ExprType] {
  
  case DisjointConstraint(
    override val targetTypeParam: String,
    disjoint: T,
  )

  case UpperBoundConstraint(
    override val targetTypeParam: String,
    upperBound: T,
  )
  
  case LowerBoundConstraint(
    override val targetTypeParam: String,
    lowerBound: T,
  )
  
  def targetTypeParam: String
  
  def map[U <: Type | ExprType](f: T => U): Constraint[U] = this match {
    case Constraint.DisjointConstraint(param, disjoint) =>
      Constraint.DisjointConstraint(param, f(disjoint))
    case Constraint.UpperBoundConstraint(param, upperBound) =>
      Constraint.UpperBoundConstraint(param, f(upperBound))
    case Constraint.LowerBoundConstraint(param, lowerBound) =>
      Constraint.LowerBoundConstraint(param, f(lowerBound))
  }
  
  def subject: T = this match {
    case Constraint.DisjointConstraint(_, disjoint) => disjoint
    case Constraint.UpperBoundConstraint(_, upperBound) => upperBound
    case Constraint.LowerBoundConstraint(_, lowerBound) => lowerBound
  }
}

extension (constraint: Constraint[ExprType]) {
  def synthesis(using env: Environment): Constraint[Type] = constraint.map(_.synthesize)
}

extension (constraint: Constraint[Type]) {
  def check(ty: Type)(using env: Environment): Boolean = constraint match {
    case Constraint.DisjointConstraint(_, disjoint) => ty.disjointWith(disjoint)
    case Constraint.UpperBoundConstraint(_, upperBound) => ty <:< upperBound
    case Constraint.LowerBoundConstraint(_, lowerBound) => lowerBound <:< ty
  }
  
  def unify(otherConstraint: Constraint[Type])(using env: Environment): Boolean = {
    (constraint, otherConstraint) match {
      case (
        Constraint.DisjointConstraint(_, disjoint1),
        Constraint.DisjointConstraint(_, disjoint2),
      ) => disjoint1 == disjoint2
      case (
        Constraint.UpperBoundConstraint(_, upperBound1),
        Constraint.UpperBoundConstraint(_, upperBound2),
      ) => upperBound1 unify upperBound2
      case (
        Constraint.LowerBoundConstraint(_, lowerBound1),
        Constraint.LowerBoundConstraint(_, lowerBound2),
      ) => lowerBound1 unify lowerBound2
      case _ => false
    }
  }
  
  infix def weakerThan(otherConstraint: Constraint[Type])(using env: Environment): Boolean = {
    (constraint, otherConstraint) match {
      case (
        Constraint.DisjointConstraint(_, disjoint1),
        Constraint.DisjointConstraint(_, disjoint2),
      ) => disjoint1 <:< disjoint2
      case (
        Constraint.UpperBoundConstraint(_, upperBound1),
        Constraint.UpperBoundConstraint(_, upperBound2),
      ) => upperBound2 <:< upperBound1
      case (
        Constraint.LowerBoundConstraint(_, lowerBound1),
        Constraint.LowerBoundConstraint(_, lowerBound2),
      ) => lowerBound1 <:< lowerBound2
      case _ => false
    }
  }
  
  // Judge whether type satisfying `constraint` guarantees to be disjoint with type satisfying `otherConstraint`
  // In other words, there is no type that satisfies both `constraint` and `otherConstraint`
  @tailrec
  infix def disjointWith(otherConstraint: Constraint[Type])(using env: Environment): Boolean = {
    // TODO: double check this logic
    (constraint, otherConstraint) match {
      case (
        Constraint.DisjointConstraint(_, disjoint1),
        Constraint.DisjointConstraint(_, disjoint2),
      ) => false
      case (_, Constraint.DisjointConstraint(_, _)) => otherConstraint.disjointWith(constraint)
      case (
        Constraint.DisjointConstraint(_, disjoint),
        Constraint.UpperBoundConstraint(_, upperBound),
      ) => disjoint <:< upperBound
      case (
        Constraint.DisjointConstraint(_, disjoint),
        Constraint.LowerBoundConstraint(_, lowerBound),
      ) => lowerBound <:< disjoint
      // If one's upper bound is lower than another's lower bound, they are disjoint
      case (
        Constraint.UpperBoundConstraint(_, upperBound),
        Constraint.LowerBoundConstraint(_, lowerBound),
      ) => upperBound <:< lowerBound
      case (
        Constraint.LowerBoundConstraint(_, lowerBound),
        Constraint.UpperBoundConstraint(_, upperBound), 
      ) => upperBound <:< lowerBound
      case _ => false
    }
  }
}

extension (constraints: Set[Constraint[Type]]) {
  def compact(using env: Environment): Set[Constraint[Type]] = {
    var upperBound: Option[Type] = None
    var lowerBound: Option[Type] = None
    var disjoint: Option[Type] = None
    constraints.foreach {
      case Constraint.UpperBoundConstraint(_, upperbound) => {
        upperBound = upperBound match {
          case Some(existingUpperbound) => Some(
            if existingUpperbound <:< upperbound then existingUpperbound
            else if upperbound <:< existingUpperbound then upperbound
            else Type.Intersection(existingUpperbound, upperbound)
          )
          case None => Some(upperbound.normalize)
        }
      }
      case Constraint.LowerBoundConstraint(_, lowerbound) => {
        lowerBound = lowerBound match {
          case Some(existingLowerbound) => Some(
            if existingLowerbound <:< lowerbound then lowerbound
            else if lowerbound <:< existingLowerbound then existingLowerbound
            else Type.Union(existingLowerbound, lowerbound)
          )
          case None => Some(lowerbound.normalize)
        }
      }
      case Constraint.DisjointConstraint(_, disjointType) => {
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
      upperBound.map(Constraint.UpperBoundConstraint("$T", _)),
      lowerBound.map(Constraint.LowerBoundConstraint("$T", _)),
      disjoint.map(Constraint.DisjointConstraint("$T", _)),
    ).flatten
  }
  
  def rename(paramName: String): Set[Constraint[Type]] = {
    constraints.map {
      case Constraint.DisjointConstraint(_, disjoint) =>
        Constraint.DisjointConstraint(paramName, disjoint)
      case Constraint.UpperBoundConstraint(_, upperBound) =>
        Constraint.UpperBoundConstraint(paramName, upperBound)
      case Constraint.LowerBoundConstraint(_, lowerBound) =>
        Constraint.LowerBoundConstraint(paramName, lowerBound)
    }
  }
}
