package cp.core

import cp.error.CoreErrorKind.*

import scala.annotation.targetName

enum Type {
  
  case Var(name: String)
  
  case Primitive(ty: LiteralType)
  
  case Arrow(domain: Type, codomain: Type)
  
  case Trait(domain: Type, codomain: Type)
  
  case Forall(
    paramName: String, codomain: Type, 
    constraints: Set[Constraint[Type]] = Set.empty,
  )
  
  case Intersection(lhs: Type, rhs: Type)
  
  case Union(lhs: Type, rhs: Type)
  
  case Record(fields: Map[String, Type])
  
  case Tuple(elements: List[Type])
  
  case Fixpoint(name: String, body: Type)
  
  case Ref(ty: Type)
  
  case Apply(func: Type, arg: Type)
  
  case Diff(lhs: Type, rhs: Type)

  def subst(from: String, replacement: Type): Type = this match {
    
    case Var(name) => if (name == from) replacement else this

    case Primitive(_) => this

    case Arrow(domain, codomain) => {
      Arrow(domain.subst(from, replacement), codomain.subst(from, replacement))
    }

    case Trait(domain, codomain) => {
      Trait(domain.subst(from, replacement), codomain.subst(from, replacement))
    }

    case Fixpoint(name, body) => {
      if name == from then this else {
        Fixpoint(name, body.subst(from, replacement))
      }
    }

    case Forall(param, codomain, constraints) => {
      if param == from then this else {
        val newConstraints = constraints.map {
          constraint => constraint.map(_.subst(from, replacement))
        }
        Forall(param, codomain.subst(from, replacement), newConstraints)
      }
    }
    
    case Intersection(lhs, rhs) => {
      Intersection(lhs.subst(from, replacement), rhs.subst(from, replacement))
    }
    
    case Union(lhs, rhs) => {
      Union(lhs.subst(from, replacement), rhs.subst(from, replacement))
    }
    
    case Record(fields) => {
      Record(fields.view.mapValues(_.subst(from, replacement)).toMap)
    }

    case Tuple(elements) => {
      Tuple(elements.map(_.subst(from, replacement)))
    }
    
    case Ref(ty) => {
      Ref(ty.subst(from, replacement))
    }
    
    case Apply(func, arg) => {
      Apply(func.subst(from, replacement), arg.subst(from, replacement))
    }
    
    case Diff(lhs, rhs) => {
      Diff(lhs.subst(from, replacement), rhs.subst(from, replacement))
    }
  }
  
  infix def == (that: Type)(using env: Environment): Boolean = {
    val normThis = this.normalize
    val normThat = that.normalize
    (normThis unify normThat) && (normThat unify normThis)
  }

  infix def unify(that: Type)(using env: Environment): Boolean = {
    
    val normThis = this.normalize
    val normThat = that.normalize

    (normThis, normThat) match {

      case (Var(name1), Var(name2)) => {
        name1 == name2 || {
          env.typeVars.get(name1).exists(_.unify(normThat)) || env.typeVars.get(name2).exists(normThis.unify)
        }
      }
      
      case (Primitive(ty1), Primitive(ty2)) => ty1 == ty2
      
      case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
        env.withFreshTypeVar { (freshVar, newEnv) =>
          given Environment = newEnv
          codomain1.subst(param1, freshVar).unify(codomain2.subst(param2, freshVar)) && {
            constraints1.size == constraints2.size && constraints1.forall { c1 =>
              constraints2.exists { c2 => c1.map(_.subst(param1, freshVar)) unify c2.map(_.subst(param2, freshVar)) }
            }
          }
        }
      }

      case (Arrow(domain1, codomain1), Arrow(domain2, codomain2)) => {
        domain1.unify(domain2) && codomain1.unify(codomain2)
      }
      
      case (Intersection(l1, r1), Intersection(l2, r2)) => {
        (l1.unify(l2) && r1.unify(r2)) || (l1.unify(r2) && r1.unify(l2))
      }
      
      case (Union(l1, r1), Union(l2, r2)) => {
        (l1.unify(l2) && r1.unify(r2)) || (l1.unify(r2) && r1.unify(l2))
      }
      
      case (Record(fields1), Record(fields2)) => {
        fields1.keySet == fields2.keySet && fields1.forall { 
          case (label, ty1) => ty1.unify(fields2(label))
        }
      }

      case (Tuple(elements1), Tuple(elements2)) => {
        elements1.length == elements2.length && elements1.zip(elements2).forall {
          case (ty1, ty2) => ty1.unify(ty2)
        }
      }
      
      case (Ref(ty1), Ref(ty2)) => ty1.unify(ty2)

      case (Fixpoint(name1, body1), Fixpoint(name2, body2)) => {
        env.withFreshTypeVar { (freshVar, newEnv) =>
          given Environment = newEnv
          body1.subst(name1, freshVar).unify(body2.subst(name2, freshVar))
        }
      }
      
      case (Apply(func1, arg1), Apply(func2, arg2)) => {
        func1.unify(func2) && arg1.unify(arg2)
      }
      
      case (Trait(impl1, constr1), Trait(impl2, constr2)) => {
        impl1.unify(impl2) && constr1.unify(constr2)
      }
      
      case (Diff(l1, r1), Diff(l2, r2)) => l1.unify(l2) && r1.unify(r2)
    
      case (Intersection(l, r), other) => l.unify(other) && r.unify(other)
      
      case (other, Intersection(l, r)) => other.unify(l) && other.unify(r)
      
      case (Union(l, r), other) => l.unify(other) || r.unify(other)
      
      case (other, Union(l, r)) => other.unify(l) || other.unify(r)
      
      case _ => false
    }
  }

  @targetName("subtype")
  infix def <:< (that: Type)(using env: Environment): Boolean = {
    val normThis = this.normalize
    val normThat = that.normalize

    if normThis unify normThat then return true

    (normThis, normThat) match {
      
      case (_, Primitive(LiteralType.TopType)) => true
      case (Primitive(LiteralType.BottomType), _) => true

      case (Var(name), _) => {
        env.typeVars.get(name).exists(_.unify(normThat)) || normThis.unify(normThat)
      }
      
      case (_, Var(name)) => {
        env.typeVars.get(name).exists(normThis.unify) || normThis.unify(normThat)
      }

      case (Arrow(domain1, codomain1), Arrow(domain2, codomain2)) => {
        domain2 <:< domain1 && codomain1 <:< codomain2
      }
      
      case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
        env.withFreshTypeVar { (freshVar, newEnv) =>
          given Environment = newEnv
          codomain1.subst(param1, freshVar) <:< codomain2.subst(param2, freshVar) && {
            // Since parameter are contravariant, the constraints on `this` should be weaker than those on `that`
            // Upperbounds: for all i and j, Ui1 <: Uj2
            constraints1.forall {
              case Constraint.UpperBoundConstraint(_, upper1) => constraints2.exists {
                case Constraint.UpperBoundConstraint(_, upper2) => upper1 <:< upper2
                case _ => false
              }
              case _ => true
            }
            // Lowerbounds: for all i and j, Li2 <: Li1
            && constraints2.forall {
              case Constraint.LowerBoundConstraint(_, lower2) => constraints1.exists {
                case Constraint.LowerBoundConstraint(_, lower1) => lower2 <:< lower1
                case _ => false
              }
              case _ => true
            }
            // Disjointness: for all Dj2 there exists Di1, Di1 <: Dj2
            && constraints2.forall {
              case Constraint.DisjointConstraint(_, disjoint2) => constraints1.exists {
                case Constraint.DisjointConstraint(_, disjoint1) => disjoint1 <:< disjoint2
                case _ => false
              }
              case _ => true
            }
          }
        }
      }
      
      case (Intersection(l1, r1), _) => l1 <:< normThat && r1 <:< normThat
      
      case (_, Union(l2, r2)) => normThis <:< l2 || normThis <:< r2
      
      case (Union(l1, r1), _) => l1 <:< normThat && r1 <:< normThat
      
      case (_, Intersection(l2, r2)) => normThis <:< l2 && normThis <:< r2
      
      case (Record(fields1), Record(fields2)) => {
        fields2.keySet.subsetOf(fields1.keySet) && fields2.forall { 
          case (label, ty2) => fields1(label) <:< ty2
        }
      }

      case (Tuple(elements1), Tuple(elements2)) => {
        elements1.length == elements2.length && elements1.zip(elements2).forall {
          case (ty1, ty2) => ty1 <:< ty2
        }
      }
      
      case (Ref(ty1), Ref(ty2)) => ty1.unify(ty2) // Ref types are invariant

      case (Apply(func1, arg1), Apply(func2, arg2)) => func1.unify(func2) && arg1.unify(arg2)
      
      case (Trait(impl1, constr1), Trait(impl2, constr2)) => impl1.unify(impl2) && constr1 <:< constr2
      
      case (Diff(l1, r1), Diff(l2, r2)) => l1.unify(l2) && r1.unify(r2)
      
      case (left, right) => left.unify(right)
    }
  }

  def normalize(using env: Environment): Type = this match {
    
    case Var(name) => {
      env.typeVars.get(name) match {
        case Some(Var(newName)) if name == newName => this
        case Some(resolvedType) => resolvedType.normalize
        case None => this
      }
    }
    
    case Primitive(ty) => this

    case Arrow(domain, codomain) => Arrow(domain.normalize, codomain.normalize)

    case Trait(domain, codomain) => Trait(domain.normalize, codomain.normalize)

    case Forall(param, codomain, constraints) => {
      env.withFreshTypeVar { (freshVar, newEnv) =>
        given Environment = newEnv
        Type.Forall(
          paramName = param,
          codomain.subst(param, freshVar).normalize,
          constraints.compact.map(_.rename(param))
        )
      }
    }

    case Intersection(lhs, rhs) => {
      val normalizedLhs = lhs.normalize
      val normalizedRhs = rhs.normalize
      (normalizedLhs, normalizedRhs) match {
        case (Primitive(LiteralType.BottomType), _) => Primitive(LiteralType.BottomType)
        case (_, Primitive(LiteralType.BottomType)) => Primitive(LiteralType.BottomType)
        case (Primitive(LiteralType.TopType), right) => right
        case (left, Primitive(LiteralType.TopType)) => left
        case _ if normalizedLhs == normalizedRhs => normalizedLhs
        case _ => Intersection(normalizedLhs, normalizedRhs)
      }
    }
    
    case Union(lhs, rhs) => {
      val normalizedLhs = lhs.normalize
      val normalizedRhs = rhs.normalize
      (normalizedLhs, normalizedRhs) match {
        case (Primitive(LiteralType.BottomType), right) => right
        case (left, Primitive(LiteralType.BottomType)) => left
        case (Primitive(LiteralType.TopType), _) => Primitive(LiteralType.TopType)
        case (_, Primitive(LiteralType.TopType)) => Primitive(LiteralType.TopType)
        case _ if normalizedLhs == normalizedRhs => normalizedLhs
        case _ => Union(normalizedLhs, normalizedRhs)
      }
    }
    
    case Record(fields) => Record(fields.map { 
      case (label, fieldType) => label -> fieldType.normalize
    })

    case Tuple(elements) => Tuple(elements.map(_.normalize))
    
    case Ref(ty) => Ref(ty.normalize)

    case Apply(func, arg) => {
      val normalizedFunc: Type = func.normalize
      val normalizedArg: Type = arg.normalize
      normalizedFunc match {
        case Forall(param, codomain, constraints) => {
          constraints.foreach { constraint =>
            if !constraint.verify(normalizedArg) then ConstraintNotSatisfied.raise {
              s"Type argument ${normalizedArg} does not satisfy constraint ${constraint} in type application ${this}"
            }
          }
          // Beta reduction: (∀x . B) T → B[T/x]
          codomain.subst(param, normalizedArg).normalize
        }
        case _ => Apply(normalizedFunc, normalizedArg)
      }
    }

    case Fixpoint(name, body) => {
      given Environment = env.addTypeVar(name, Type.Var(name))
      Fixpoint(name, body.normalize)
    }

    case Diff(lhs, rhs) => {
      val normalizedLhs = lhs.normalize
      val normalizedRhs = rhs.normalize
      (normalizedLhs, normalizedRhs) match {
        case (left, Primitive(LiteralType.BottomType)) => left
        case (Primitive(LiteralType.BottomType), _) => Primitive(LiteralType.BottomType)
        case _ if normalizedLhs == normalizedRhs => Primitive(LiteralType.BottomType)
        case _ => Diff(normalizedLhs, normalizedRhs)
      }
    }
  }

  private def isTopLike: Boolean = this match {
    case Primitive(LiteralType.TopType) => true
    case Arrow(_, codomain) => codomain.isTopLike
    case Intersection(left, right) => left.isTopLike && right.isTopLike
    case Union(left, right) => left.isTopLike || right.isTopLike
    case Record(fields) => fields.values.forall(_.isTopLike)
    case Tuple(elements) => elements.forall(_.isTopLike)
    case Forall(_, codomain, _) => codomain.isTopLike
    case Fixpoint(_, body) => body.isTopLike
    case _ => false
  }

  // TODO: this implementation need to be revisited
  def disjointWith(that: Type)(using env: Environment): Boolean = (this, that) match {

    case (left, right) if left == right => false

    case (Var(name), other) => {
      env.typeVars.get(name).forall(_.disjointWith(other))
    }

    case (other, Var(name)) => {
      env.typeVars.get(name).forall(other.disjointWith)
    }
    
    // TODO: Figure out why this case returns true in the previous implementation
    case (left, right) if left.isTopLike || right.isTopLike => false

    case (Record(firstFields), Record(secondFields)) => {
      firstFields.forall { case (fieldName, fieldType) =>
        secondFields.get(fieldName) match {
          case Some(otherFieldType) => fieldType.disjointWith(otherFieldType)
          case None => true
        }
      }
    }
    
    case (Intersection(l1, r1), other) => {
      l1.disjointWith(other) || r1.disjointWith(other)
    }
    
    case (other, Intersection(l2, r2)) => {
      other.disjointWith(l2) || other.disjointWith(r2)
    }
    
    case (Union(l1, r1), other) => {
      l1.disjointWith(other) && r1.disjointWith(other)
    }
    
    case (other, Union(l2, r2)) => {
      other.disjointWith(l2) && other.disjointWith(r2)
    }

    case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
      if constraints1.forall {
        constraint1 => constraints2.exists {
          constraint2 => constraint1.disjointWith(constraint2)
        }
      } then true else {
        // TODO: We need to check whether it's okay to use top type here
        // No disjointness constraints, we need to check the bodies
        val disjoint = Primitive(LiteralType.TopType) // A type that is guaranteed to be disjoint with any other type
        env.withFreshTypeBinding(disjoint) { (freshVar, newEnv) =>
          given Environment = newEnv
          codomain1.subst(param1, freshVar) disjointWith codomain2.subst(param2, freshVar)
        }
      }
    }
    
    case (_, Type.Primitive(LiteralType.BottomType)) => false
    
    case (Type.Primitive(LiteralType.BottomType), _) => false
    
    case (left, right) => left != right
  }
  
  // Add parentheses where necessary to ensure correct parsing.
  def toAtomString: String = this match {
    case Forall(_, _, _) => s"(${toString})"
    case Arrow(_, _) => s"(${toString})"
    case Trait(_, _) => s"(${toString})"
    case Apply(_, _) => s"(${toString})"
    case Intersection(_, _) => s"(${toString})"
    case Union(_, _) => s"(${toString})"
    case Fixpoint(_, _) => s"(${toString})"
    case Diff(_, _) => s"(${toString})"
    case _ => toString
  }

  override def toString: String = this match {
    
    case Var(name) => name
    
    case Primitive(ty) => ty.toString
    
    case Arrow(domain, codomain) => s"${domain.toAtomString} -> ${codomain.toString}"
    
    case Trait(domain, codomain) => s"${domain.toAtomString} ~> ${codomain.toString}"
    
    case Forall(param, codomain, constraints) => {
      val constraintsStr = if constraints.isEmpty then ""
      else constraints.map(_.toString).mkString(" | ") + " "
      s"forall $param . $constraintsStr-> ${codomain.toString}"
    }
    
    case Intersection(lhs, rhs) => s"${lhs.toAtomString} & ${rhs.toAtomString}"
    
    case Union(lhs, rhs) => s"${lhs.toAtomString} | ${rhs.toAtomString}"
    
    case Record(fields) => {
      val fieldsStr = fields.map { case (label, ty) => s"$label: $ty" }.mkString("; ")
      s"{ $fieldsStr }"
    }
    
    case Tuple(elements) => {
      val elementsStr = elements.map(_.toString).mkString(", ")
      s"($elementsStr)"
    }
    
    case Fixpoint(name, body) => s"μ $name . ${body.toAtomString}"
    
    case Ref(ty) => s"&${ty.toAtomString}"
    
    case Apply(func, arg) => s"${func.toAtomString} ${arg.toAtomString}"
    
    case Diff(lhs, rhs) => s"${lhs.toAtomString} \\ ${rhs.toAtomString}"
  }
}

object Type {
  lazy val top: Type = Primitive(LiteralType.TopType)
  lazy val bottom: Type = Primitive(LiteralType.BottomType)
}
