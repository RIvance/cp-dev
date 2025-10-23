package cp.core

import cp.common.TypeEnvironment
import cp.error.CoreErrorKind.*
import cp.core.PrimitiveType.*
import cp.util.IdentifiedByString

import scala.annotation.targetName

enum Type extends IdentifiedByString {

  private type TypeEnv = TypeEnvironment[String, Type]

  case Var(name: String)
  
  case Primitive(ty: PrimitiveType)
  
  case Arrow(domain: Type, codomain: Type, isTrait: Boolean = false)
  
//  case Trait(domain: Type, codomain: Type)
  
  case Forall(
    paramName: String, codomain: Type, 
    constraints: Set[Constraint[Type]] = Set.empty,
  )
  
  // Signature is a special form of forall type for accepting sorts.
  // It should only exist in type synthesis phase.
  // After that, it should be treated as a normal forall type.
  case Signature(sortInName: String, sortOutName: String, body: Type)
  
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

    case Arrow(domain, codomain, isTrait) => {
      Arrow(domain.subst(from, replacement), codomain.subst(from, replacement), isTrait)
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

    case Signature(sortInName, sortOutName, body) => {
      val bodySubst = if sortInName == from || sortOutName == from then body
      else body.subst(from, replacement)
      Signature(sortInName, sortOutName, bodySubst)
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
  
  infix def == (that: Type)(using env: TypeEnv): Boolean = {
    val normThis = this.normalize
    val normThat = that.normalize
    (normThis unify normThat) && (normThat unify normThis)
  }

  infix def unify(that: Type)(using env: TypeEnv): Boolean = {
    
    val normThis = this.normalize
    val normThat = that.normalize

    (normThis, normThat) match {

      case (Var(name1), Var(name2)) => {
        name1 == name2 || {
          env.types.get(name1).exists(_.unify(normThat)) || env.types.get(name2).exists(normThis.unify)
        }
      }
      
      case (Primitive(ty1), Primitive(ty2)) => ty1 == ty2

      case (Primitive(UnitType), Tuple(Nil)) => true
      case (Tuple(Nil), Primitive(UnitType)) => true
      case (Primitive(UnitType), Record(fields)) if fields.isEmpty => true
      case (Record(fields), Primitive(UnitType)) if fields.isEmpty => true

      case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
        val typesToCheck = Seq(codomain1, codomain2) ++ constraints1.map(_.subject) ++ constraints2.map(_.subject)
        env.withFreshTypeVar(typesToCheck*) { (freshVar, newEnv) =>
          given TypeEnv = newEnv
          codomain1.subst(param1, freshVar).unify(codomain2.subst(param2, freshVar)) && {
            constraints1.size == constraints2.size && constraints1.forall { c1 =>
              constraints2.exists { c2 => c1.map(_.subst(param1, freshVar)) unify c2.map(_.subst(param2, freshVar)) }
            }
          }
        }
      }

      case (Arrow(domain1, codomain1, isTrait1), Arrow(domain2, codomain2, isTrait2)) => {
        isTrait1 == isTrait2 && domain1.unify(domain2) && codomain1.unify(codomain2)
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
        env.withFreshTypeVar(body1, body2) { (freshVar, newEnv) =>
          given TypeEnv = newEnv
          body1.subst(name1, freshVar).unify(body2.subst(name2, freshVar))
        }
      }
      
      case (Apply(func1, arg1), Apply(func2, arg2)) => {
        func1.unify(func2) && arg1.unify(arg2)
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
  infix def <:< (that: Type)(using env: TypeEnv): Boolean = {

    if this == that then return true

    val normThis = this.normalize
    val normThat = that.normalize

    if normThis unify normThat then return true

    (normThis, normThat) match {
      
      case (_, Primitive(TopType)) => true
      case (Primitive(BottomType), _) => true

      case (Var(name), _) => {
        env.types.get(name).exists(_.unify(normThat)) || normThis.unify(normThat)
      }
      
      case (_, Var(name)) => {
        env.types.get(name).exists(normThis.unify) || normThis.unify(normThat)
      }

      case (Arrow(domain1, codomain1, isTrait1), Arrow(domain2, codomain2, isTrait2)) => {
        isTrait1 == isTrait2 && domain2 <:< domain1 && codomain1 <:< codomain2
      }

      // (A -> B) & (C -> D) <: (A & C) -> (B & D)
      case (Intersection(Arrow(a, b, isTrait1), Arrow(c, d, isTrait2)), Arrow(ac, bd, isTrait3)) => {
        isTrait1 == isTrait2 && isTrait2 == isTrait3 && (ac <:< Type.Intersection(a, c)) && (Type.Intersection(b, d) <:< bd)
      }

      case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
        val typesToCheck = Seq(codomain1, codomain2) ++ constraints1.map(_.subject) ++ constraints2.map(_.subject)
        env.withFreshTypeVar(typesToCheck*) { (freshVar, newEnv) =>
          given TypeEnv = newEnv
          codomain1.subst(param1, freshVar) <:< codomain2.subst(param2, freshVar) && {
            // Since parameter are contravariant, the constraints on `this` should be
            //  weaker (i.e., include more elements, or `:>`) than those on `that`
            // Upperbounds: for all i and j, Ui1 <: Uj2
            constraints1.forall {
              case Constraint.UpperBound(_, upper1) => constraints2.exists {
                case Constraint.UpperBound(_, upper2) => upper1 <:< upper2
                case _ => false
              }
              case _ => true
            }
            // Lowerbounds: for all i and j, Li2 <: Li1
            && constraints2.forall {
              case Constraint.LowerBound(_, lower2) => constraints1.exists {
                case Constraint.LowerBound(_, lower1) => lower2 <:< lower1
                case _ => false
              }
              case _ => true
            }
            // Disjointness: for all Dj2 there exists Di1, Di1 <: Dj2
            && constraints2.forall {
              case Constraint.Disjoint(_, disjoint2) => constraints1.exists {
                case Constraint.Disjoint(_, disjoint1) => disjoint1 <:< disjoint2
                case _ => false
              }
              case _ => true
            }
          }
        }
      }
      
      case (Intersection(l1, r1), _) => l1 <:< normThat || r1 <:< normThat
      
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
      
      case (Diff(l1, r1), Diff(l2, r2)) => l1.unify(l2) && r1.unify(r2)
      
      case (left, right) => left.unify(right)
    }
  }

  def normalize(using env: TypeEnv): Type = this match {
    
    case Var(name) => env.types.get(name) match {
      case Some(Var(newName)) if name == newName => this
      case Some(resolvedType) => resolvedType.normalize
      case None => this
    }
    
    case Primitive(ty) => this

    case Arrow(domain, codomain, isTrait) => Arrow(domain.normalize, codomain.normalize, isTrait)

    case Forall(param, codomain, constraints) => {
      env.withTypeVar(param, Type.Var(param)) { implicit newEnv =>
        given TypeEnv = newEnv
        Type.Forall(
          paramName = param,
          codomain.normalize,
          constraints.compact.map(_.rename(param))
        )
      }
    }

    case Signature(sortInName, sortOutName, body) => {
      env.withTypeVars(
        sortInName -> Type.Var(sortInName),
        sortOutName -> Type.Var(sortOutName),
      ) { implicit newEnv =>
        given TypeEnv = newEnv
        Signature(sortInName, sortOutName, body.normalize)
      }
    }

    case Intersection(lhs, rhs) => {
      val normalizedLhs = lhs.normalize
      val normalizedRhs = rhs.normalize
      (normalizedLhs, normalizedRhs) match {
        case (Primitive(BottomType), _) => Primitive(BottomType)
        case (_, Primitive(BottomType)) => Primitive(BottomType)
        case (Primitive(TopType), right) => right
        case (left, Primitive(TopType)) => left
        case _ if normalizedLhs == normalizedRhs => normalizedLhs
        case (Record(fields1), Record(fields2)) => {
          val commonFields = fields1.keySet.intersect(fields2.keySet)
          val disjointFields = fields1.keySet.diff(fields2.keySet) ++ fields2.keySet.diff(fields1.keySet)
          // Merge the two records into one single record type, for common fields, take the intersection of their types
          val fields = (disjointFields.map { label =>
            if fields1.contains(label) then label -> fields1(label)
            else label -> fields2(label)
          } ++ commonFields.map { label =>
            label -> fields1(label).normalize.merge(fields2(label).normalize)
          }).toMap
          Record(fields)
        }
        case _ => Intersection(normalizedLhs, normalizedRhs)
      }
    }
    
    case Union(lhs, rhs) => {
      val normalizedLhs = lhs.normalize
      val normalizedRhs = rhs.normalize
      (normalizedLhs, normalizedRhs) match {
        case (Primitive(BottomType), right) => right
        case (left, Primitive(BottomType)) => left
        case (Primitive(TopType), _) => Primitive(TopType)
        case (_, Primitive(TopType)) => Primitive(TopType)
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
      given TypeEnv = env.addTypeVar(name, Type.Var(name))
      Fixpoint(name, body.normalize)
    }

    case Diff(lhs, rhs) => {
      val normalizedLhs = lhs.normalize
      val normalizedRhs = rhs.normalize
      (normalizedLhs, normalizedRhs) match {
        case (left, Primitive(BottomType)) => left
        case (Primitive(BottomType), _) => Primitive(BottomType)
        case _ if normalizedLhs == normalizedRhs => Primitive(BottomType)
        case _ => Diff(normalizedLhs, normalizedRhs)
      }
    }
  }

  private def isTopLike: Boolean = this match {
    case Primitive(TopType) => true
    // case Record(fields) if fields.isEmpty => true
    case _ => false
  }

  private def isBottomLike: Boolean = this match {
    case Primitive(BottomType) => true
    case Arrow(domain, _, _) => domain.isBottomLike
    case Intersection(left, right) => left.isBottomLike || right.isBottomLike
    case Union(left, right) => left.isBottomLike && right.isBottomLike
    case Record(fields) => fields.values.exists(_.isBottomLike)
    case Tuple(elements) => elements.exists(_.isBottomLike)
    case Forall(_, codomain, _) => codomain.isBottomLike
    case Fixpoint(_, body) => body.isBottomLike
    case _ => false
  }

  // TODO: this implementation need to be revisited
  def disjointWith(that: Type)(using env: TypeEnv): Boolean = (this, that) match {

    case (left, right) if left == right => false

    case (Var(name), other) => {
      env.types.get(name).forall(_.disjointWith(other))
    }

    case (other, Var(name)) => {
      env.types.get(name).forall(other.disjointWith)
    }

    // TODO: Figure out why this case returns true in the previous implementation
    case (left, right) if left.isTopLike || right.isTopLike => false
    case (left, right) if left.isBottomLike || right.isBottomLike => true

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
        val disjoint = Primitive(TopType) // A type that is guaranteed to be disjoint with any other type
        val typesToCheck = Seq(codomain1, codomain2) ++ constraints1.map(_.subject) ++ constraints2.map(_.subject)
        env.withFreshTypeBinding(disjoint)(typesToCheck*) { (freshVarName, newEnv) =>
          given TypeEnv = newEnv
          val freshVar = Type.Var(freshVarName)
          codomain1.subst(param1, freshVar) disjointWith codomain2.subst(param2, freshVar)
        }
      }
    }
    
    case (_, Type.Primitive(BottomType)) => false
    
    case (Type.Primitive(BottomType), _) => false
    
    case (left, right) => left != right
  }

  infix def diff(that: Type)(using env: TypeEnv): Type = (this, that) match {

    case (left, right) if left <:< right => Type.bottom
    case (left, right) if right.isTopLike => Type.bottom
    // In fact, we cannot "dig a hole" in top type (universal set)
    //  so we just return top type here
    case (left, right) if left.isTopLike => Type.top
    case (left, right) if left unify right => Type.bottom
    case (left, right) if left.split.isDefined => {
      val (left1, left2) = left.split.get
      left1.diff(right).merge(left2.diff(right)).normalize
    }

    // Still, why does the original implementation return top here???
    case (Primitive(BottomType), Primitive(BottomType)) => Type.bottom
    case (Primitive(BottomType), _) => Type.bottom
    case (_, Primitive(BottomType)) => this.normalize

    case (Record(fields1), Record(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      if commonFields.isEmpty then this.normalize else {
        val diffFields = commonFields.map { label =>
          label -> fields1(label).diff(fields2(label)).normalize
        }.filterNot { case (_, ty) => ty == Type.bottom }.toMap
        if diffFields.isEmpty then Type.bottom
        else Record(fields1 -- commonFields ++ diffFields).normalize
      }
    }

    case (Tuple(elements1), Tuple(elements2)) => {
      if elements1.length != elements2.length then this else {
        val diffElements = elements1.zip(elements2).map { case (ty1, ty2) => ty1.diff(ty2).normalize }
        if diffElements.contains(Type.bottom) then Type.bottom
        else Tuple(diffElements).normalize
      }
    }

    // For function-like types:
    // If lfs is a subtype of rfs, then lfs \ rfs = ⊥
    // For other cases, we just simply return lfs
    //  However, this is correct but not precise when domain1 and domain2 are not disjoint
    //  as in current implementation, we don't have a way to represent such type
    //  (we cannot "dig a hole" in the domain type)
    // We might need a better representation of such type in the future if we
    //  want to do reasoning on the type system.
    // But for now, this should be fine for a practical implementation.
    case (Arrow(domain1, codomain1, _), Arrow(domain2, codomain2, _)) => {
      if (domain2 <:< domain1) && (codomain1 <:< codomain2) then Type.bottom else this.normalize
    }

    case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
      val typesToCheck = Seq(codomain1, codomain2) ++ constraints1.map(_.subject) ++ constraints2.map(_.subject)
      env.withFreshTypeVar(typesToCheck*) { (freshVar, newEnv) =>
        given TypeEnv = newEnv
        val codomainDiff = codomain1.subst(param1, freshVar).diff(codomain2.subst(param2, freshVar)).normalize
        if constraints2.forall(_.verify(codomainDiff)) then this
        else Forall(
          param1,
          codomainDiff.subst(freshVar.name, Type.Var(param1)).normalize,
          // TODO: for constraints, we need to be more careful here
          //  we should compute constraints1 \ constraints2 instead of just taking constraints1
          //  but it's a bit tricky to define the difference of two sets of constraints
          //  so for now, we just take constraints1 that are not unified with any constraints in constraints2
          constraints1.filterNot { c1 => constraints2.exists(c2 => c1.unify(c2)) }.map(_.rename(param1))
        ).normalize
      }
    }

    case (Diff(left1, left2), right) => left1.diff(left2).diff(right).normalize
    case (left, Diff(right1, right2)) => left.diff(right1).merge(left.diff(right2)).normalize

    case (_: Fixpoint, _) | (_, _: Fixpoint) => UnsupportedFeature.raise {
      s"Diff operation is not supported for recursive types: ${this} \\ ${that}"
    }

    case (_: Ref, _) | (_, _: Ref) => UnsupportedFeature.raise {
      s"Diff operation is not supported for reference types: ${this} \\ ${that}"
    }

    case (left, right) => left.normalize
  }

  infix def merge(that: Type)(using env: TypeEnv): Type = (this, that) match {
    case (Arrow(domain1, codomain1, isTrait1), Arrow(domain2, codomain2, isTrait2)) 
      if isTrait1 == isTrait2 && (domain1 unify domain2) => 
      Arrow(domain1, codomain1.merge(codomain2).normalize, isTrait1)
    case (Record(fields1), Record(fields2)) 
      if fields1.keySet == fields2.keySet =>
      Record(fields1.map { case (label, ty1) => label -> ty1.merge(fields2(label)).normalize })
    case (Forall(param1, codomain1, constraints1), Forall(param2, codomain2, constraints2)) => {
      val typesToCheck = Seq(codomain1, codomain2) ++ constraints1.map(_.subject) ++ constraints2.map(_.subject)
      env.withFreshTypeVar(typesToCheck*) { (freshVar, newEnv) =>
        given TypeEnv = newEnv
        Forall(
          freshVar.name,
          codomain1.subst(param1, freshVar).merge(codomain2.subst(param2, freshVar)).normalize,
          (constraints1 ++ constraints2).compact.map(_.rename(freshVar.name))
        )
      }
    }
    case (left, right) => Intersection(left, right)
  }
  
  def split: Option[(Type, Type)] = this match {
    case Intersection(lhs, rhs) => Some((lhs, rhs))
    case Arrow(domain, codomain, isTrait) => codomain.split match {
      case Some((left, right)) => Some((Arrow(domain, left, isTrait), Arrow(domain, right, isTrait)))
      case None => None
    }
    case Record(fields) if fields.isEmpty => None
    case Record(fields) => {
      val splitFields = fields.map { case (label, ty) => label -> ty.split }
      if splitFields.exists(_._2.isEmpty) then None else {
        val leftFields = splitFields.map { (label, field) => label -> field.get._1 }
        val rightFields = splitFields.map { (label, field) => label -> field.get._2 }
        Some((Record(leftFields), Record(rightFields)))
      }
    }
    case Tuple(elements) => {
      val splitElements = elements.map(_.split)
      if splitElements.exists(_.isEmpty) then None else {
        val leftElements = splitElements.map(_.get._1)
        val rightElements = splitElements.map(_.get._2)
        Some((Tuple(leftElements), Tuple(rightElements)))
      }
    }
    case Forall(param, codomain, constraints) => {
      codomain.split.map { case (left, right) =>
        (Forall(param, left, constraints), Forall(param, right, constraints))
      }
    }
    case _ => None
  }
  
  override def contains(name: String): Boolean = this match {
    case Var(n) => n == name
    case Primitive(_) => false
    case Arrow(domain, codomain, _) => domain.contains(name) || codomain.contains(name)
    case Forall(param, codomain, constraints) => param != name && {
      codomain.contains(name) || constraints.exists(_.subject.contains(name))
    }
    case Signature(sortInName, sortOutName, body) => {
      sortInName != name && sortOutName != name && body.contains(name)
    }
    case Intersection(lhs, rhs) => lhs.contains(name) || rhs.contains(name)
    case Union(lhs, rhs) => lhs.contains(name) || rhs.contains(name)
    case Record(fields) => fields.values.exists(_.contains(name))
    case Tuple(elements) => elements.exists(_.contains(name))
    case Fixpoint(param, body) => param != name && body.contains(name)
    case Ref(ty) => ty.contains(name)
    case Apply(func, arg) => func.contains(name) || arg.contains(name)
    case Diff(lhs, rhs) => lhs.contains(name) || rhs.contains(name)
  }
  
  def getParamAndReturnTypes: (List[Type], Type) = this match {
    case Arrow(domain, codomain, _) => {
      val (params, ret) = codomain.getParamAndReturnTypes
      (domain :: params, ret)
    }
    // TODO: Handle intersection of function-like types
    case _ => (Nil, this)
  }
  
  def testApplicationReturn(argType: Type)(using env: TypeEnv): Option[Type] = this match {
    
    case Arrow(domain, codomain, _) => {
      if argType <:< domain then Some(codomain) else None
    }
    
    case Intersection(lhs, rhs) => {
      val lhsResult = lhs.testApplicationReturn(argType)
      val rhsResult = rhs.testApplicationReturn(argType)
      (lhsResult, rhsResult) match {
        case (Some(l), Some(r)) => Some(Intersection(l, r).normalize)
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case (None, None) => None
      }
    }
    
    case _ => None
  }
  
  // Add parentheses where necessary to ensure correct parsing.
  def toAtomString: String = this match {
    case Forall(_, _, _) => s"(${toString})"
    case Signature(_, _, _) => s"(${toString})"
    case Arrow(_, _, _) => s"(${toString})"
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
    
    case Arrow(domain, codomain, true) => s"${domain.toAtomString} -> ${codomain.toString}"
    
    case Arrow(domain, codomain, false) => s"${domain.toAtomString} => ${codomain.toString}"
    
    case Forall(param, codomain, constraints) => {
      val constraintsStr = if constraints.isEmpty then ""
      else constraints.map(_.toString).mkString(" | ") + " "
      s"∀$param $constraintsStr. ${codomain.toString}"
    }

    case Signature(sortInName, sortOutName, body) => {
      s"∀<$sortInName => $sortOutName> . ${body.toString}"
    }
    
    case Intersection(lhs, rhs) => s"${lhs.toAtomString} & ${rhs.toAtomString}"
    
    case Union(lhs, rhs) => s"${lhs.toAtomString} | ${rhs.toAtomString}"
    
    case Record(fields) => {
      s"{ ${fields.map { case (label, ty) => s"$label: $ty" }.mkString("; ")} }"
    }
    
    case Tuple(elements) => {
      s"(${elements.map(_.toString).mkString(", ")})"
    }
    
    case Fixpoint(name, body) => s"μ $name . ${body.toAtomString}"
    
    case Ref(ty) => s"&${ty.toAtomString}"
    
    case Apply(func, arg) => s"${func.toAtomString} ${arg.toAtomString}"
    
    case Diff(lhs, rhs) => s"${lhs.toAtomString} \\ ${rhs.toAtomString}"
  }
}

object Type {
  lazy val top: Type = Primitive(TopType)
  lazy val bottom: Type = Primitive(BottomType)
  lazy val unit: Type = Primitive(UnitType)
}
