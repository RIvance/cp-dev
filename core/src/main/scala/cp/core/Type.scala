package cp.core

enum Type {
  case Var(name: String)
  case Primitive(ty: LiteralType)
  case Forall(domain: Type, paramName: Option[String], codomain: Type)
  case Intersection(lhs: Type, rhs: Type)
  case Union(lhs: Type, rhs: Type)
  case Record(fields: Map[String, Type])
  case Ref(ty: Type)
  case Apply(func: Type, arg: Type)
  case Trait(impl: Type, constraint: Type)
  case Diff(lhs: Type, rhs: Type)

  def subst(from: String, replacement: Type): Type = this match {
    case Var(name) => if (name == from) replacement else this

    case Primitive(_) => this

    case Forall(domain, paramName, codomain) =>
      paramName match {
        case Some(param) if param == from =>
          // Don't substitute in codomain if the bound variable shadows 'from'
          Forall(domain.subst(from, replacement), paramName, codomain)
        case _ =>
          Forall(
            domain.subst(from, replacement),
            paramName,
            codomain.subst(from, replacement)
          )
      }

    case Intersection(lhs, rhs) =>
      Intersection(lhs.subst(from, replacement), rhs.subst(from, replacement))

    case Union(lhs, rhs) =>
      Union(lhs.subst(from, replacement), rhs.subst(from, replacement))

    case Record(fields) =>
      Record(fields.view.mapValues(_.subst(from, replacement)).toMap)

    case Ref(ty) =>
      Ref(ty.subst(from, replacement))

    case Apply(func, arg) =>
      Apply(func.subst(from, replacement), arg.subst(from, replacement))

    case Trait(impl, constraint) =>
      Trait(impl.subst(from, replacement), constraint.subst(from, replacement))

    case Diff(lhs, rhs) =>
      Diff(lhs.subst(from, replacement), rhs.subst(from, replacement))
  }

  infix def unify(that: Type)(using env: Environment): Boolean = {
    val normThis = this.normalize
    val normThat = that.normalize

    (normThis, normThat) match {
      case (Var(name1), Var(name2)) =>
        name1 == name2 || env.typeVars.get(name1).exists(_.unify(normThat)) || env.typeVars.get(name2).exists(normThis.unify)

      case (Primitive(ty1), Primitive(ty2)) =>
        ty1 == ty2

      case (Forall(dom1, param1, cod1), Forall(dom2, param2, cod2)) =>
        dom1.unify(dom2) && {
          (param1, param2) match {
            case (Some(p1), Some(p2)) =>
              // Compare codomains with fresh variable to avoid capture
              val freshVar = Var(env.freshTypeName)
              cod1.subst(p1, freshVar).unify(cod2.subst(p2, freshVar))
            case (None, None) =>
              cod1.unify(cod2)
            case _ => false
          }
        }

      case (Intersection(l1, r1), Intersection(l2, r2)) =>
        (l1.unify(l2) && r1.unify(r2)) || (l1.unify(r2) && r1.unify(l2))

      case (Union(l1, r1), Union(l2, r2)) =>
        (l1.unify(l2) && r1.unify(r2)) || (l1.unify(r2) && r1.unify(l2))

      case (Record(fields1), Record(fields2)) =>
        fields1.keySet == fields2.keySet &&
          fields1.forall { case (label, ty1) =>
            ty1.unify(fields2(label))
          }

      case (Ref(ty1), Ref(ty2)) =>
        ty1.unify(ty2)

      case (Apply(func1, arg1), Apply(func2, arg2)) =>
        func1.unify(func2) && arg1.unify(arg2)

      case (Trait(impl1, constr1), Trait(impl2, constr2)) =>
        impl1.unify(impl2) && constr1.unify(constr2)

      case (Diff(l1, r1), Diff(l2, r2)) =>
        l1.unify(l2) && r1.unify(r2)

      case (Intersection(l, r), other) =>
        l.unify(other) && r.unify(other)

      case (other, Intersection(l, r)) =>
        other.unify(l) && other.unify(r)

      case (Union(l, r), other) =>
        l.unify(other) || r.unify(other)

      case (other, Union(l, r)) =>
        other.unify(l) || other.unify(r)

      case _ => false
    }
  }

  // subtype relation
  infix def <:<(that: Type)(using env: Environment): Boolean = {
    val normThis = this.normalize
    val normThat = that.normalize

    if normThis == normThat then return true

    (normThis, normThat) match {
      case (_, Primitive(LiteralType.TopType)) => true
      case (Primitive(LiteralType.BottomType), _) => true

      case (Var(name), _) =>
        env.typeVars.get(name).exists(_.unify(normThat)) || normThis.unify(normThat)

      case (_, Var(name)) =>
        env.typeVars.get(name).exists(normThis.unify) || normThis.unify(normThat)

      case (Forall(dom1, param1, cod1), Forall(dom2, param2, cod2)) =>
        dom2 <:< dom1 && {
          (param1, param2) match {
            case (Some(p1), Some(p2)) =>
              // Contravariant in domain, covariant in codomain
              val freshVar = Var(env.freshTypeName)
              cod1.subst(p1, freshVar) <:< cod2.subst(p2, freshVar)
            case (None, None) =>
              cod1 <:< cod2
            case _ => false
          }
        }

      case (Intersection(l1, r1), _) =>
        l1 <:< normThat && r1 <:< normThat

      case (_, Union(l2, r2)) =>
        normThis <:< l2 || normThis <:< r2

      case (Union(l1, r1), _) =>
        l1 <:< normThat && r1 <:< normThat

      case (_, Intersection(l2, r2)) =>
        normThis <:< l2 && normThis <:< r2

      case (Record(fields1), Record(fields2)) =>
        fields2.keySet.subsetOf(fields1.keySet) &&
          fields2.forall { case (label, ty2) =>
            fields1(label) <:< ty2
          }

      case (Ref(ty1), Ref(ty2)) =>
        ty1.unify(ty2) // Ref types are invariant

      case (Apply(func1, arg1), Apply(func2, arg2)) =>
        func1.unify(func2) && arg1.unify(arg2)

      case (Trait(impl1, constr1), Trait(impl2, constr2)) =>
        impl1.unify(impl2) && constr1 <:< constr2

      case (Diff(l1, r1), Diff(l2, r2)) =>
        l1.unify(l2) && r1.unify(r2)

      case (left, right) =>
        left.unify(right)
    }
  }

  def normalize(using env: Environment): Type = this match {
    case Var(name) =>
      env.typeVars.get(name) match {
        case Some(resolvedType) => resolvedType.normalize
        case None => this
      }

    case Primitive(ty) => this

    case Forall(domain, paramName, codomain) =>
      paramName match {
        case Some(param) =>
          val normalizedDomain = domain.normalize
          val normalizedCodomain = env.withTypeVar(param, Var(param)) { innerEnv =>
            codomain.normalize(using innerEnv)
          }
          Forall(normalizedDomain, paramName, normalizedCodomain)
        case None =>
          Forall(domain.normalize, None, codomain.normalize)
      }

    case Intersection(lhs, rhs) =>
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

    case Union(lhs, rhs) =>
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

    case Record(fields) =>
      val normalizedFields = fields.map { case (label, fieldType) =>
        label -> fieldType.normalize
      }
      Record(normalizedFields)

    case Ref(ty) =>
      Ref(ty.normalize)

    case Apply(func, arg) =>
      val normalizedFunc = func.normalize
      val normalizedArg = arg.normalize
      normalizedFunc match {
        case Forall(domain, Some(param), codomain) =>
          // Beta reduction: (∀x: A. B) T → B[T/x]
          codomain.subst(param, normalizedArg).normalize
        case _ =>
          Apply(normalizedFunc, normalizedArg)
      }

    case Trait(impl, constraint) =>
      Trait(impl.normalize, constraint.normalize)

    case Diff(lhs, rhs) =>
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
