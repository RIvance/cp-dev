package cp.parser

import cp.ast.CpParser.*
import cp.ast.CpParserBaseVisitor
import cp.core.*
import cp.syntax.*
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.*

class Visitor extends CpParserBaseVisitor[
  RawModule | RawProgram | Definition | Statement | ExprTerm | ExprType
] {

  extension (self: ExprType) {
    def withSpan(ctx: ParserRuleContext): ExprType = self.withSpan(ctx.span)
  }

  extension (self: ExprTerm) {
    def withSpan(ctx: ParserRuleContext): ExprTerm = self.withSpan(ctx.span)

    def foldLambda(params: Seq[(String, Option[ExprType])]): ExprTerm = params.foldRight(self) {
      case ((paramIdent, paramType), acc) => ExprTerm.Lambda(paramIdent, paramType, acc)
    }

    def foldTypeLambda(params: Seq[String]): ExprTerm = params.foldRight(self) {
      case (paramIdent, acc) => ExprTerm.TypeLambda(paramIdent, acc)
    }
  }

  override def visitModule(ctx: ModuleContext): RawModule = {
    val definitions = ctx.definitions.asScala.map(visitDefinition).toList
    RawModule(definitions)
  }

  override def visitProgram(ctx: ProgramContext): RawProgram = {
    val defaultModule = RawModule(ctx.definitions.asScala.map(visitDefinition).toList)
    val mainExpr: ExprTerm = ctx.main match {
      case mainCtx: MainExprContext => mainCtx.expr.visit
      case mainCtx: MainAssignExprContext => mainCtx.expr.visit
      case mainCtx: MainBlockContext => ??? // TODO: main block
    }
    RawProgram(defaultModule, mainExpr)
  }

  def visitDefinition(ctx: DefinitionContext): Definition = ctx match {
    case ctx: InterfaceDefinitionContext => {
      // interface EqPoint { x: Int; y: Int; eq: EqPoint → Bool };
      // -- is equivalent to:
      // type EqPoint = fix EqPoint. { x: Int; y: Int; eq: EqPoint → Bool };
      val name = ctx.`def`.name.getText
      val sorts = ctx.`def`.sorts.asScala
      val params = ctx.`def`.params.visit
      val body = visitRecordType(ctx.`def`.body)
      ??? // TODO
    }
    case ctx: TypeDefinitionContext => {
      val name = ctx.`def`.name.getText
      val typeParams = ctx.`def`.params.visit
      val ty: ExprType = ctx.`def`.body.visit
      // Wrap type in forall quantifiers if there are type parameters
      val fullType: ExprType = typeParams.foldRight(ty) { 
        (param, acc: ExprType) => ExprType.Forall(param, acc)
      }.withSpan(ctx)
      Definition.TypeDef(name, fullType).withSpan(ctx.span)
    }
    case ctx: TermDefinitionContext => {
      val name = ctx.`def`.name.getText
      val typeParams = Option(ctx.`def`.typeParams).map(_.visit).getOrElse(List.empty)
      val params = ctx.`def`.params.asScala.flatMap(_.visit).toList
      val value = ctx.`def`.body.visit
      // Handle function with parameters
      val func = if params.isEmpty then value else value.foldLambda(params)
      // Apply type parameters if any
      val typedFunc = if typeParams.isEmpty then func else func.foldTypeLambda(typeParams)
      Definition.TermDef(name, typedFunc).withSpan(ctx.span)
    }
    case ctx: SubmoduleDefinitionContext => {
      val name = ctx.`def`.name.getText
      val module = visitModule(ctx.`def`.module)
      Definition.SubmodDef(name, module).withSpan(ctx.span)
    }
  }
  
  extension (ctx: ExpressionContext) {
    def visit: ExprTerm = ctx match {
      case ctx: ExpressionComplexContext => visitExpressionComplex(ctx)
      case ctx: ExpressionUnaryContext => visitExpressionUnary(ctx)
      case ctx: ExpressionIndexContext => visitExpressionIndex(ctx)
      case ctx: ExpressionMulDivContext => visitExpressionMulDiv(ctx)
      case ctx: ExpressionAddSubContext => visitExpressionAddSub(ctx)
      case ctx: ExpressionAppendContext => visitExpressionAppend(ctx)
      case ctx: ExpressionComparisonContext => visitExpressionComparison(ctx)
      case ctx: ExpressionAndContext => visitExpressionAnd(ctx)
      case ctx: ExpressionOrContext => visitExpressionOr(ctx)
      case ctx: ExpressionForwardContext => visitExpressionForward(ctx)
      case ctx: ExpressionMergeContext => visitExpressionMerge(ctx)
      case ctx: ExpressionRefAssignContext => visitExpressionRefAssign(ctx)
      case ctx: ExpressionSeqContext => visitExpressionSeq(ctx)
      case ctx: ExpressionRecordUpdateContext => visitExpressionRecordUpdate(ctx)
    }
  }

  override def visitExpressionComplex(ctx: ExpressionComplexContext): ExprTerm = {
    ctx.compExpr.visit.withSpan(ctx)
  }

  override def visitExpressionUnary(ctx: ExpressionUnaryContext): ExprTerm = {
    ExprTerm.UnaryOp(ctx.op.getText, ctx.expression.visit).withSpan(ctx)
  }

  override def visitExpressionIndex(ctx: ExpressionIndexContext): ExprTerm = {
    ExprTerm.Index(ctx.arr.visit, ctx.index.visit).withSpan(ctx)
  }

  override def visitExpressionMulDiv(ctx: ExpressionMulDivContext): ExprTerm = {
    ExprTerm.BinaryOp(ctx.op.getText, ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionAddSub(ctx: ExpressionAddSubContext): ExprTerm = {
    ExprTerm.BinaryOp(ctx.op.getText, ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionAppend(ctx: ExpressionAppendContext): ExprTerm = {
    ExprTerm.BinaryOp("++", ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionComparison(ctx: ExpressionComparisonContext): ExprTerm = {
    ExprTerm.BinaryOp(ctx.op.getText, ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionAnd(ctx: ExpressionAndContext): ExprTerm = {
    ExprTerm.BinaryOp("&", ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionOr(ctx: ExpressionOrContext): ExprTerm = {
    ExprTerm.BinaryOp("|", ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionForward(ctx: ExpressionForwardContext): ExprTerm = {
    ExprTerm.BinaryOp("^", ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
    
  }

  override def visitExpressionMerge(ctx: ExpressionMergeContext): ExprTerm = {
    ExprTerm.Merge(ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionRefAssign(ctx: ExpressionRefAssignContext): ExprTerm = {
    ExprTerm.Apply(ExprTerm.Var("#refAssign"), List(ctx.ref.visit, ctx.value.visit)).withSpan(ctx)
  }

  override def visitExpressionSeq(ctx: ExpressionSeqContext): ExprTerm = {
    ExprTerm.BinaryOp("<<", ctx.lhs.visit, ctx.rhs.visit).withSpan(ctx)
  }

  override def visitExpressionRecordUpdate(ctx: ExpressionRecordUpdateContext): ExprTerm = {
    val updatedFields = ctx.fields.asScala.map { fieldAssignment =>
      fieldAssignment.field.getText -> fieldAssignment.value.visit
    }.toMap
    ExprTerm.Update(ctx.expression.visit, updatedFields).withSpan(ctx)
  }
  
  extension (ctx: TypedExprContext) {
    def visit: ExprTerm = ExprTerm.Typed(ctx.expression.visit, ctx.`type`.visit)
  }

  extension (ctx: CompExprContext) {
    def visit: ExprTerm = ctx match {
      case ctx: CompCtorAppContext => visitCompCtorApp(ctx)
      case ctx: CompExprAppContext => visitCompExprApp(ctx)
      case ctx: CompExprLambdaContext => visitCompExprLambda(ctx)
      case ctx: CompExprTypeLambdaContext => visitCompExprTypeLambda(ctx)
      case ctx: CompExprLetInContext => visitCompExprLetIn(ctx)
      case ctx: CompExprLetRecContext => visitCompExprLetRec(ctx)
      case ctx: CompExprOpenInContext => visitCompExprOpenIn(ctx)
      case ctx: CompExprIfContext => visitCompExprIf(ctx)
      case ctx: CompExprMatchContext => visitCompExprMatch(ctx)
      case ctx: CompExprTraitContext => visitCompExprTrait(ctx)
      case ctx: CompExprNewTraitContext => visitCompExprNewTrait(ctx)
      case ctx: CompExprFixpointContext => visitCompExprFixpoint(ctx)
      case ctx: CompExprFoldContext => visitCompExprFold(ctx)
      case ctx: CompExprUnfoldContext => visitCompExprUnfold(ctx)
      case ctx: CompExprRefContext => visitCompExprRef(ctx)
    }
  }

  override def visitCompCtorApp(ctx: CompCtorAppContext): ExprTerm = {
    val ctor = ctx.ctorName.getText
    val args = ctx.args.asScala.map(_.visit).toList
    ExprTerm.Apply(ExprTerm.Var(ctor), args).withSpan(ctx)
  }

  override def visitCompExprApp(ctx: CompExprAppContext): ExprTerm = {
    val func = ctx.excludeExpr.visit
    val args = ctx.args.asScala.map(_.visit).toList
    ExprTerm.Apply(func, args).withSpan(ctx)
  }

  override def visitCompExprLambda(ctx: CompExprLambdaContext): ExprTerm = {
    val params = ctx.termParams.asScala.flatMap(_.visit)
    val body = ctx.expr.visit
    // Handle multiple parameters by nesting lambdas
    params.foldRight(body) { case ((paramIdent, paramType), acc: ExprTerm) =>
      ExprTerm.Lambda(paramIdent, paramType, acc)
    }.withSpan(ctx)
  }

  override def visitCompExprTypeLambda(ctx: CompExprTypeLambdaContext): ExprTerm = {
    val typeParams = ctx.typeParams.visit
    val body = ctx.expr.visit
    // Nest type lambdas for each type parameter
    typeParams.foldRight(body) { (param, acc: ExprTerm) =>
      ExprTerm.TypeLambda(param, acc)
    }.withSpan(ctx)
  }

  override def visitCompExprLetIn(ctx: CompExprLetInContext): ExprTerm = {
    val name = ctx.name.getText
    val typeParams = Option(ctx.typeParams).map(_.visit).getOrElse(Nil)
    val params = ctx.params.asScala.flatMap(_.visit).toList
    val value = ctx.value.visit
    val body = ctx.body.visit
    // Handle function with parameters
    val func = if (params.isEmpty) value else value.foldLambda(params)
    // Apply type parameters if any
    val typedFunc = if (typeParams.isEmpty) func else func.foldTypeLambda(typeParams)
    ExprTerm.LetIn(name, typedFunc, body).withSpan(ctx)
  }

  override def visitCompExprLetRec(ctx: CompExprLetRecContext): ExprTerm = {
    val name = ctx.name.visit
    val typeParams = Option(ctx.typeParams).map(_.visit).getOrElse(Nil)
    val params = ctx.params.asScala.flatMap(_.visit).toList
    val returnType = ctx.ty.visit
    val value = ctx.value.visit
    val body = ctx.body.visit
    // Create fixpoint for recursive function
    val funcBody = value.foldLambda(params)
    val fixpoint = ExprTerm.Fixpoint(name, Some(returnType), funcBody)
    // Apply type parameters if any
    val typedFixpoint = if (typeParams.isEmpty) fixpoint else fixpoint.foldTypeLambda(typeParams)
    ExprTerm.LetIn(name, typedFixpoint, body).withSpan(ctx)
  }

  override def visitCompExprOpenIn(ctx: CompExprOpenInContext): ExprTerm = {
    ExprTerm.OpenIn(ctx.expr.visit, ctx.body.visit).withSpan(ctx)
  }

  override def visitCompExprIf(ctx: CompExprIfContext): ExprTerm = {
    ExprTerm.IfThenElse(ctx.condition.visit, ctx.thenBranch.visit, ctx.elseBranch.visit).withSpan(ctx)
  }

  override def visitCompExprMatch(ctx: CompExprMatchContext): ExprTerm = {
    val scrutinee = ctx.typedExpr.visit
    val cases = ctx.caseClause.asScala.map(visitCaseClause).toList
    ??? // TODO: Implement pattern matching
  }

  override def visitCompExprTrait(ctx: CompExprTraitContext): ExprTerm = {
    val selfAnno = Option(ctx.selfAnno).map(_.visit)
    val implements = Option(ctx.implType).map(_.visit)
    val inherits = Option(ctx.inheritance).map(_.visit)
    val body = ctx.expr.visit

    ExprTerm.Trait(
      selfAnno.getOrElse(SelfAnnotation[ExprType]("self", None)), 
      implements, inherits, body
    ).withSpan(ctx)
  }
  
  extension (ctx: SelfAnnotation[ExprType]) def visit: SelfAnnotation[ExprType] = SelfAnnotation[ExprType](ctx.name, ctx.ty)

  override def visitCompExprNewTrait(ctx: CompExprNewTraitContext): ExprTerm = {
    ExprTerm.New(ctx.expr.visit).withSpan(ctx)
  }

  override def visitCompExprFixpoint(ctx: CompExprFixpointContext): ExprTerm = {
    ExprTerm.Fixpoint(ctx.name.visit, Some(ctx.ty.visit), ctx.expr.visit).withSpan(ctx)
  }

  override def visitCompExprFold(ctx: CompExprFoldContext): ExprTerm = {
    ExprTerm.FoldFixpoint(ctx.typeArg.visit, ctx.dotExpr.visit).withSpan(ctx)
  }

  override def visitCompExprUnfold(ctx: CompExprUnfoldContext): ExprTerm = {
    ExprTerm.UnfoldFixpoint(ctx.typeArg.visit, ctx.dotExpr.visit).withSpan(ctx)
  }

  override def visitCompExprRef(ctx: CompExprRefContext): ExprTerm = {
    ExprTerm.Apply(ExprTerm.Var("#refInit"), List(ctx.dotExpr.visit))
  }

  extension (ctx: ExcludeExprContext) def visit: ExprTerm = {
    (Option(ctx.excludeType), Option(ctx.removalField)) match {
      case (Some(excludeType), _) =>
        ExprTerm.Exclude(ctx.expr.visit, excludeType.visit).withSpan(ctx)
      case (_, Some(removalField)) =>
        ExprTerm.Remove(ctx.expr.visit, Set(removalField.getText)).withSpan(ctx)
      case _ =>
        ctx.expr.visit.withSpan(ctx)
    }
  }

  extension (ctx: DotExprContext) def visit: ExprTerm = {
    ctx.path.asScala.foldLeft(ctx.expr.visit) {
      (current, fieldLabel) => ExprTerm.Projection(current, fieldLabel.getText)
    }.withSpan(ctx)
  }

  extension (ctx: RenameExprContext) def visit: ExprTerm = {
    Option(ctx.oldName) match {
      case Some(oldName) =>
        ExprTerm.Rename(ctx.expr.visit, Map(oldName.getText -> ctx.newName.getText)).withSpan(ctx)
      case None =>
        ctx.expr.visit.withSpan(ctx)
    }
  }

  extension (ctx: TermParamGroupContext) {
    def visit: Seq[(String, Option[ExprType])] = ctx match {
      case single: TermParamSingleIdentContext =>
        List((single.getText, None))
      case wildcard: TermParamSingleWildcardContext =>
        List(("_", None))
      case multiple: TermParamMultipleContext =>
        multiple.groups.asScala.flatMap(_.visit).toSeq
      // TODO: what's this? Is that a syntax sugar for
      //  `let r: { l1 = e1; ... } = open r in ...` ?
      case wild: TermParamRecordWildcardContext => ???
    }
  }

  extension (ctx: TermParamAtomGroupContext) {
    def visit: Seq[(String, Option[ExprType])] = {
      val ty = ctx.`type`.visit
      ctx.params.asScala.map(_.getText -> Some(ty)).toSeq
    }
  }

  extension (ctx: TypeParamListContext) {
    def visit: List[String] = ctx match {
      case bracket: TypeParamListBracketContext =>
        bracket.params.asScala.map(_.getText).toList
      case plain: TypeParamListPlainContext =>
        plain.params.asScala.map(_.getText).toList
    }
  }

  extension (ctx: SelfAnnoContext) {
    def visit: SelfAnnotation[ExprType] = SelfAnnotation[ExprType](ctx.name.getText, Option(ctx.`type`).map(_.visit))
  }

  extension (ctx: TermNameDeclContext) {
    def visit: String = ctx.getText
  }

  extension (ctx: TypeArgContext) {
    def visit: ExprType = ctx.typeWithSort.visit
  }

  extension (ctx: SpineArgContext) {
    def visit: ExprTerm = ctx match {
      case term: SpineArgTermContext => term.expr.visit
      case ty: SpineArgTypeContext => ??? // TODO: Type arguments become type applications
    }
  }

  def visitStatement(ctx: StmtContext): Statement = ctx match {

    case exprCtx: StmtExprContext =>
      Statement.Expression(exprCtx.expr.visit)(ctx.span)

    case refAssignCtx: StmtRefAssignContext =>
      Statement.RefAssign(refAssignCtx.ref.visit, refAssignCtx.value.visit)(ctx.span)

    case letCtx: StmtLetContext => {
      val typeParams = Option(letCtx.typeParams).map(_.visit).getOrElse(Nil)
      val params = letCtx.params.asScala.flatMap(_.visit).toList
      val value = letCtx.value.visit
      // Handle function with parameters
      val func = if (params.isEmpty) value else value.foldLambda(params)
      // Apply type parameters if any
      val typedFunc = if (typeParams.isEmpty) func else func.foldTypeLambda(typeParams)
      Statement.Let(letCtx.name.getText, typedFunc)(ctx.span)
    }

    case letRecCtx: StmtLetRecContext => {
      val name = letRecCtx.name.getText
      val typeParams = Option(letRecCtx.typeParams).map(_.visit).getOrElse(Nil)
      val params = letRecCtx.params.asScala.flatMap(_.visit).toList
      val returnType = letRecCtx.ty.visit
      val value = letRecCtx.value.visit
      // Create fixpoint for recursive function
      val funcBody = value.foldLambda(params)
      val fixpoint = ExprTerm.Fixpoint(name, Some(returnType), funcBody)
      // Apply type parameters if any
      val typedFixpoint = if (typeParams.isEmpty) fixpoint else fixpoint.foldTypeLambda(typeParams)
      Statement.LetRec(name, typedFixpoint, returnType)(ctx.span)
    }

    case letTupleCtx: StmtLetTupleContext => {
      val names = letTupleCtx.names.asScala.map(_.getText).toList
      Statement.LetTupleDestruct(names, letTupleCtx.value.visit)(ctx.span)
    }

    case letRecordCtx: StmtLetRecordContext => {
      val fields = letRecordCtx.fields.asScala.map { field =>
        field.name.getText -> field.alias.getText
      }.toMap
      Statement.LetRecordDestruct(fields, letRecordCtx.value.visit)(ctx.span)
    }
  }

  extension (ctx: StmtContext) {
    def visit: Statement = visitStatement(ctx)
  }

  extension (ctx: AtomicExprContext) {
    def visit: ExprTerm = ctx match {
      case varCtx: AtomicExprVarContext =>
        ExprTerm.Var(varCtx.termName.getText)
      case intCtx: AtomicExprIntContext =>
        ExprTerm.Primitive(Literal.IntValue(intCtx.IntLit.getText.toInt))
      case floatCtx: AtomicExprFloatContext =>
        ExprTerm.Primitive(Literal.FloatValue(floatCtx.FloatLit.getText.toFloat))
      case stringCtx: AtomicExprStringContext =>
        ExprTerm.Primitive(Literal.StringValue(stringCtx.StringLit.getText))
      // case docCtx: AtomicExprDocContext =>
      //   ExprTerm.Document(docCtx.document.visit)
      case unitCtx: AtomicExprUnitContext =>
        ExprTerm.Primitive(Literal.UnitValue)
      case boolCtx: AtomicExprBoolContext =>
        ExprTerm.Primitive(Literal.BoolValue(boolCtx.BoolLit.getText == "true"))
      case arrayCtx: AtomicExprArrayContext => 
        ExprTerm.ArrayLiteral(arrayCtx.array.visitArrayElements)
      case recordCtx: AtomicExprRecordContext => 
        recordCtx.record.visit
      case tupleCtx: AtomicExprTupleContext => 
        tupleCtx.tuple.visit
      case updateCtx: AtomicExprRecordUpdateContext => 
        updateCtx.recordUpdate.visit
      case ctorCtx: AtomicExprCtorContext => 
        ExprTerm.Var(ctorCtx.ctorName.getText)
      case parenCtx: AtomicExprParenContext => 
        parenCtx.typedExpr.visit
        
      case blockCtx: AtomicExprBlockContext => {
        val stmts = blockCtx.stmt.asScala.map(_.visit).toList
        val lastExpr = Option(blockCtx.typedExpr).map(_.visit)
        // Convert statements to nested lets
        stmts.foldRight(lastExpr.getOrElse(ExprTerm.Primitive(Literal.UnitValue))) { 
          (stmt, acc) => stmt match {
            case Statement.RefAssign(reference, value) =>
              ExprTerm.Do(
                ExprTerm.Apply(
                  ExprTerm.Var("#refAssign"), List(reference, value)
                ).withSpan(stmt.span),
                body = acc
              )
            case Statement.Let(name, value) =>
              ExprTerm.LetIn(name, value, acc).withSpan(stmt.span)
            case Statement.LetRec(name, value, _) =>
              ExprTerm.LetRecIn(name, value, acc).withSpan(stmt.span)
            case Statement.LetTupleDestruct(_, _) => ???
            case Statement.LetRecordDestruct(_, _) => ???
            case Statement.Expression(expr) =>
              ExprTerm.Do(expr.withSpan(stmt.span), acc)
          }
        }
      }
      
      case appCtx: AtomicExprAppContext => {
        val func = appCtx.atomicExpr.visit
        val args = appCtx.args.asScala.map(_.visit).toList
        ExprTerm.Apply(func, args)
      }
      
      case typeAppCtx: AtomicExprTypeAppContext => {
        val term = typeAppCtx.atomicExpr.visit
        val typeArgs = typeAppCtx.args.asScala.map(_.visit)
        ExprTerm.TypeApply(term, typeArgs.toList)
      }
    }
  }

  extension (ctx: ArrayContext) {
    def visitArrayElements: List[ExprTerm] = ctx.elements.asScala.map(_.visit).toList
  }

  extension (ctx: RecordContext) def visit: ExprTerm = {
    val fields = ctx.entities.asScala.map {
      case field: RecordFieldContext => field.name.getText -> field.value.visit
      // TODO: Handle other record entity types as needed
      case _ => ("unknown", ExprTerm.unit)
    }.toMap
    ExprTerm.Record(fields)
  }

  extension (ctx: TupleContext) def visit: ExprTerm = {
    ExprTerm.Tuple(ctx.elements.asScala.map(_.visit).toList)
  }

  extension (ctx: RecordUpdateContext) def visit: ExprTerm = {
    val record = ctx.recordExpr.visit
    val updates = ctx.fields.asScala.map { field =>
      field.field.getText -> field.value.visit
    }.toMap
    ExprTerm.Update(record, updates)
  }

  // Type expressions
  override def visitTypeIntersect(ctx: TypeIntersectContext): ExprType = {
    ExprType.Intersection(ctx.lhs.visit, ctx.rhs.visit)
  }

  override def visitTypeUnion(ctx: TypeUnionContext): ExprType = {
    ExprType.Union(ctx.lhs.visit, ctx.rhs.visit)
  }

  override def visitTypeDifference(ctx: TypeDifferenceContext): ExprType = {
    ExprType.Diff(ctx.lhs.visit, ctx.rhs.visit)
  }

  override def visitTypeArrow(ctx: TypeArrowContext): ExprType = {
    ExprType.Arrow(ctx.domain.visit, ctx.codomain.visit)
  }

  override def visitTypeForAll(ctx: TypeForAllContext): ExprType = {
    val typeParams = ctx.typeParamList.visit
    val codomain = ctx.codomain.visit
    // Nest forall quantifiers for each type parameter
    typeParams.foldRight(codomain) { (param, acc) =>
      ExprType.Forall(param, acc)
    }
  }

  override def visitTypeFix(ctx: TypeFixContext): ExprType = {
    ??? // TODO
  }

  override def visitTypeTrait(ctx: TypeTraitContext): ExprType = {
    val inType = ctx.inType.visit
    val outType = Option(ctx.outType).map(_.visit)
    ExprType.Trait(Some(inType), outType.getOrElse(ExprType.Primitive(LiteralType.UnitType)))
  }

  override def visitTypeRef(ctx: TypeRefContext): ExprType = {
    ExprType.Ref(ctx.typeWithSort.visit)
  }

  override def visitTypeApp(ctx: TypeAppContext): ExprType = {
    val func = ctx.ty.visit
    val args = ctx.args.asScala.map(_.visit).toList
    // Apply type arguments sequentially
    args.foldLeft(func) { (currentFunc, arg) =>
      ExprType.Apply(currentFunc, arg)
    }.withSpan(ctx)
  }

  override def visitTypeSpine(ctx: TypeSpineContext): ExprType = {
    val baseType = ctx.typeWithSort.visit
    val args = ctx.args.asScala.map(_.visit).toList
    // Apply spine arguments sequentially
    args.foldLeft(baseType) { (currentType, arg) =>
      ExprType.Apply(currentType, arg)
    }.withSpan(ctx)
  }

  override def visitTypeWithSort(ctx: TypeWithSortContext): ExprType = {
    val baseType = ctx.typeLiteral.visit
    val sorts = Option(ctx.sort).map(_.asScala.map(_.visit).toList).getOrElse(Nil)
    ??? // TODO: Apply sorts to baseType
  }

  override def visitTypeIdent(ctx: TypeIdentContext): ExprType = {
    ctx.name.getText match {
      case "Int" => ExprType.Primitive(LiteralType.IntType)
      case "Float" => ExprType.Primitive(LiteralType.FloatType)
      case "String" => ExprType.Primitive(LiteralType.StringType)
      case "Bool" => ExprType.Primitive(LiteralType.BoolType)
      case "Unit" => ExprType.Primitive(LiteralType.UnitType)
      case ident => ExprType.Var(ident)
    }
  }

  override def visitTypeRecord(ctx: TypeRecordContext): ExprType = visitRecordType(ctx.recordType)

  override def visitRecordType(ctx: RecordTypeContext): ExprType = {
    val fields = ctx.fields.asScala.map { field =>
      field.ident.getText -> field.ty.visit
    }.toMap
    ExprType.Record(fields).withSpan(ctx)
  }

  override def visitTypeArray(ctx: TypeArrayContext): ExprType = {
    ExprType.Array(ctx.ty.visit).withSpan(ctx)
  }

  override def visitTypeParen(ctx: TypeParenContext): ExprType = ctx.ty.visit

  extension (ctx: SortContext) {
    def visit: (ExprType, Option[ExprType]) = {
      val inType = ctx.inType.visit
      val outType = Option(ctx.outType).map(_.visit)
      (inType, outType)
    }
  }

  extension (ctx: TypeContext) {
    def visit: ExprType = ctx match {
      case ctx: TypeIntersectContext => visitTypeIntersect(ctx)
      case ctx: TypeUnionContext => visitTypeUnion(ctx)
      case ctx: TypeDifferenceContext => visitTypeDifference(ctx)
      case ctx: TypeArrowContext => visitTypeArrow(ctx)
      case ctx: TypeForAllContext => visitTypeForAll(ctx)
      case ctx: TypeFixContext => visitTypeFix(ctx)
      case ctx: TypeTraitContext => visitTypeTrait(ctx)
      case ctx: TypeRefContext => visitTypeRef(ctx)
      case ctx: TypeAppContext => visitTypeApp(ctx)
      case ctx: TypeSpineContext => visitTypeSpine(ctx)
    }
  }

  extension (ctx: TypeWithSortContext) {
    def visit: ExprType = visitTypeWithSort(ctx)
  }

  extension (ctx: TypeLiteralContext) {
    def visit: ExprType = ctx match {
      case ctx: TypeIdentContext => visitTypeIdent(ctx)
      case ctx: TypeRecordContext => visitTypeRecord(ctx)
      case ctx: TypeArrayContext => visitTypeArray(ctx)
      case ctx: TypeParenContext => visitTypeParen(ctx)
    }
  }

  extension (ctx: RecordTypeFieldContext) {
    def visit: (String, ExprType) = (ctx.ident.getText, ctx.ty.visit)
  }

  enum Pattern {
    case Wildcard
    case Variable(name: String)
    case Literal(value: Literal)
    case Constructor(name: String, patterns: List[Pattern])
    case Record(fields: Map[String, Pattern])
  }
}

