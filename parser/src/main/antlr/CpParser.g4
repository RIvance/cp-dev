parser grammar CpParser;

options {
    tokenVocab = CpLexer;
}

program
    :   (imports+=importing Semicolon)* (definitions+=definition Semicolon)* main EOF
    ;

header
    :  (imports+=importing Semicolon)* (definitions+=headerDefinition Semicolon)* EOF
    ;

// For REPL

singletonDef
    :   definition Semicolon? EOF
    ;

singletonExpr
    :   expression EOF
    ;

singletonType
    :   type EOF
    ;

singletonStmt
    :   stmt Semicolon? EOF
    ;

main
    :   expr=typedExpr  # mainExpr
    |   Main Assign expr=typedExpr Semicolon  # mainAssignExpr
    |   Main BraceOpen statements=stmt* BraceClose Semicolon?  # mainBlock
    ;

module
    :   (definitions+=definition Semicolon)* EOF
    ;

importing
    :   Import moduleTree
    ;

moduleTree
    :   Asterisk  # moduleTreeWildcard
    |   name=Identifier  # moduleTreeLeaf
    |   parent=Identifier PathSep child=moduleTree  # moduleTreeNode
    |   parent=Identifier PathSep BraceOpen children+=moduleTree (Comma children+=moduleTree)* Comma? BraceClose  # moduleTreeBranch
    ;

definition
    :   def=interfaceDef        # interfaceDefinition
    |   def=typeDef             # typeDefinition
    |   def=termDef             # termDefinition
    |   def=submoduleDef        # submoduleDefinition
    ;

headerDefinition
    :   def=interfaceDef        # interfaceHeaderDefinition
    |   def=typeDef             # typeHeaderDefinition
    |   def=externDef           # termHeaderDefinition
    ;

externDef
    :   Extern symbol=Identifier ':' ty=type
    ;

interfaceDef
    :   Interface name=Identifier (Less sorts+=Identifier (Comma sorts+=Identifier)* Greater)? params=typeParamList?
            (Where constraints+=constraint (Comma constraints+=constraint)*)?
            (Extends extends=typeWithSort)? body=recordType
    ;

typeDef
    :   Type name=Identifier (Less sorts+=Identifier (Comma sorts+=Identifier)* Greater)? params=typeParamList?
            (Where constraints+=constraint (Comma constraints+=constraint)*)?
            Assign body=type
    ;

termDef
    :   Def? name=Identifier typeParams=typeParamList? params+=termParamGroup* (Colon ty=type)?
            (Where constraints+=constraint (Comma constraints+=constraint)*)?
            Assign body=typedExpr
    ;

submoduleDef
    :   Module name=Identifier BraceOpen module BraceClose
    ;

// `where T * U` means T is disjoint from U
constraint
    :   target=Identifier Asterisk disjointness=type  # constraintDisjoint
    |   target=Identifier Subtype supertype=type      # constraintSubtype
    |   target=Identifier Supertype subtype=type      # constraintSupertype
    ;

type
    :   <assoc=left> lhs=type Intersect rhs=type            # typeIntersect
    |   <assoc=left> lhs=type Union rhs=type                # typeUnion
    |   <assoc=left> lhs=type Backslash rhs=type            # typeDifference
    |   <assoc=right> domain=type Arrow codomain=type       # typeArrow
    |   ForAll typeParamList Dot codomain=type              # typeForAll
    |   Fix name=Identifier Dot type                        # typeFix
    |   TraitType Less inType=type (FatArrow outType=type)? Greater        # typeTrait
    |   RefType typeWithSort                                # typeRef
    |   Out sortName=Identifier                             # typeOut
    |   ty=typeWithSort BracketOpen args+=typeWithSort (Comma args+=typeWithSort)* BracketClose     # typeApp
    |   ty=typeWithSort args+=typeWithSort*                 # typeSpine
    ;

typeWithSort
    :   typeLiteral (Less sorts+=sort (Comma sorts+=sort)* Greater)?
    ;

typeLiteral
    :   name=Identifier                       # typeIdent
    |   recordType                          # typeRecord
    |   BracketOpen ty=type BracketClose       # typeArray
    |   ParenOpen ty=type ParenClose           # typeParen
    ;

recordType
    :   BraceOpen (fields+=recordTypeField Semicolon)* fields+=recordTypeField? BraceClose
    ;

recordTypeField
    :   ident=Identifier optional=Question? Colon ty=type                            # recordTypeFieldPlain
    |   ident=Identifier ParenOpen ((Identifier Colon)? paramTypes+=type) (Comma (Identifier Colon)? paramTypes+=type)* Comma? ParenClose Colon returnType=type    # recordTypeFieldProto
    ;

typedExpr
    :   expression (Colon type)?
    ;

expression
    :   appExpr                                                     # expressionApp
    |   compExpr                                                    # expressionComp
    |   op=(Minus | Sqrt | Deref) expression                        # expressionUnary
    |   <assoc=left> arr=expression Index index=expression            # expressionIndex
    |   <assoc=left> lhs=expression op=(Asterisk | Slash | Modulo) rhs=expression   # expressionMulDiv
    |   <assoc=left> lhs=expression op=(Plus | Minus) rhs=expression                # expressionAddSub
    |   <assoc=left> lhs=expression Append rhs=expression                           # expressionAppend
    |   lhs=expression op=(Less | Greater | LessEqual | GreaterEqual | Equal | NotEqual) rhs=expression # expressionComparison
    |   <assoc=right> lhs=expression And rhs=expression             # expressionAnd
    |   <assoc=right> lhs=expression Or rhs=expression              # expressionOr
    |   <assoc=left> lhs=expression Forward rhs=expression          # expressionForward
    |   <assoc=left> lhs=expression op=(Merge | LeftistMerge | RightistMerge | BackslashMinus) rhs=expression # expressionMerge
    |   ref=expression Walrus value=expression        # expressionRefAssign
    |   lhs=expression RightShift rhs=expression           # expressionSeq
    |   expression With BraceOpen (fields+=fieldAssign Semicolon)* fields+=fieldAssign? BraceClose # expressionRecordUpdate
    ;

appExpr
    :   dotExpr (args+=spineArgGroup)*
    ;

compExpr
    :
//    |   Identifier args+=spineArgGroup*                         # compCtorApp
//    |   compExpr args+=spineArgGroup*                           # compExprApp
       (Fun | Backslash | Lambda)? termParams+=termParamGroup+ Arrow expr=typedExpr # compExprLambda
    |   SlashBackslash typeParams=typeParamList Dot expr=typedExpr # compExprTypeLambda
    |   Let name=Identifier typeParams=typeParamList? params+=termParamGroup* Assign value=typedExpr (Colon ty=type)? In body=typedExpr  # compExprLetIn
    |   LetRec name=Identifier typeParams=typeParamList? params+=termParamGroup* Colon ty=type Assign value=typedExpr In body=typedExpr  # compExprLetRec
    |   Open expr=typedExpr In body=typedExpr # compExprOpenIn
    |   If condition=typedExpr Then thenBranch=typedExpr Else elseBranch=typedExpr # compExprIf
    |   Match typedExpr BraceOpen (caseClause Semicolon)* caseClause Semicolon BraceClose # compExprMatch
    |   Trait selfAnno? (Implements implType=type)? (Inherits inheritance=expression)? FatArrow expr=expression # compExprTrait
    |   New expr=expression # compExprNewTrait
    |   Fix name=Identifier Colon ty=type Dot expr=expression # compExprFixpoint
    |   Fold typeArgGroup dotExpr # compExprFold
    |   Unfold typeArgGroup dotExpr # compExprUnfold
    |   Ref dotExpr # compExprRef
    ;

spineArgGroup
    :   typeArgGroup        # spineArgGroupType
    |   exprArgGroup        # spineArgGroupTerm
    ;

exprArgGroup
    :   expr=atomicExpr                                                     # exprArgGroupSingle
    |   ParenOpen exprs+=expression (Comma exprs+=expression)* ParenClose   # exprArgGroupMultiple
    ;

stmt
    :   expr=typedExpr      # stmtExpr
        // let a: T = e
    |   Let name=Identifier typeParams=typeParamList? params+=termParamGroup* (Colon ty=type)? Assign value=typedExpr   # stmtLet
        // letrec f (x: T1) ... (y: Tn): T = e
    |   LetRec name=Identifier typeParams=typeParamList? params+=termParamGroup* Colon ty=type Assign value=typedExpr  # stmtLetRec
        // let (x, y, z) = e
    |   Let ParenOpen names+=Identifier (Comma names+=Identifier)* ParenClose Assign value=typedExpr     # stmtLetTuple
        // let { l1 = e1; l2 = e2; ... } = e
    |   Let BraceOpen fields+=recordFieldAlias (Comma fields+=recordFieldAlias)* BraceClose Assign value=typedExpr     # stmtLetRecord
        // r := e
    |   ref=atomicExpr Walrus value=typedExpr    # stmtRefAssign
    ;

recordFieldAlias
    :   name=Identifier Assign alias=Identifier
    ;

typeParamList
    :   BracketOpen params+=boundedTypeParam (Comma params+=boundedTypeParam)* BracketClose     # typeParamListBracket
    |   params+=typeParam+                                                                      # typeParamListPlain
    ;

renameExpr
    :   expr=dotExpr (BracketOpen oldName=Identifier LeftArrow newName=Identifier BracketClose)?
    ;

matchExpr
    :   Match expr=typedExpr BraceOpen (cases+=caseClause Semicolon)* cases+=caseClause Semicolon BraceClose
    ;

caseClause
    :   Case pattern FatArrow expr=typedExpr
    ;

pattern
    :   Underscore                          # patternWildcard
    |   variable=Identifier                 # patternVar
    |   IntLit                              # patternInt
    |   FloatLit                            # patternFloat
    |   StringLit                           # patternString
    |   BoolLit                             # patternBool
    |   Unit                                # patternUnit
    |   Dollar Identifier params+=termParamGroup*                           # patternCtor
    |   ParenOpen patterns+=pattern (Comma patterns+=pattern)+ ParenClose   # patternTuple
    |   BraceOpen fieldPatterns+=recordPatternField (Semicolon fieldPatterns+=recordPatternField)* BraceClose   # patternRecord
    ;

recordPatternField
    :   name=Identifier Assign pattern   # recordPatternFieldMatching
    |   name=Identifier                  # recordPatternFieldBinding
    |   Underscore                       # recordPatternFieldWildcard
    ;

dotExpr
    :   expr=atomicExpr (Dot path+=Identifier)*
    ;

atomicExpr
    :   Identifier                      # atomicExprVar
    |   IntLit                          # atomicExprInt
    |   FloatLit                        # atomicExprFloat
    |   StringLit                       # atomicExprString
    |   Unit                            # atomicExprUnit
    |   BoolLit                         # atomicExprBool
    |   array                           # atomicExprArray
    |   '(' expression ')'              # atomicExprNested
    |   ident=Identifier ParenOpen (args+=typedExpr (Comma args+=typedExpr)*)? ParenClose    # atomicExprApp
//    |   expr=atomicExpr ParenOpen (args+=typedExpr (Comma args+=typedExpr)*)? ParenClose    # atomicExprApp
    |   expr=atomicExpr BracketOpen args+=type (Comma args+=type)* BracketClose             # atomicExprTypeApp
    |   record                          # atomicExprRecord
    |   recordUpdate                    # atomicExprRecordUpdate
    |   Dollar Identifier               # atomicExprCtor
    |   ParenOpen typedExpr ParenClose  # atomicExprParen
    |   BraceOpen (params+=termParamGroup+ Arrow)? (stmt Semicolon)* typedExpr? BraceClose  # atomicExprBlock
    |   ParenOpen ParenOpen (elements+=typedExpr Comma)+ elements+=typedExpr? ParenClose ParenClose # atomicExprTuple
    ;

array
    :   BracketOpen (elements+=typedExpr Semicolon)* elements+=typedExpr? BracketClose
    ;

record
    :   BraceOpen (entities+=recordEntity Semicolon)* entities+=recordEntity? BraceClose
    ;

recordEntity
    :   (Impl | Override)? selfAnno? name=Identifier params+=termParamGroup* Assign value=typedExpr  # recordField
    |   Override? (selfAnno At)? fieldPattern=recordEntityPattern
          Dot method=Identifier methodParams+=termParamGroup* Assign value=typedExpr # recordMethodPattern
    // Wildcard pattern e.g. `trait implements ExpSig<Eval> => { _.eval = 0 };`
    |   (Underscore | selfAnno) Dot name=Identifier params+=termParamGroup* Assign value=typedExpr # recordDefaultPattern
    ;

recordEntityPattern
    :   ParenOpen name=Identifier paramGroups+=termParamGroup* ParenClose
    |   name=Identifier ParenOpen (atomGroups+=termParamAtomGroup (Comma atomGroups+=termParamAtomGroup)*) ParenClose
    ;

recordUpdate
    :   BraceOpen recordExpr=typedExpr With (fields+=fieldAssign Semicolon)* fields+=fieldAssign? BraceClose
    ;

fieldAssign
    :   field=Identifier Assign value=typedExpr
    ;

typeArgGroup
    :   At atomicType=typeWithSort                                      # typeArgGroupSingle
    |   BracketOpen (types+=type (Comma types+=type)*) BracketClose     # typeArgGroupMultiple
    ;

typeParam
    :   Identifier
    |   ParenOpen Identifier Asterisk type ParenClose
    ;

boundedTypeParam
    :   (lowerBound=type Subtype)? Identifier (Subtype upperBound=type)?
    ;

termParamGroup
    :   name=Identifier             # termParamSingleIdent
    |   Underscore                  # termParamSingleWildcard
    |   ParenOpen (groups+=termParamAtomGroup (Comma groups+=termParamAtomGroup)*) ParenClose # termParamMultiple
    |   wildcardRecord              # termParamRecordWildcard
    ;

termParamAtomGroup
    :   params+=termParamIdent+ Colon type
    ;

termParamIdent
    :   Identifier
    |   Underscore
    ;

wildcardRecord
    :   BraceOpen (fields+=fieldAssign Semicolon)* DotDot BraceClose
    ;

selfAnno
    :   BracketOpen name=Identifier (Colon type)? BracketClose
    ;

sort
    :   inType=type (FatArrow outType=type)?
    ;

recordArgField
    :   Identifier termParamGroup* Assign typedExpr
    ;
