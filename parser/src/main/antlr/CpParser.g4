parser grammar CpParser;

options {
    tokenVocab = CpLexer;
}

program
    :   (imports+=importing Semicolon)* (definitions+=definition Semicolon)* main EOF
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
    |   name=(LowerId | UpperId)  # moduleTreeLeaf
    |   parent=moduleName PathSep child=moduleTree  # moduleTreeNode
    |   parent=moduleName PathSep BraceOpen children+=moduleTree (Comma children+=moduleTree)* Comma? BraceClose  # moduleTreeBranch
    ;

definition
    :   def=interfaceDef        # interfaceDefinition
    |   def=typeDef             # typeDefinition
    |   def=termDef             # termDefinition
    |   def=submoduleDef        # submoduleDefinition
    ;

interfaceDef
    :   Interface name=typeNameDecl (Less sorts+=typeNameDecl (Comma sorts+=typeNameDecl)* Greater)? params=typeParamList?
            (Where constraints+=constraint (Comma constraints+=constraint)*)?
            (Extends extends=typeWithSort)? body=recordType
    ;

typeDef
    :   Type name=typeNameDecl (Less sorts+=typeNameDecl (Comma sorts+=typeNameDecl)* Greater)? params=typeParamList?
            (Where constraints+=constraint (Comma constraints+=constraint)*)?
            Assign body=type
    ;

termDef
    :   Def? name=termNameDecl typeParams=typeParamList? params+=termParamGroup* (Colon ty=type)?
            (Where constraints+=constraint (Comma constraints+=constraint)*)?
            Assign body=typedExpr
    ;

submoduleDef
    :   Module name=moduleName BraceOpen module BraceClose
    ;

// `where T * U` means T is disjoint from U
constraint
    :   target=typeNameDecl Asterisk disjointness=type  # constraintDisjoint
    |   target=typeNameDecl Subtype supertype=type      # constraintSubtype
    |   target=typeNameDecl Supertype subtype=type      # constraintSupertype
    ;

type
    :   <assoc=left> lhs=type Intersect rhs=type            # typeIntersect
    |   <assoc=left> lhs=type Union rhs=type                # typeUnion
    |   <assoc=left> lhs=type Backslash rhs=type            # typeDifference
    |   <assoc=right> domain=type Arrow codomain=type       # typeArrow
    |   ForAll typeParamList Dot codomain=type              # typeForAll
    |   Fix name=typeNameDecl Dot type                      # typeFix
    |   TraitType Less inType=type (FatArrow outType=type)? Greater        # typeTrait
    |   RefType typeWithSort                                # typeRef
    |   ty=typeWithSort BracketOpen args+=typeWithSort (Comma args+=typeWithSort)* BracketClose     # typeApp
    |   ty=typeWithSort args+=typeWithSort*                 # typeSpine
    ;

typeWithSort
    :   typeLiteral (Less sort (Comma sort)* Greater)?
    ;

typeLiteral
    :   name=typeName                       # typeIdent
    |   recordType                          # typeRecord
    |   BracketOpen ty=type BracketClose       # typeArray
    |   ParenOpen ty=type ParenClose           # typeParen
    ;

recordType
    :   BraceOpen (fields+=recordTypeField Semicolon)* fields+=recordTypeField? BraceClose
    ;

recordTypeField
    :   ident=labelDecl optional=Question? Colon ty=type
    ;

typedExpr
    :   expression (Colon type)?
    ;

expression
    :   compExpr                                                 # expressionComplex
    |   op=(Minus | Sqrt | Deref) expression                     # expressionUnary
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

compExpr
    :   ctorName args+=spineArg*                            # compCtorApp
    |   excludeExpr args+=spineArg*                         # compExprApp
    |   (Fun | Backslash | Lambda) termParams+=termParamGroup+ Arrow expr=typedExpr # compExprLambda
    |   SlashBackslash typeParams=typeParamList Dot expr=typedExpr # compExprTypeLambda
    |   Let name=termNameDecl typeParams=typeParamList? params+=termParamGroup* Assign value=typedExpr (Colon ty=type)? In body=typedExpr  # compExprLetIn
    |   LetRec name=termNameDecl typeParams=typeParamList? params+=termParamGroup* Colon ty=type Assign value=typedExpr In body=typedExpr  # compExprLetRec
    |   Open expr=typedExpr In body=typedExpr # compExprOpenIn
    |   If condition=typedExpr Then thenBranch=typedExpr Else elseBranch=typedExpr # compExprIf
    |   Match typedExpr BraceOpen (caseClause Semicolon)* caseClause Semicolon BraceClose # compExprMatch
    |   Trait selfAnno? (Implements implType=type)? (Inherits inheritance=expression)? FatArrow expr=expression # compExprTrait
    |   New expr=expression # compExprNewTrait
    |   Fix name=termNameDecl Colon ty=type Dot expr=expression # compExprFixpoint
    |   Fold typeArg dotExpr # compExprFold
    |   Unfold typeArg dotExpr # compExprUnfold
    |   Ref dotExpr # compExprRef
    ;

spineArg
    :   expr=excludeExpr   # spineArgTerm
    |   ty=typeArg         # spineArgType
    ;

stmt
    :   expr=typedExpr      # stmtExpr
        // let a: T = e
    |   Let name=termNameDecl typeParams=typeParamList? params+=termParamGroup* (Colon ty=type)? Assign value=typedExpr   # stmtLet
        // letrec f (x: T1) ... (y: Tn): T = e
    |   LetRec name=termNameDecl typeParams=typeParamList? params+=termParamGroup* Colon ty=type Assign value=typedExpr  # stmtLetRec
        // let (x, y, z) = e
    |   Let ParenOpen names+=termNameDecl (Comma names+=termNameDecl)* ParenClose Assign value=typedExpr     # stmtLetTuple
        // let { l1 = e1; l2 = e2; ... } = e
    |   Let BraceOpen fields+=recordFieldAlias (Comma fields+=recordFieldAlias)* BraceClose Assign value=typedExpr     # stmtLetRecord
        // r := e
    |   ref=excludeExpr Walrus value=typedExpr    # stmtRefAssign
    ;

recordFieldAlias
    :   name=labelDecl Assign alias=labelDecl
    ;

typeParamList
    :   BracketOpen params+=boundedTypeParam (Comma params+=boundedTypeParam)* BracketClose     # typeParamListBracket
    |   params+=typeParam+                                                                      # typeParamListPlain
    ;

excludeExpr
    :   expr=renameExpr (DoubleBackslashes excludeType=typeWithSort | Backslash removalField=label)?
    ;

renameExpr
    :   expr=dotExpr (BracketOpen oldName=label LeftArrow newName=labelDecl BracketClose)?
    ;

matchExpr
    :   Match expr=typedExpr BraceOpen (cases+=caseClause Semicolon)* cases+=caseClause Semicolon BraceClose
    ;

caseClause
    :   Case pattern FatArrow expr=typedExpr
    ;

pattern
    :   Underscore                          # patternWildcard
    |   variable=termNameDecl               # patternVar
    |   IntLit                              # patternInt
    |   FloatLit                            # patternFloat
    |   StringLit                           # patternString
    |   BoolLit                             # patternBool
    |   Unit                                # patternUnit
    |   Dollar ctorName params+=termParamGroup*                         # patternCtor
    |   ParenOpen patterns+=pattern Comma patterns+=pattern+ ParenClose     # patternTuple
    |   BraceOpen fieldPatterns+=recordPatternField (Semicolon fieldPatterns+=recordPatternField)* BraceClose   # patternRecord
    ;

recordPatternField
    :   name=labelDecl Assign pattern   # recordPatternFieldMatching
    |   name=labelDecl                  # recordPatternFieldBinding
    |   Underscore                      # recordPatternFieldWildcard
    ;

dotExpr
    :   expr=atomicExpr (Dot path+=label)*
    ;

atomicExpr
    :   termName                        # atomicExprVar
    |   IntLit                          # atomicExprInt
    |   FloatLit                        # atomicExprFloat
    |   StringLit                       # atomicExprString
    |   document                        # atomicExprDoc
    |   Unit                            # atomicExprUnit
    |   BoolLit                         # atomicExprBool
    |   array                           # atomicExprArray
    |   record                          # atomicExprRecord
    |   tuple                           # atomicExprTuple
    |   recordUpdate                    # atomicExprRecordUpdate
    |   Dollar ctorName                 # atomicExprCtor
    |   ParenOpen typedExpr ParenClose  # atomicExprParen
    |   BraceOpen (stmt Semicolon)* typedExpr? BraceClose                   # atomicExprBlock
    |   expr=atomicExpr ParenOpen (args+=typedExpr (Comma args+=typedExpr)*)? ParenClose     # atomicExprApp
    |   expr=atomicExpr BracketOpen args+=type (Comma args+=type)* BracketClose      # atomicExprTypeApp
    ;

tuple
    :   ParenOpen (elements+=typedExpr Comma)+ elements+=typedExpr? ParenClose
    ;

array
    :   BracketOpen (elements+=typedExpr Semicolon)* elements+=typedExpr? BracketClose
    ;

record
    :   BraceOpen (entities+=recordEntity Semicolon)* entities+=recordEntity? BraceClose
    ;

recordEntity
    :   (Impl | Override)? selfAnno? name=labelDecl params+=termParamGroup* Assign value=typedExpr  # recordField
    |   Override? (selfAnno At)? ParenOpen name=labelDecl objectParams+=termParamGroup* ParenClose Dot method=labelDecl methodParams+=termParamGroup* Assign value=typedExpr # recordMethod
    |   (Underscore | selfAnno) Dot name=labelDecl params+=termParamGroup* Assign value=typedExpr # recordDefaultPattern
    ;

recordUpdate
    :   BraceOpen recordExpr=typedExpr With (fields+=fieldAssign Semicolon)* fields+=fieldAssign? BraceClose
    ;

fieldAssign
    :   field=labelDecl Assign value=typedExpr
    ;

typeArg
    :   At typeWithSort
    |   BracketOpen typeWithSort BracketClose
    ;

typeParam
    :   typeNameDecl
    |   ParenOpen typeNameDecl Asterisk type ParenClose
    ;

boundedTypeParam
    :   (lowerBound=type Subtype)? typeNameDecl (Subtype upperBound=type)?
    ;

termParamGroup
    :   name=termNameDecl           # termParamSingleIdent
    |   Underscore                  # termParamSingleWildcard
    |   ParenOpen (groups+=termParamAtomGroup (Comma groups+=termParamAtomGroup)*) ParenClose # termParamMultiple
    |   wildcardRecord        # termParamRecordWildcard
    ;

termParamAtomGroup
    :   params+=termParamIdent+ Colon type
    ;

termParamIdent
    :   termNameDecl
    |   Underscore
    ;

wildcardRecord
    :   BraceOpen (fields+=fieldAssign Semicolon)* DotDot BraceClose
    ;

selfAnno
    :   BracketOpen name=termNameDecl (Colon type)? BracketClose
    ;

sort
    :   inType=type (FatArrow outType=type)?
    ;

moduleName
    :   LowerId
    |   UpperId
    ;

typeNameDecl
    :   UpperId
    ;

typeName
    :   UpperId
    ;

termNameDecl
    :   LowerId
    ;

ctorName
    :   UpperId
    ;

termName
    :   LowerId | UpperId
    ;

labelDecl
    :   LowerId | UpperId
    ;

label
    :   LowerId | UpperId
    ;

/* Documents */

document
    :   BacktickOpen docElement* (BacktickClose | BacktickCloseAfterCmd)
    ;

docElement
    :   command
    |   interpolation
    |   newline
    |   plaintext
    ;

command
    :   (Command | CommandAfterCmd) docArg*
    ;

interpolation
    :   (Interpolation | InterpolationAfterCmd) typedExpr ParenClose
    ;

newline
    :   (NewLine | NewLineAfterCmd)
    ;

plaintext
    :   (PlainText | PlainTextAfterCmd)
    ;

docArg
    :   BracketOpenAsArg docElement* (BracketCloseInDoc | BracketCloseAfterCmd)
    |   ParenOpenAsArg typedExpr ParenClose
    |   BraceOpenAsArg (recordArgField Semicolon)* (recordArgField)? BraceClose
    ;

recordArgField
    :   labelDecl termParamGroup* Assign typedExpr
    ;
