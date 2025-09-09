parser grammar CpParser;

options {
    tokenVocab = CpLexer;
}

program
    :   definitions+=definition* expr=typedExpr EOF
    ;

definition
    :   interfaceDef
    |   typeDef
    |   termDef
    ;

interfaceDef
    :   Interface name=typeNameDecl (Less sorts+=typeNameDecl (Comma sorts+=typeNameDecl Greater)*)? typeParamList? (Extends extends=typeWithSort)? recordType Semicolon
    ;

typeDef
    :   Type name=typeNameDecl (Less sorts+=typeNameDecl (Comma sorts+=typeNameDecl Greater)*)? typeParamList? Assign ty=type Semicolon
    ;

termDef
    :   Def? name=termNameDecl typeParams=typeParamList? paramGroups+=termParamGroup* (Colon ty=type)? Assign expr=typedExpr Semicolon
    ;

type
    :   <assoc=left> lhs=type Intersect rhs=type            # typeIntersect
    |   <assoc=left> lhs=type Union rhs=type                # typeUnion
    |   <assoc=left> lhs=type Backslash rhs=type            # typeDifference
    |   <assoc=right> domain=type Arrow codomain=type       # typeArrow
    |   ForAll typeParamList Dot codomain=type              # typeForAll
    |   Fix name=typeNameDecl Dot type                      # typeFix
    |   TraitType Less type (FatArrow type)? Greater        # typeTrait
    |   RefType typeWithSort                                # typeRef
    |   ty=typeWithSort BracketOpen args+=typeWithSort (Comma args+=typeWithSort)* BracketClose     # typeApp
    |   typeWithSort typeWithSort*                          # typeSpine
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
    :   BraceOpen (recordTypeField Semicolon)* recordTypeField? BraceClose
    ;

recordTypeField
    :   ident=labelDecl optional=Question? Colon ty=type
    ;

typedExpr
    :   expression (Colon type)?
    ;

expression
    :   compExpr                                                 # expressionComplex
    |   (Minus | Sqrt | Deref) expression                           # expressionUnary
    |   <assoc=left> lhs=expression Index rhs=expression            # expressionIndex
    |   <assoc=left> lhs=expression op=(Asterisk | Slash | Modulo) rhs=expression   # expressionMulDiv
    |   <assoc=left> lhs=expression op=(Plus | Minus) rhs=expression                # expressionAddSub
    |   <assoc=left> lhs=expression Append rhs=expression                           # expressionAppend
    |   lhs=expression op=(Less | Greater | LessEqual | GreaterEqual | Equal | NotEqual) rhs=expression # expressionComparison
    |   <assoc=right> lhs=expression And rhs=expression             # expressionAnd
    |   <assoc=right> lhs=expression Or rhs=expression              # expressionOr
    |   <assoc=left> lhs=expression Forward rhs=expression          # expressionForward
    |   <assoc=left> lhs=expression op=(Merge | LeftistMerge | RightistMerge | BackslashMinus) rhs=expression # expressionMerge
    |   lhs=expression Walrus rhs=expression        # expressionRefAssign
    |   lhs=expression Seq rhs=expression           # expressionSeq
    ;

compExpr
    :   (ctorName | excludeExpr) (excludeExpr | typeArg)*                           # compExprApp
    |   (Fun | Backslash | Lambda) termParams+=termParamGroup+ Arrow expr=typedExpr # compExprLambda
    |   SlashBackslash typeParams=typeParamList Dot expr=typedExpr # compExprTypeLambda
    |   Let name=termNameDecl typeParams=typeParamList? params+=termParamGroup* Assign typedExpr In typedExpr  # compExprLetIn
    |   LetRec name=termNameDecl typeParams=typeParamList? params+=termParamGroup* Colon type Assign typedExpr In typedExpr # compExprLetRec
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

stmt
    :   expr=typedExpr      # stmtExpr
        // let a: T = e
    |   Let variable=termNameDecl (Colon type)? Assign expr=typedExpr   # stmtLet
        // letrec f (x: T1) ... (y: Tn): T = e
    |   LetRec variable=termNameDecl typeParams=typeParamList? termParamGroup* Colon ty=type Assign expr=typedExpr  # stmtLetRec
        // let (x, y, z) = e
    |   Let ParenOpen variables+=termNameDecl (Comma variables+=termNameDecl)* ParenClose Assign expr=typedExpr     # stmtLetTuple
        // let { l1 = e1; l2 = e2; ... } = e
    |   Let BraceOpen recordFields+=labelDecl (Comma recordFields+=labelDecl)* BraceClose Assign expr=typedExpr     # stmtLetRecord
    ;

typeParamList
    :   BracketOpen params+=boundedTypeParam (Comma params+=boundedTypeParam)* BracketClose     # typeParamListBracket
    |   params+=typeParam+                                                                      # typeParamListPlain
    ;

appExpr
    :   (ctorName | excludeExpr) (excludeExpr | At typeWithSort)*
    ;

excludeExpr
    :   renameExpr (DoubleBackslashes typeWithSort | Backslash label)?
    ;

renameExpr
    :   dotExpr (BracketOpen label LeftArrow labelDecl BracketClose)?
    ;

matchExpr
    :   Match typedExpr BraceOpen (caseClause Semicolon)* caseClause Semicolon BraceClose
    ;

caseClause
    :   Case pattern FatArrow typedExpr
    ;

pattern
    :   Underscore                          # patternWildcard
    |   variable=termNameDecl               # patternVar
    |   IntLit                              # patternInt
    |   StringLit                           # patternString
    |   BoolLit                             # patternBool
    |   Unit                                # patternUnit
    |   Dollar ctorName params+=termParamGroup*                         # patternCtor
    |   ParenOpen patterns+=pattern Comma patterns+=pattern+ ParenClose     # patternTuple
    |   BraceOpen fieldPatterns+=recordPatternField (Semicolon fieldPatterns+=recordPatternField)* BraceClose   # patternRecord
    ;

recordPatternField
    :   labelDecl (Assign pattern)      # recordPatternFieldMatching
    |   labelDecl                       # recordPatternFieldBinding
    |   Underscore                      # recordPatternFieldWildcard
    ;

dotExpr
    :   atomicExpr (Dot label)*
    ;

atomicExpr
    :   termName                        # atomicExprVar
    |   IntLit                          # atomicExprInt
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
    |   atomicExpr ParenOpen (typedExpr (Comma typedExpr)*)? ParenClose     # atomicExprApp
    |   atomicExpr BracketOpen ty=type (Comma args=type)* BracketClose      # atomicExprTypeApp
    ;

tuple
    :   ParenOpen (typedExpr Comma)+ typedExpr? ParenClose
    ;

array
    :   BracketOpen (typedExpr Semicolon)* typedExpr? BracketClose
    ;

record
    :   BraceOpen
            ((recordField | methodPattern | defaultPattern) Semicolon)*
            ((recordField | methodPattern | defaultPattern))?
        BraceClose
    ;

recordField
    :   (Impl | Override)? selfAnno? labelDecl termParamGroup* Assign typedExpr
    ;

methodPattern
    :   Override? (selfAnno At)? ParenOpen labelDecl termParamGroup* ParenClose Dot labelDecl termParamGroup* Assign typedExpr
    ;

defaultPattern
    :   (Underscore | selfAnno) Dot labelDecl termParamGroup* Assign typedExpr
    ;

recordUpdate
    :   BraceOpen typedExpr With ((labelDecl Assign typedExpr) Semicolon)* (labelDecl Assign typedExpr)? BraceClose
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
    :   termNameDecl
    |   Underscore
    |   ParenOpen (termParamAtom (Comma termParamAtom)*) ParenClose
    |   wildcard
    ;

termParamAtom
    :   (termNameDecl | Underscore)+ Colon type
    ;

wildcard
    :   BraceOpen ((labelDecl Assign typedExpr) Semicolon)* DotDot BraceClose
    ;

selfAnno
    :   BracketOpen termNameDecl (Colon type)? BracketClose
    ;

sort
    :   type (FatArrow type)?
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
