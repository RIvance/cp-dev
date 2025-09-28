lexer grammar CpLexer;

/* SKIPS */

Whitespaces
    :   [ \t\r\n]+ -> skip
    ;

LineComment
    :   '--' ~[\n]+ -> skip
    ;

LineCommentCStyle
    :   '//' ~[\n]+ -> skip
    ;

BlockComment
    :   '{-' .*? '-}' -> skip
    ;

BlockCommentCStyle
    :   '/*' .*? '*/' -> skip
    ;

/* LITERALS */

IntLit
    :   [0-9]+
    |   ('0x' | '0X') [0-9a-fA-F]+
    |   ('0o' | '0O') [0-7]+
    |   ('0b' | '0B') [01]+
    ;

FloatLit
    :   ([0-9]+)? '.' [0-9]+ (('e' | 'E') ('+' | '-')? [0-9]+)? 'f'?
    |   [0-9]+ ('e' | 'E') ('+' | '-')? [0-9]+ 'f'?
    ;

BoolLit
    :   True
    |   False
    ;

StringLit
    :   '"' (~[\\\r\n"] | '\\' .)* '"'
    ;


/* KEYWORDS */

Module
    :   'module'
    ;

Import
    :   'import'
    ;

Main
    :   'main'
    ;

Type
    :   'type'
    ;

Def
    :   'def'
    ;

Fun
    :   'fun'
    ;

Impl
    :   'impl'
    ;

Interface
    :   'interface'
    ;

Extends
    :   'extends'
    ;

Where
    :   'where'
    ;

ForAll
    :   'forall'
    |   '∀'
    ;

TraitType
    :   'Trait'
    ;

Trait
    :   'trait'
    ;

RefType
    :   'Ref'
    ;

Ref
    :   'ref'
    ;

Match
    :   'match'
    ;

Case
    :   'case'
    ;

Implements
    :   'implements'
    ;

Inherits
    :   'inherits'
    ;

New
    :   'new'
    ;

Fix
    :   'fix'
    ;

If
    :   'if'
    ;

Then
    :   'then'
    ;

Else
    :   'else'
    ;

Let
    :   'let'
    ;

Rec
    :   'rec'
    ;

LetRec
    :   'letrec'
    |   Let Rec
    ;

Open
    :   'open'
    ;

In
    :   'in'
    ;

Out
    :   'out'
    ;

With
    :   'with'
    ;

Fold
    :   'fold'
    ;

Unfold
    :   'unfold'
    ;

Override
    :   'override'
    ;

True
    :   'true'
    ;

False
    :   'false'
    ;


/* IDENTIFIERS */

Identifier
    :   ValueIdent | TypeIdent | ContractIdent
    ;

Underscore
    :   '_'
    ;

// camelCaseWithEnglishOrGreekLetters with optional postfix single quotation
//fragment ValueIdent: [a-zα-ω]+([A-ZΑ-Ωa-zα-ω0-9])*('\'')*;
fragment ValueIdent: [a-z]+([A-Za-z0-9])*('\'')*;

// A single blackboard bold letter or PascalCase
fragment TypeIdent: PascalCase | BlackboardBoldLetter;

// Quoted PascalCase: A PascalCase with a prefixed single quotation
fragment ContractIdent: '\''TypeIdent;

fragment PascalCase: [A-Z][a-zA-Z]*;

// Blackboard bold letters (𝔸 - 𝕫)
fragment BlackboardBoldLetter: [𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ];

/* SYMBOLS */

Unit
    :   '()'
    ;

Backslash
    :   '\\'
    ;

Lambda
    :   'λ'
    ;

SlashBackslash
    :   '/\\'
    |   'Λ'
    ;

BackslashMinus
    :   '\\-'
    ;

DoubleBackslashes
    :   '\\\\'
    ;

Arrow
    :   '->'
    ;

FatArrow
    :   '=>'
    ;

LeftArrow
    :   '<-'
    ;

Subtype
    :   '<:'
    ;

Supertype
    :   ':>'
    ;

Intersect
    :   '&'
    ;

Union
    :   '|'
    ;

Plus
    :   '+'
    ;

Minus
    :   '-'
    ;

Asterisk
    :   '*'
    ;

Slash
    :   '/'
    ;

Modulo
    :   '%'
    ;

Sqrt
    :   '√'
    ;

Deref
    :   '!'
    ;

Walrus
    :   ':='
    ;

RightShift
    :   '>>'
    ;

And
    :   '&&'
    ;

Or
    :   '||'
    ;

Less
    :   '<'
    ;

Greater
    :   '>'
    ;

LessEqual
    :   '<='
    ;

GreaterEqual
    :   '>='
    ;

Equal
    :   '=='
    ;

NotEqual
    :   '!='
    ;

Index
    :   '!!'
    ;

Append
    :   '++'
    ;

Forward
    :   '^'
    ;

At
    :   '@'
    ;

Merge
    :   '#'
    |   ',,'
    ;

LeftistMerge
    :   '+#'
    ;

RightistMerge
    :   '#+'
    ;

Assign
    :   '='
    ;

Semicolon
    :   ';'
    ;

Colon
    :   ':'
    ;

Comma
    :   ','
    ;

Dot
    :   '.'
    ;

DotDot
    :   '..'
    ;

Dollar
    :   '$'
    ;

Question
    :   '?'
    ;

PathSep
    :   '::'
    ;

/* BRACKETS */

BracketOpen
    :   '['
    ;

BracketClose
    :   ']'
    ;

BraceOpen
    :   '{' -> pushMode(DEFAULT_MODE)
    ;

BraceClose
    :   '}' -> popMode
    ;

ParenOpen
    :   '(' -> pushMode(DEFAULT_MODE)
    ;

ParenClose
    :   ')' -> popMode
    ;
