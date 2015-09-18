{
{-# LANGUAGE BangPatterns #-}
module Language.JavaScript.Parser.Grammar5 (
    parseProgram
  , parseLiteral
  , parsePrimaryExpression
  , parseStatement
  -- debug
  , fp
  ) where

import Data.Char
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Language.JavaScript.Parser.AST as AST

}

-- The name of the generated function to be exported from the module
%name parseProgram           Program
%name parseLiteral           LiteralMain
%name parsePrimaryExpression PrimaryExpressionMain
%name parseStatement         StatementMain

%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexCont } { EOFToken {} }


%token

     ';'	{ SemiColonToken {} }
     ','	{ CommaToken {} }
     '?'	{ HookToken {} }
     ':'	{ ColonToken {} }
     '||'	{ OrToken {} }
     '&&'	{ AndToken {} }
     '|'	{ BitwiseOrToken {} }
     '^'	{ BitwiseXorToken {} }
     '&'	{ BitwiseAndToken {} }
     '==='	{ StrictEqToken {} }
     '=='	{ EqToken {} }
     '='	{ SimpleAssignToken {} }
     '!=='	{ StrictNeToken {} }
     '!='	{ NeToken {} }
     '<<'	{ LshToken {} }
     '<='	{ LeToken {} }
     '<'	{ LtToken {} }
     '>>>'	{ UrshToken {} }
     '>>'	{ RshToken {} }
     '>='	{ GeToken {} }
     '>'	{ GtToken {} }
     '++'	{ IncrementToken {} }
     '--'	{ DecrementToken {} }
     '+'	{ PlusToken {} }
     '-'	{ MinusToken {} }
     '*'	{ MulToken {} }
     '/'	{ DivToken {} }
     '%'	{ ModToken {} }
     '!'	{ NotToken {} }
     '~'	{ BitwiseNotToken {} }
     '.'	{ DotToken {} }
     '['	{ LeftBracketToken {} }
     ']'	{ RightBracketToken {} }
     '{'	{ LeftCurlyToken {} }
     '}'	{ RightCurlyToken {} }
     '('	{ LeftParenToken {} }
     ')'	{ RightParenToken {} }
     '@*/'	{ CondcommentEndToken {} }

     'autosemi'   { AutoSemiToken {} }
     'break'      { BreakToken {} }
     'case'       { CaseToken {} }
     'catch'      { CatchToken {} }
     'const'      { ConstToken {} }
     'continue'   { ContinueToken {} }
     'debugger'   { DebuggerToken {} }
     'default'    { DefaultToken {} }
     'delete'     { DeleteToken {} }
     'do'         { DoToken {} }
     'else'       { ElseToken {} }
     'enum'       { EnumToken {} }
     'false'      { FalseToken {} }
     'finally'    { FinallyToken {} }
     'for'        { ForToken {} }
     'function'   { FunctionToken {} }
     'get'        { GetToken {} }
     'if'         { IfToken {} }
     'in'         { InToken {} }
     'instanceof' { InstanceofToken {} }
     'new'        { NewToken {} }
     'null'       { NullToken {} }
     'return'     { ReturnToken {} }
     'set'        { SetToken {} }
     'switch'     { SwitchToken {} }
     'this'       { ThisToken {} }
     'throw'      { ThrowToken {} }
     'true'       { TrueToken {} }
     'try'        { TryToken {} }
     'typeof'     { TypeofToken {} }
     'var'        { VarToken {} }
     'void'       { VoidToken {} }
     'while'      { WhileToken {} }
     'with'       { WithToken {} }


     'ident'      { IdentifierToken {} }
     'decimal'    { DecimalToken {} }
     'hexinteger' { HexIntegerToken {} }
     'octal'      { OctalToken {} }
     'string'     { StringToken {} }
     'regex'      { RegExToken {} }
     'assign'     { AssignToken {} }

     'future'     { FutureToken {} }

     'tail'       { TailToken {} }
     'eof'        { EOFToken {} }


%%

-- ---------------------------------------------------------------------
-- Sort out automatically inserted semi-colons.
-- A MaybeSemi is an actual semi-colon or nothing.
-- An AutoSemu is either an actual semi-colon or 'virtual' semi-colon inserted
-- by the Alex lexer or nothing.

MaybeSemi :: { AST.JSNode }
MaybeSemi : ';' { AST.NT (AST.JSLiteral ";") (ss $1) (gc $1)}
          |     { AST.NT (AST.JSLiteral "") tokenPosnEmpty []}

AutoSemi :: { AST.JSNode }
AutoSemi : ';'         { AST.NT (AST.JSLiteral ";") (ss $1) (gc $1)}
         | 'autosemi'  { AST.NT (AST.JSLiteral "") (ss $1) (gc $1)}
         |             { AST.NT (AST.JSLiteral "") tokenPosnEmpty []}

-- ---------------------------------------------------------------------

-- Helpers

LParen :: { AST.JSNode }
LParen : '(' { fp (AST.NT (AST.JSLiteral "(") (ss $1) (gc $1))}

RParen :: { AST.JSNode }
RParen : ')' { fp (AST.NT (AST.JSLiteral ")") (ss $1) (gc $1))}


LBrace :: { AST.JSNode }
LBrace : '{' { fp (AST.NT (AST.JSLiteral "{") (ss $1) (gc $1))}

RBrace :: { AST.JSNode }
RBrace : '}' { fp (AST.NT (AST.JSLiteral "}") (ss $1) (gc $1))}


LSquare :: { AST.JSNode }
LSquare : '[' { fp (AST.NT (AST.JSLiteral "[") (ss $1) (gc $1))}

RSquare :: { AST.JSNode }
RSquare : ']' { fp (AST.NT (AST.JSLiteral "]") (ss $1) (gc $1))}

Comma :: { AST.JSNode }
Comma : ',' { fp (AST.NT (AST.JSLiteral ",") (ss $1) (gc $1))}

Colon :: { AST.JSNode }
Colon : ':' { fp (AST.NT (AST.JSLiteral ":") (ss $1) (gc $1))}

Semi :: { AST.JSNode }
Semi : ';' { fp (AST.NT (AST.JSLiteral ";") (ss $1) (gc $1))}

Dot :: { AST.JSNode }
Dot : '.' { fp (AST.NT (AST.JSLiteral ".") (ss $1) (gc $1))}

Increment :: { AST.JSNode }
Increment : '++' { fp (AST.NT (AST.JSLiteral "++") (ss $1) (gc $1))}

Decrement :: { AST.JSNode }
Decrement : '--' { fp (AST.NT (AST.JSLiteral "--") (ss $1) (gc $1))}

Delete :: { AST.JSNode }
Delete : 'delete' { fp (AST.NT (AST.JSLiteral "delete") (ss $1) (gc $1))}

Void :: { AST.JSNode }
Void : 'void' { fp (AST.NT (AST.JSLiteral "void") (ss $1) (gc $1))}

Typeof :: { AST.JSNode }
Typeof : 'typeof' { fp (AST.NT (AST.JSLiteral "typeof") (ss $1) (gc $1))}

Plus :: { AST.JSNode }
Plus : '+' { fp (AST.NT (AST.JSLiteral "+") (ss $1) (gc $1))}

Minus :: { AST.JSNode }
Minus : '-' { fp (AST.NT (AST.JSLiteral "-") (ss $1) (gc $1))}

Tilde :: { AST.JSNode }
Tilde : '~' { fp (AST.NT (AST.JSLiteral "~") (ss $1) (gc $1))}

Not :: { AST.JSNode }
Not : '!' { fp (AST.NT (AST.JSLiteral "!") (ss $1) (gc $1))}

Mul :: { AST.JSNode }
Mul : '*' { fp (AST.NT (AST.JSLiteral "*") (ss $1) (gc $1))}

Div :: { AST.JSNode }
Div : '/' { fp (AST.NT (AST.JSLiteral "/") (ss $1) (gc $1))}

Mod :: { AST.JSNode }
Mod : '%' { fp (AST.NT (AST.JSLiteral "%") (ss $1) (gc $1))}

Lsh :: { AST.JSNode }
Lsh : '<<' { fp (AST.NT (AST.JSLiteral "<<") (ss $1) (gc $1))}

Rsh :: { AST.JSNode }
Rsh : '>>' { fp (AST.NT (AST.JSLiteral ">>") (ss $1) (gc $1))}

Ursh :: { AST.JSNode }
Ursh : '>>>' { fp (AST.NT (AST.JSLiteral ">>>") (ss $1) (gc $1))}

Le :: { AST.JSNode }
Le : '<=' { fp (AST.NT (AST.JSLiteral "<=") (ss $1) (gc $1))}

Lt :: { AST.JSNode }
Lt : '<' { fp (AST.NT (AST.JSLiteral "<") (ss $1) (gc $1))}

Ge :: { AST.JSNode }
Ge : '>=' { fp (AST.NT (AST.JSLiteral ">=") (ss $1) (gc $1))}

Gt :: { AST.JSNode }
Gt : '>' { fp (AST.NT (AST.JSLiteral ">") (ss $1) (gc $1))}

In :: { AST.JSNode }
In : 'in' { fp (AST.NT (AST.JSLiteral "in") (ss $1) (gc $1))}

Instanceof :: { AST.JSNode }
Instanceof : 'instanceof' { fp (AST.NT (AST.JSLiteral "instanceof") (ss $1) (gc $1))}

StrictEq :: { AST.JSNode }
StrictEq : '===' { fp (AST.NT (AST.JSLiteral "===") (ss $1) (gc $1))}

Equal :: { AST.JSNode }
Equal : '==' { fp (AST.NT (AST.JSLiteral "==") (ss $1) (gc $1))}

StrictNe :: { AST.JSNode }
StrictNe : '!==' { fp (AST.NT (AST.JSLiteral "!==") (ss $1) (gc $1))}

Ne :: { AST.JSNode }
Ne : '!=' { fp (AST.NT (AST.JSLiteral "!=") (ss $1) (gc $1))}

Or :: { AST.JSNode }
Or : '||' { fp (AST.NT (AST.JSLiteral "||") (ss $1) (gc $1))}

And :: { AST.JSNode }
And : '&&' { fp (AST.NT (AST.JSLiteral "&&") (ss $1) (gc $1))}

BitOr :: { AST.JSNode }
BitOr : '|' { fp (AST.NT (AST.JSLiteral "|") (ss $1) (gc $1))}

BitAnd :: { AST.JSNode }
BitAnd : '&' { fp (AST.NT (AST.JSLiteral "&") (ss $1) (gc $1))}

BitXor :: { AST.JSNode }
BitXor : '^' { fp (AST.NT (AST.JSLiteral "^") (ss $1) (gc $1))}

Hook :: { AST.JSNode }
Hook : '?' { fp (AST.NT (AST.JSLiteral "?") (ss $1) (gc $1))}

SimpleAssign :: { AST.JSNode }
SimpleAssign : '=' { fp (AST.NT (AST.JSLiteral "=") (ss $1) (gc $1))}

Assign :: { AST.JSNode }
Assign : 'assign' { fp (AST.NT (AST.JSLiteral (tokenLiteral $1)) (ss $1) (gc $1))}

Var :: { AST.JSNode }
Var : 'var' { fp (AST.NT (AST.JSLiteral "var") (ss $1) (gc $1))}

Const :: { AST.JSNode }
Const : 'const' { fp (AST.NT (AST.JSLiteral "const") (ss $1) (gc $1))}

If :: { AST.JSNode }
If : 'if' { fp (AST.NT (AST.JSLiteral "if") (ss $1) (gc $1))}

Else :: { AST.JSNode }
Else : 'else' { fp (AST.NT (AST.JSLiteral "else") (ss $1) (gc $1))}

Do :: { AST.JSNode }
Do : 'do' { fp (AST.NT (AST.JSLiteral "do") (ss $1) (gc $1))}

While :: { AST.JSNode }
While : 'while' { fp (AST.NT (AST.JSLiteral "while") (ss $1) (gc $1))}

For :: { AST.JSNode }
For : 'for' { fp (AST.NT (AST.JSLiteral "for") (ss $1) (gc $1))}

Continue :: { AST.JSNode }
Continue : 'continue' { fp (AST.NT (AST.JSLiteral "continue") (ss $1) (gc $1))}

Break :: { AST.JSNode }
Break : 'break' { fp (AST.NT (AST.JSLiteral "break") (ss $1) (gc $1))}

Return :: { AST.JSNode }
Return : 'return' { fp (AST.NT (AST.JSLiteral "return") (ss $1) (gc $1))}

With :: { AST.JSNode }
With : 'with' { fp (AST.NT (AST.JSLiteral "with") (ss $1) (gc $1))}

Switch :: { AST.JSNode }
Switch : 'switch' { fp (AST.NT (AST.JSLiteral "switch") (ss $1) (gc $1))}

Case :: { AST.JSNode }
Case : 'case' { fp (AST.NT (AST.JSLiteral "case") (ss $1) (gc $1))}

Default :: { AST.JSNode }
Default : 'default' { fp (AST.NT (AST.JSLiteral "default") (ss $1) (gc $1))}

Throw :: { AST.JSNode }
Throw : 'throw' { fp (AST.NT (AST.JSLiteral "throw") (ss $1) (gc $1))}

Try :: { AST.JSNode }
Try : 'try' { fp (AST.NT (AST.JSLiteral "try") (ss $1) (gc $1))}

CatchL :: { AST.JSNode }
CatchL : 'catch' { fp (AST.NT (AST.JSLiteral "catch") (ss $1) (gc $1))}

FinallyL :: { AST.JSNode }
FinallyL : 'finally' { fp (AST.NT (AST.JSLiteral "finally") (ss $1) (gc $1))}

Function :: { AST.JSNode }
Function : 'function' { fp (AST.NT (AST.JSLiteral "function") (ss $1) (gc $1))}

Eof :: { AST.JSNode }
Eof : 'tail' { fp (AST.NT (AST.JSLiteral "") (ss $1) (gc $1))}

-- Literal ::                                                                See 7.8
--         NullLiteral
--         BooleanLiteral
--         NumericLiteral
--         StringLiteral
Literal :: { AST.JSNode }
Literal : NullLiteral     {$1}
        | BooleanLiteral  {$1}
        | NumericLiteral  {$1}
        | StringLiteral   {$1}
        | RegularExpressionLiteral {$1}

NullLiteral :: { AST.JSNode }
NullLiteral : 'null' { fp (AST.NT (AST.JSLiteral "null") (ss $1) (gc $1))}

BooleanLiteral :: { AST.JSNode }
BooleanLiteral : 'true'  { fp (AST.NT (AST.JSLiteral "true")  (ss $1) (gc $1)) }
               | 'false' { fp (AST.NT (AST.JSLiteral "false") (ss $1) (gc $1)) }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
--                     | OctalLiteral
NumericLiteral :: { AST.JSNode }
NumericLiteral : 'decimal'    { fp (AST.NT (AST.JSDecimal (tokenLiteral $1)) (ss $1) (gc $1))}
               | 'hexinteger' { fp (AST.NT (AST.JSHexInteger (tokenLiteral $1)) (ss $1) (gc $1)) }
               | 'octal'      { fp (AST.NT (AST.JSOctal (tokenLiteral $1)) (ss $1) (gc $1)) }

StringLiteral :: { AST.JSNode }
StringLiteral : 'string'  { fp (AST.NT (AST.JSStringLiteral (token_delimiter $1) (tokenLiteral $1)) (ss $1) (gc $1)) }

-- <Regular Expression Literal> ::= RegExp
RegularExpressionLiteral :: { AST.JSNode }
RegularExpressionLiteral : 'regex' { fp (AST.NT (AST.JSRegEx (tokenLiteral $1)) (ss $1) (gc $1)) }

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSNode }
PrimaryExpression : 'this'                   { fp (AST.NT (AST.JSLiteral "this") (ss $1) (gc $1))}
                  | Identifier               { $1 {- PrimaryExpression1 -}}
                  | Literal                  { $1 {- PrimaryExpression2 -}}
                  | ArrayLiteral             { $1 {- PrimaryExpression3 -}}
                  | ObjectLiteral            { $1 {- PrimaryExpression4 -}}
                  | LParen Expression RParen  { fp (AST.NN (AST.JSExpressionParen $1 $2 $3)) }

-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
Identifier :: { AST.JSNode }
Identifier : 'ident' {  (AST.NT (AST.JSIdentifier (tokenLiteral $1)) (ss $1) (gc $1))}
           | 'get'   {  (AST.NT (AST.JSIdentifier "get") (ss $1) (gc $1))}
           | 'set'   {  (AST.NT (AST.JSIdentifier "set") (ss $1) (gc $1))}

-- TODO: make this include any reserved word too, including future ones
IdentifierName :: { AST.JSNode }
IdentifierName : Identifier {$1}
             | 'break'      { fp (AST.NT (AST.JSIdentifier "break") (ss $1) (gc $1))}
             | 'case'       { fp (AST.NT (AST.JSIdentifier "case") (ss $1) (gc $1))}
             | 'catch'      { fp (AST.NT (AST.JSIdentifier "catch") (ss $1) (gc $1))}
             | 'const'      { fp (AST.NT (AST.JSIdentifier "const") (ss $1) (gc $1))}
             | 'continue'   { fp (AST.NT (AST.JSIdentifier "continue") (ss $1) (gc $1))}
             | 'debugger'   { fp (AST.NT (AST.JSIdentifier "debugger") (ss $1) (gc $1))}
             | 'default'    { fp (AST.NT (AST.JSIdentifier "default") (ss $1) (gc $1))}
             | 'delete'     { fp (AST.NT (AST.JSIdentifier "delete") (ss $1) (gc $1))}
             | 'do'         { fp (AST.NT (AST.JSIdentifier "do") (ss $1) (gc $1))}
             | 'else'       { fp (AST.NT (AST.JSIdentifier "else") (ss $1) (gc $1))}
             | 'enum'       { fp (AST.NT (AST.JSIdentifier "enum") (ss $1) (gc $1))}
             | 'false'      { fp (AST.NT (AST.JSIdentifier "false") (ss $1) (gc $1))}
             | 'finally'    { fp (AST.NT (AST.JSIdentifier "finally") (ss $1) (gc $1))}
             | 'for'        { fp (AST.NT (AST.JSIdentifier "for")  (ss $1) (gc $1))}
             | 'function'   { fp (AST.NT (AST.JSIdentifier "function") (ss $1) (gc $1))}
             | 'get'        { fp (AST.NT (AST.JSIdentifier "get") (ss $1) (gc $1))}
             | 'if'         { fp (AST.NT (AST.JSIdentifier "if") (ss $1) (gc $1))}
             | 'in'         { fp (AST.NT (AST.JSIdentifier "in") (ss $1) (gc $1))}
             | 'instanceof' { fp (AST.NT (AST.JSIdentifier "instanceof") (ss $1) (gc $1))}
             | 'new'        { fp (AST.NT (AST.JSIdentifier "new") (ss $1) (gc $1))}
             | 'null'       { fp (AST.NT (AST.JSIdentifier "null") (ss $1) (gc $1))}
             | 'return'     { fp (AST.NT (AST.JSIdentifier "return") (ss $1) (gc $1))}
             | 'set'        { fp (AST.NT (AST.JSIdentifier "set") (ss $1) (gc $1))}
             | 'switch'     { fp (AST.NT (AST.JSIdentifier "switch") (ss $1) (gc $1))}
             | 'this'       { fp (AST.NT (AST.JSIdentifier "this") (ss $1) (gc $1))}
             | 'throw'      { fp (AST.NT (AST.JSIdentifier "throw") (ss $1) (gc $1))}
             | 'true'       { fp (AST.NT (AST.JSIdentifier "true") (ss $1) (gc $1))}
             | 'try'        { fp (AST.NT (AST.JSIdentifier "try") (ss $1) (gc $1))}
             | 'typeof'     { fp (AST.NT (AST.JSIdentifier "typeof") (ss $1) (gc $1))}
             | 'var'        { fp (AST.NT (AST.JSIdentifier "var") (ss $1) (gc $1))}
             | 'void'       { fp (AST.NT (AST.JSIdentifier "void") (ss $1) (gc $1))}
             | 'while'      { fp (AST.NT (AST.JSIdentifier "while") (ss $1) (gc $1))}
             | 'with'       { fp (AST.NT (AST.JSIdentifier "with") (ss $1) (gc $1))}
             | 'future'     { fp (AST.NT (AST.JSIdentifier (tokenLiteral $1)) (ss $1) (gc $1))}



-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSNode }
ArrayLiteral : LSquare RSquare                           { fp (AST.NN (AST.JSArrayLiteral $1 [] $2))}
             | LSquare Elision RSquare                   { fp (AST.NN (AST.JSArrayLiteral $1 $2 $3))}
             | LSquare ElementList RSquare               { fp (AST.NN (AST.JSArrayLiteral $1 $2 $3))}
             | LSquare ElementList Comma Elision RSquare { fp (AST.NN (AST.JSArrayLiteral $1 ($2++[$3]++$4) $5))}
             | LSquare ElementList Comma RSquare         { fp (AST.NN (AST.JSArrayLiteral $1 ($2++[$3])     $4))}



-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSNode] }
ElementList : Elision AssignmentExpression                 { (($1)++($2)) {- ElementList -}}
            | AssignmentExpression                         { $1           {- ElementList -}}
            | ElementList Comma Elision AssignmentExpression { (($1)++[fp (AST.NN (AST.JSElision $2))]++($3)++($4)) {- ElementList -}}
            | ElementList Comma AssignmentExpression         { (($1)++[fp (AST.NN (AST.JSElision $2))]++($3)) {- ElementList -}}


-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSNode] }
Elision : Comma        { [        fp (AST.NN (AST.JSElision $1))] }
        | Elision Comma { ($1 ++ [fp (AST.NN (AST.JSElision $2))]) }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSNode }
ObjectLiteral : LBrace RBrace                                { fp (AST.NN (AST.JSObjectLiteral $1 [] $2)         )}
              | LBrace PropertyNameandValueList RBrace       { fp (AST.NN (AST.JSObjectLiteral $1 $2 $3)         )}
              | LBrace PropertyNameandValueList Comma RBrace { fp (AST.NN (AST.JSObjectLiteral $1 ($2++[$3]) $4) )}

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- Seems we can have function declarations in the value part too
-- PropertyNameAndValueList :                                            See 11.1.5
--        PropertyAssignment
--        PropertyNameAndValueList , PropertyAssignment
PropertyNameandValueList :: { [ AST.JSNode ] }
PropertyNameandValueList : PropertyAssignment                              { [$1] {- PropertyNameandValueList1 -} }
                         | PropertyNameandValueList Comma PropertyAssignment { ($1++[$2]++[$3]) {- PropertyNameandValueList2 -} }

-- PropertyAssignment :                                                  See 11.1.5
--        PropertyName : AssignmentExpression
--        get PropertyName() { FunctionBody }
--        set PropertyName( PropertySetParameterList ) { FunctionBody }
-- TODO: not clear if get/set are keywords, or just used in a specific context. Puzzling.
PropertyAssignment :: { AST.JSNode }
PropertyAssignment : PropertyName Colon AssignmentExpression { fp (AST.NN (AST.JSPropertyNameandValue $1 $2 $3)) }
                   -- Should be "get" in next, but is not a Token
                   | 'get' PropertyName LParen RParen FunctionBody
                       { fp (AST.NN (AST.JSPropertyAccessor (AST.NT (AST.JSLiteral "get") (ss $1) (gc $1)) $2 $3 [] $4 $5)) }
                   -- Should be "set" in next, but is not a Token
                   | 'set' PropertyName LParen PropertySetParameterList RParen FunctionBody
                       { fp (AST.NN (AST.JSPropertyAccessor (AST.NT (AST.JSLiteral "set") (ss $1) (gc $1)) $2 $3 [$4] $5 $6)) }

-- PropertyName :                                                        See 11.1.5
--        IdentifierName
--        StringLiteral
--        NumericLiteral
PropertyName :: { AST.JSNode }
PropertyName : IdentifierName { $1 {- PropertyName1 -}}
             | StringLiteral  { $1 {- PropertyName2 -}}
             | NumericLiteral { $1 {- PropertyName3 -}}

-- PropertySetParameterList :                                            See 11.1.5
--        Identifier
PropertySetParameterList :: { AST.JSNode }
PropertySetParameterList : Identifier { $1 {- PropertySetParameterList -}}

-- MemberExpression :                                           See 11.2
--        PrimaryExpression
--        FunctionExpression
--        MemberExpression [ Expression ]
--        MemberExpression . IdentifierName
--        new MemberExpression Arguments
MemberExpression :: { [AST.JSNode] }
MemberExpression : PrimaryExpression   { [$1] {- MemberExpression -}}
                 | FunctionExpression  { [$1] {- MemberExpression -}}
                 | MemberExpression LSquare Expression RSquare { [fp (AST.NN (AST.JSMemberSquare $1 $2 $3 $4))] }
                 | MemberExpression Dot IdentifierName         { [fp (AST.NN (AST.JSMemberDot $1 $2 $3))] }
                 | 'new' MemberExpression Arguments            { (((fp (AST.NT (AST.JSLiteral "new") (ss $1) (gc $1))):$2)++[$3])}

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { [AST.JSNode] }
NewExpression : MemberExpression    { $1 {- NewExpression -}}
              | 'new' NewExpression { (fp (AST.NT (AST.JSLiteral "new") (ss $1) (gc $1))):$2 }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { [AST.JSNode] }
CallExpression : MemberExpression Arguments        { $1++[$2] {- CallExpression -} }
               | CallExpression Arguments          { ($1++[fp (AST.NN (AST.JSCallExpression "()" [] [$2] []))]) }
               | CallExpression LSquare Expression RSquare { ($1++[fp (AST.NN (AST.JSCallExpression "[]" [$2] [$3] [$4]))]) }
               | CallExpression Dot IdentifierName { ($1++[fp (AST.NN (AST.JSCallExpression "."  [$2] [$3] []))]) }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
Arguments :: { AST.JSNode }
Arguments : LParen RParen               { fp (AST.NN (AST.JSArguments $1 [] $2)) }
          | LParen ArgumentList RParen  { fp (AST.NN (AST.JSArguments $1 $2 $3)) }

-- ArgumentList :                                               See 11.2
--        AssignmentExpression
--        ArgumentList , AssignmentExpression
ArgumentList :: { [AST.JSNode] }
ArgumentList : AssignmentExpression { $1 {- ArgumentList -}}
             | ArgumentList Comma AssignmentExpression { $1++[$2]++$3 {- ArgumentList2 -} }

-- LeftHandSideExpression :                                     See 11.2
--        NewExpression
--        CallExpression
LeftHandSideExpression :: { [AST.JSNode] }
LeftHandSideExpression : NewExpression  { $1 {- LeftHandSideExpression1 -}}
                       | CallExpression { $1 {- LeftHandSideExpression12 -}}

-- PostfixExpression :                                          See 11.3
--        LeftHandSideExpression
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             ++
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             --
PostfixExpression :: { [AST.JSNode] }
PostfixExpression : LeftHandSideExpression { $1 {- PostfixExpression -} }
                  | PostfixExpression Increment {[fp (AST.NN (AST.JSExpressionPostfix "++" $1 $2))]}
                  | PostfixExpression Decrement {[fp (AST.NN (AST.JSExpressionPostfix "--" $1 $2))]}

-- UnaryExpression :                                            See 11.4
--        PostfixExpression
--        delete UnaryExpression
--        void UnaryExpression
--        typeof UnaryExpression
--        ++ UnaryExpression
--        -- UnaryExpression
--        + UnaryExpression
--        - UnaryExpression
--        ~ UnaryExpression
--        ! UnaryExpression
UnaryExpression :: { [AST.JSNode] }
UnaryExpression : PostfixExpression { $1 {- UnaryExpression -} }
                | Delete    UnaryExpression { ((fp (AST.NN (AST.JSUnary "delete " $1))):$2)}
                | Void      UnaryExpression { ((fp (AST.NN (AST.JSUnary "void "   $1))):$2)}
                | Typeof    UnaryExpression { ((fp (AST.NN (AST.JSUnary "typeof " $1))):$2)}
                | Increment UnaryExpression { ((fp (AST.NN (AST.JSUnary "++"      $1))):$2) }
                | Decrement UnaryExpression { ((fp (AST.NN (AST.JSUnary "--"      $1))):$2)}
                | Plus      UnaryExpression { ((fp (AST.NN (AST.JSUnary "+"       $1))):$2)}
                | Minus     UnaryExpression { ((fp (AST.NN (AST.JSUnary "-"       $1))):$2)}
                | Tilde     UnaryExpression { ((fp (AST.NN (AST.JSUnary "~"       $1))):$2)}
                | Not       UnaryExpression { ((fp (AST.NN (AST.JSUnary "!"       $1))):$2)}

-- MultiplicativeExpression :                                   See 11.5
--        UnaryExpression
--        MultiplicativeExpression * UnaryExpression
--        MultiplicativeExpression / UnaryExpression
--        MultiplicativeExpression % UnaryExpression
MultiplicativeExpression :: { [AST.JSNode] }
MultiplicativeExpression : UnaryExpression { $1 {- MultiplicativeExpression -}}
                         | MultiplicativeExpression Mul UnaryExpression { [fp (AST.NN (AST.JSExpressionBinary "*" $1 $2 $3))]}
                         | MultiplicativeExpression Div UnaryExpression { [fp (AST.NN (AST.JSExpressionBinary "/" $1 $2 $3))]}
                         | MultiplicativeExpression Mod UnaryExpression { [fp (AST.NN (AST.JSExpressionBinary "%" $1 $2 $3))]}

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { [AST.JSNode] }
AdditiveExpression : AdditiveExpression Plus  MultiplicativeExpression { [fp (AST.NN (AST.JSExpressionBinary "+" $1 $2 $3))]}
                   | AdditiveExpression Minus MultiplicativeExpression { [fp (AST.NN (AST.JSExpressionBinary "-" $1 $2 $3))]}
                   | MultiplicativeExpression { $1 {- (goRegExp $1)-} {- AdditiveExpression -} }

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { [AST.JSNode] }
ShiftExpression : ShiftExpression Lsh  AdditiveExpression { [fp (AST.NN (AST.JSExpressionBinary "<<"  $1 $2 $3))]}
                | ShiftExpression Rsh  AdditiveExpression { [fp (AST.NN (AST.JSExpressionBinary ">>"  $1 $2 $3))]}
                | ShiftExpression Ursh AdditiveExpression { [fp (AST.NN (AST.JSExpressionBinary ">>>" $1 $2 $3))]}
                | AdditiveExpression { $1 {- ShiftExpression -}}

-- RelationalExpression :                                      See 11.8
--        ShiftExpression
--        RelationalExpression < ShiftExpression
--        RelationalExpression > ShiftExpression
--        RelationalExpression <= ShiftExpression
--        RelationalExpression >= ShiftExpression
--        RelationalExpression instanceof ShiftExpression
--        RelationalExpression in ShiftExpression
RelationalExpression :: { [AST.JSNode] }
RelationalExpression : ShiftExpression { $1 {- RelationalExpression -}}
                     | RelationalExpression Lt  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary "<"  $1 $2 $3))]}
                     | RelationalExpression Gt  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary ">"  $1 $2 $3))]}
                     | RelationalExpression Le  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary "<=" $1 $2 $3))]}
                     | RelationalExpression Ge  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary ">=" $1 $2 $3))]}
                     | RelationalExpression Instanceof ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary " instanceof " $1 $2 $3))]}
                     | RelationalExpression In         ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary " in "         $1 $2 $3))]}

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { [AST.JSNode] }
RelationalExpressionNoIn : ShiftExpression { $1 {- RelationalExpressionNoIn -}}
                     | RelationalExpressionNoIn Lt  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary "<"  $1 $2 $3))]}
                     | RelationalExpressionNoIn Gt  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary ">"  $1 $2 $3))]}
                     | RelationalExpressionNoIn Le  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary "<=" $1 $2 $3))]}
                     | RelationalExpressionNoIn Ge  ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary ">=" $1 $2 $3))]}
                     | RelationalExpressionNoIn Instanceof ShiftExpression { [fp (AST.NN (AST.JSExpressionBinary " instanceof " $1 $2 $3))]}

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { [AST.JSNode] }
EqualityExpression : RelationalExpression { $1 {- EqualityExpression -} }
                   | EqualityExpression Equal    RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "=="  $1 $2 $3))]}
                   | EqualityExpression Ne       RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "!="  $1 $2 $3))]}
                   | EqualityExpression StrictEq RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "===" $1 $2 $3))]}
                   | EqualityExpression StrictNe RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "!==" $1 $2 $3))]}

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { [AST.JSNode] }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- EqualityExpressionNoIn -} }
                       | EqualityExpressionNoIn Equal    RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "=="  $1 $2 $3))]}
                       | EqualityExpressionNoIn Ne       RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "!="  $1 $2 $3))]}
                       | EqualityExpressionNoIn StrictEq RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "===" $1 $2 $3))]}
                       | EqualityExpressionNoIn StrictNe RelationalExpression { [fp (AST.NN (AST.JSExpressionBinary "!==" $1 $2 $3))]}

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { [AST.JSNode] }
BitwiseAndExpression : EqualityExpression { $1 {- BitwiseAndExpression -} }
                     | BitwiseAndExpression BitAnd EqualityExpression { [fp (AST.NN (AST.JSExpressionBinary "&" $1 $2 $3))]}

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { [AST.JSNode] }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- BitwiseAndExpression -} }
                     | BitwiseAndExpressionNoIn BitAnd EqualityExpressionNoIn { [fp (AST.NN (AST.JSExpressionBinary "&" $1 $2 $3))]}

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { [AST.JSNode] }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- BitwiseXOrExpression -} }
                     | BitwiseXOrExpression BitXor BitwiseAndExpression { [fp (AST.NN (AST.JSExpressionBinary "^" $1 $2 $3))]}

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { [AST.JSNode] }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- BitwiseXOrExpression -} }
                         | BitwiseXOrExpressionNoIn BitXor BitwiseAndExpressionNoIn { [fp (AST.NN (AST.JSExpressionBinary "^" $1 $2 $3))]}

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { [AST.JSNode] }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- BitwiseOrExpression -} }
                    | BitwiseOrExpression BitOr BitwiseXOrExpression { [fp (AST.NN (AST.JSExpressionBinary "|" $1 $2 $3))]}

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { [AST.JSNode] }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- BitwiseOrExpression -} }
                        | BitwiseOrExpressionNoIn BitOr BitwiseXOrExpressionNoIn { [fp (AST.NN (AST.JSExpressionBinary "|" $1 $2 $3))]}

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { [AST.JSNode] }
LogicalAndExpression : BitwiseOrExpression { $1 {- LogicalAndExpression -} }
                     | LogicalAndExpression And BitwiseOrExpression { [fp (AST.NN (AST.JSExpressionBinary "&&" $1 $2 $3))]}

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { [AST.JSNode] }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- LogicalAndExpression -} }
                         | LogicalAndExpressionNoIn And BitwiseOrExpressionNoIn { [fp (AST.NN (AST.JSExpressionBinary "&&" $1 $2 $3))]}

-- LogicalORExpression :                                                                 See 11.11
--        LogicalANDExpression
--        LogicalORExpression || LogicalANDExpression
LogicalOrExpression :: { [AST.JSNode] }
LogicalOrExpression : LogicalAndExpression { $1 {- LogicalOrExpression -} }
                    | LogicalOrExpression Or LogicalAndExpression { [fp (AST.NN (AST.JSExpressionBinary "||" $1 $2 $3))]}

-- LogicalORExpressionNoIn :                                                             See 11.11
--        LogicalANDExpressionNoIn
--        LogicalORExpressionNoIn || LogicalANDExpressionNoIn
LogicalOrExpressionNoIn :: { [AST.JSNode] }
LogicalOrExpressionNoIn : LogicalAndExpressionNoIn { $1 {- LogicalOrExpression -} }
                        | LogicalOrExpressionNoIn Or LogicalAndExpressionNoIn { [fp (AST.NN (AST.JSExpressionBinary "||" $1 $2 $3))]}

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { [AST.JSNode] }
ConditionalExpression : LogicalOrExpression { $1 {- ConditionalExpression -} }
                      | LogicalOrExpression Hook AssignmentExpression Colon AssignmentExpression
                        { [fp (AST.NN (AST.JSExpressionTernary $1 $2 $3 $4 $5))] }

-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { [AST.JSNode] }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- ConditionalExpression -} }
                          | LogicalOrExpressionNoIn Hook AssignmentExpressionNoIn Colon AssignmentExpressionNoIn
                            { [fp (AST.NN (AST.JSExpressionTernary $1 $2 $3 $4 $5))] }

-- AssignmentExpression :                                                                See 11.13
--        ConditionalExpression
--        LeftHandSideExpression AssignmentOperator AssignmentExpression
AssignmentExpression :: { [AST.JSNode] }
AssignmentExpression : ConditionalExpression { $1 {- AssignmentExpression -}}
                     | LeftHandSideExpression AssignmentOperator AssignmentExpression
                       { ($1++[$2]++$3) }

-- AssignmentExpressionNoIn :                                                            See 11.13
--        ConditionalExpressionNoIn
--        LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
AssignmentExpressionNoIn :: { [AST.JSNode] }
AssignmentExpressionNoIn : ConditionalExpressionNoIn { $1 {- AssignmentExpression -}}
                         | LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
                           { ($1++[$2]++$3) }

-- AssignmentOperator : one of                                                           See 11.13
--     '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
AssignmentOperator :: { AST.JSNode }
AssignmentOperator : Assign       { fp (AST.NN (AST.JSOperator $1))}
                   | SimpleAssign { fp (AST.NN (AST.JSOperator $1))}

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSNode }
Expression : AssignmentExpression { fp (AST.NN (AST.JSExpression $1)) {- Expression -} }
           | Expression Comma AssignmentExpression  { fp (AST.NN (AST.JSExpression ($1:[$2]++$3))) {- Expression2 -} }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSNode }
ExpressionNoIn : AssignmentExpressionNoIn { fp (AST.NN (AST.JSExpression $1)) {- ExpressionNoIn -} }
               | ExpressionNoIn Comma AssignmentExpressionNoIn  { fp (AST.NN (AST.JSExpression ($1:[$2]++$3))) {- ExpressionNoIn2 -} }

-- TODO: still required?
ExpressionOpt :: { [AST.JSNode] }
ExpressionOpt : Expression { [$1] {- ExpressionOpt -}}
              |            { []   {- ExpressionOpt -}}

ExpressionNoInOpt :: { [AST.JSNode] }
ExpressionNoInOpt : ExpressionNoIn { [$1] {- ExpressionOpt -}}
                  |            { []   {- ExpressionOpt -}}


-- Statement :                                                    See clause 12
--         Block
--         VariableStatement
--         EmptyStatement
--         ExpressionStatement
--         IfStatement
--         IterationStatement
--         ContinueStatement
--         BreakStatement
--         ReturnStatement
--         WithStatement
--         LabelledStatement
--         SwitchStatement
--         ThrowStatement
--         TryStatement
--         DebuggerStatement
Statement :: { AST.JSNode }
Statement : StatementNoEmpty   { $1 {- Statement1 -}}
          | EmptyStatement     { $1 {- Statement3 -}}

StatementNoEmpty :: { AST.JSNode }
StatementNoEmpty : StatementBlock      { $1 {- StatementNoEmpty1 -}}
                 | VariableStatement   { $1 {- StatementNoEmpty2 -}}
                 | ExpressionStatement { $1 {- StatementNoEmpty4 -}}
                 | IfStatement         { $1 {- StatementNoEmpty5 -}}
                 | IterationStatement  { $1 {- StatementNoEmpty6 -}}
                 | ContinueStatement   { $1 {- StatementNoEmpty7 -}}
                 | BreakStatement      { $1 {- StatementNoEmpty8 -}}
                 | ReturnStatement     { $1 {- StatementNoEmpty9 -}}
                 | WithStatement       { $1 {- StatementNoEmpty10 -}}
                 | LabelledStatement   { $1 {- StatementNoEmpty11 -}}
                 | SwitchStatement     { $1 {- StatementNoEmpty12 -}}
                 | ThrowStatement      { $1 {- StatementNoEmpty13 -}}
                 | TryStatement        { $1 {- StatementNoEmpty14 -}}
                 | DebuggerStatement   { $1 {- StatementNoEmpty15 -}}


StatementBlock :: { AST.JSNode }
StatementBlock : LBrace RBrace               { fp (AST.NN (AST.JSBlock [$1] [] [$2])) }
               | LBrace StatementList RBrace { fp (AST.NN (AST.JSBlock [$1] $2 [$3])) }

-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSNode }
Block : LBrace RBrace               { fp (AST.NN (AST.JSBlock [$1] [] [$2])) }
      | LBrace StatementList RBrace { fp (AST.NN (AST.JSBlock [$1] $2 [$3])) }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { [AST.JSNode] }
StatementList : Statement               { [$1]       {- StatementList1 -} }
              | StatementList Statement { ($1++[$2]) {- StatementList2 -} }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSNode }
VariableStatement : Var   VariableDeclarationList MaybeSemi { fp (AST.NN (AST.JSVariables $1 $2 $3))}
                  | Const VariableDeclarationList MaybeSemi { fp (AST.NN (AST.JSVariables $1 $2 $3))}

-- VariableDeclarationList :                                      See 12.2
--         VariableDeclaration
--         VariableDeclarationList , VariableDeclaration
VariableDeclarationList :: { [AST.JSNode] }
VariableDeclarationList : VariableDeclaration { [$1] {- VariableDeclarationList -}}
                        | VariableDeclarationList Comma VariableDeclaration { ($1++[$2]++[$3]) {- VariableDeclarationList -}}

-- VariableDeclarationListNoIn :                                  See 12.2
--         VariableDeclarationNoIn
--         VariableDeclarationListNoIn , VariableDeclarationNoIn
VariableDeclarationListNoIn :: { [AST.JSNode] }
VariableDeclarationListNoIn : VariableDeclarationNoIn { [$1] {- VariableDeclarationList -}}
                            | VariableDeclarationListNoIn Comma VariableDeclarationNoIn { ($1++[$2]++[$3]) {- VariableDeclarationListNoIn -}}

-- VariableDeclaration :                                          See 12.2
--         Identifier Initialiseropt
VariableDeclaration :: { AST.JSNode }
VariableDeclaration : Identifier              { fp (AST.NN (AST.JSVarDecl $1 []))}
                    | Identifier Initializer  { fp (AST.NN (AST.JSVarDecl $1 $2))}

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSNode }
VariableDeclarationNoIn : Identifier InitializerNoIn { fp (AST.NN (AST.JSVarDecl $1 $2)) }
                        | Identifier                 { fp (AST.NN (AST.JSVarDecl $1 [])) }

-- Initialiser :                                                                            See 12.2
--         = AssignmentExpression
Initializer :: { [AST.JSNode] }
Initializer : SimpleAssign AssignmentExpression { $1:$2 {- Initializer -} }

-- InitialiserNoIn :                                                                        See 12.2
--         = AssignmentExpressionNoIn
InitializerNoIn :: { [AST.JSNode] }
InitializerNoIn : SimpleAssign AssignmentExpressionNoIn { $1:$2 {- InitializerNoIn -}}

-- EmptyStatement :                                                                         See 12.3
--         ;
EmptyStatement :: { AST.JSNode }
EmptyStatement : Semi { $1 }

-- ExpressionStatement :                                                                    See 12.4
--         [lookahead not in {{, function}] Expression  ;
-- TODO: Sort out lookahead issue. Maybe by just putting production lower to set reduce/reduce conflict
--       According to http://sideshowbarker.github.com/es5-spec/#x12.4, the ambiguity is with
--       Block or FunctionDeclaration
ExpressionStatement :: { AST.JSNode }
ExpressionStatement : Expression { $1 {- ExpressionStatement -} }


-- IfStatement :                                                                            See 12.5
--         if ( Expression ) Statement else Statement
--         if ( Expression ) Statement
IfStatement :: { AST.JSNode } -- +++XXXX++
IfStatement : If LParen Expression RParen StatementSemi IfElseRest
                  { (fp (AST.NN (AST.JSIf $1 $2 $3 $4 $5 $6)) ) }

IfElseRest :: { [AST.JSNode] }
IfElseRest : Else Statement     { [$1,$2] }
           |                    { [] }

StatementSemi :: { [AST.JSNode] }
StatementSemi : StatementNoEmpty Semi { [$1,$2] {- StatementSemi1 -}}
              | StatementNoEmpty      { [$1]    {- StatementSemi2 -}}
              | Semi                  { [$1]    {- StatementSemi3 -}}


-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSNode }
IterationStatement : Do Statement While LParen Expression RParen MaybeSemi
                     { fp (AST.NN (AST.JSDoWhile $1 $2 $3 $4 $5 $6 $7)) }
                   | Do Expression MaybeSemi While LParen Expression RParen MaybeSemi
                     { fp (AST.NN (AST.JSDoWhile $1 $2 $4 $5 $6 $7 $8)) }
                   | While LParen Expression RParen Statement
                     { fp (AST.NN (AST.JSWhile $1 $2 $3 $4 $5)) }
                   | For LParen ExpressionNoInOpt Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { fp (AST.NN (AST.JSFor $1 $2 $3 $4 $5 $6 $7 $8 $9)) }
                   | For LParen Var VariableDeclarationListNoIn Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { fp (AST.NN (AST.JSForVar $1 $2 $3 $4 $5 $6 $7 $8 $9 $10)) }
                   | For LParen LeftHandSideExpression In Expression RParen Statement
                     { fp (AST.NN (AST.JSForIn $1 $2 $3 $4 $5 $6 $7)) }
                   | For LParen Var VariableDeclarationNoIn In Expression RParen Statement
                     { fp (AST.NN (AST.JSForVarIn $1 $2 $3 $4 $5 $6 $7 $8)) }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
ContinueStatement :: { AST.JSNode }
ContinueStatement : Continue AutoSemi              { fp (AST.NN (AST.JSContinue $1 []   $2)) }
                  | Continue Identifier MaybeSemi  { fp (AST.NN (AST.JSContinue $1 [$2] $3)) }

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
-- TODO: deal with [no LineTerminator here]
BreakStatement :: { AST.JSNode }
BreakStatement : Break AutoSemi              { fp (AST.NN (AST.JSBreak $1 []   $2)) }
               | Break Identifier MaybeSemi  { fp (AST.NN (AST.JSBreak $1 [$2] $3)) }

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
-- TODO: deal with [no LineTerminator here]
ReturnStatement :: { AST.JSNode }
ReturnStatement : Return AutoSemi              { fp (AST.NN (AST.JSReturn $1 []   $2)) }
                | Return Expression MaybeSemi  { fp (AST.NN (AST.JSReturn $1 [$2] $3)) }

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSNode }
WithStatement : With LParen Expression RParen Statement MaybeSemi  { fp (AST.NN (AST.JSWith $1 $2 $3 $4 [$5,$6])) }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSNode }
SwitchStatement : Switch LParen Expression RParen CaseBlock { (AST.NN (AST.JSSwitch $1 $2 $3 $4 $5)) }

-- CaseBlock :                                                                              See 12.11
--         { CaseClausesopt }
--         { CaseClausesopt DefaultClause CaseClausesopt }
CaseBlock :: { AST.JSNode }
CaseBlock : LBrace CaseClausesOpt RBrace                              { fp (AST.NN (AST.JSBlock [$1] $2             [$3])){- CaseBlock1 -}}
          | LBrace CaseClausesOpt DefaultClause CaseClausesOpt RBrace { fp (AST.NN (AST.JSBlock [$1] ($2++[$3]++$4) [$5])){- CaseBlock2 -}}

-- CaseClauses :                                                                            See 12.11
--         CaseClause
--         CaseClauses CaseClause
CaseClausesOpt :: { [AST.JSNode] }
CaseClausesOpt : CaseClause                { [$1] {- CaseClauses1 -}}
               | CaseClausesOpt CaseClause { ($1++[$2]) {- CaseClauses2 -}}
               |                           { [fp (AST.NT (AST.JSLiteral "") tokenPosnEmpty []) ] } -- { [] }

-- CaseClause :                                                               See 12.11
--        case Expression : StatementListopt
CaseClause :: { AST.JSNode }
CaseClause : Case Expression Colon StatementList  { fp (AST.NN (AST.JSCase $1 $2 $3 $4)) }
           | Case Expression Colon                { fp (AST.NN (AST.JSCase $1 $2 $3 [])) }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSNode }
DefaultClause : Default Colon                { fp (AST.NN (AST.JSDefault $1 $2 [])) }
              | Default Colon StatementList  { fp (AST.NN (AST.JSDefault $1 $2 $3)) }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSNode }
LabelledStatement : Identifier Colon Statement { fp (AST.NN (AST.JSLabelled $1 $2 $3)) }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
-- TODO : sort out no LineTerminator here
--        Does it need a semi at the end?
ThrowStatement :: { AST.JSNode }
ThrowStatement : Throw Expression { fp (AST.NN (AST.JSThrow $1 $2)) }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSNode }
TryStatement : Try Block Catches         { fp (AST.NN (AST.JSTry $1 $2 $3)         ) {- TryStatement1 -} }
             | Try Block Finally         { fp (AST.NN (AST.JSTry $1 $2 [$3])       ) {- TryStatement2 -} }
             | Try Block Catches Finally { fp (AST.NN (AST.JSTry $1 $2 ($3++[$4])) ) {- TryStatement3 -} }

Catches :: { [AST.JSNode] }
Catches : Catch         { [$1]       {- Catches 1 -} }
        | Catches Catch { ($1++[$2]) {- Catches 2 -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSNode }
Catch : CatchL LParen Identifier                          RParen Block { fp (AST.NN (AST.JSCatch $1 $2 $3 [     ] $4 $5)) }
      | CatchL LParen Identifier If ConditionalExpression RParen Block { fp (AST.NN (AST.JSCatch $1 $2 $3 ($4:$5) $6 $7)) }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSNode }
Finally : FinallyL Block { fp (AST.NN (AST.JSFinally $1 $2)) }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSNode }
DebuggerStatement : 'debugger' MaybeSemi { fp (AST.NT (AST.JSLiteral "debugger") (ss $1) (gc $1)) }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSNode }
FunctionDeclaration : Function Identifier LParen FormalParameterList RParen FunctionBody
                      { fp (AST.NN (AST.JSFunction $1 $2 $3 $4 $5 $6) ) }
                    | Function Identifier LParen RParen FunctionBody
                      { fp (AST.NN (AST.JSFunction $1 $2 $3 [] $4 $5) ) }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSNode }
FunctionExpression : Function IdentifierOpt LParen RParen FunctionBody
                     { fp (AST.NN (AST.JSFunctionExpression $1 $2 $3 [] $4 $5) ) }
                   | Function IdentifierOpt LParen FormalParameterList RParen FunctionBody
                     { fp (AST.NN (AST.JSFunctionExpression $1 $2 $3 $4 $5 $6) ) }

IdentifierOpt :: { [AST.JSNode] }
IdentifierOpt : Identifier { [$1] {- IdentifierOpt -}}
              |            { []   {- IdentifierOpt -}}

-- FormalParameterList :                                                      See clause 13
--        Identifier
--        FormalParameterList , Identifier
FormalParameterList :: { [AST.JSNode] }
FormalParameterList : Identifier                            { [$1] {- FormalParameterList -}}
                    | FormalParameterList Comma Identifier  { ($1++[$2]++[$3]) }

-- FunctionBody :                                                             See clause 13
--        SourceElementsopt
FunctionBody :: { AST.JSNode }
FunctionBody : LBrace SourceElements RBrace { (AST.NN (AST.JSBlock [$1] $2 [$3]) ) }
             | LBrace                RBrace { (AST.NN (AST.JSBlock [$1] [] [$2]) ) }

-- Program :                                                                  See clause 14
--        SourceElementsopt

Program :: { AST.JSNode }
Program : SourceElementsTop Eof { (combineTop $1 $2) {- Program -}}
        | Eof                   { fp (AST.NN (AST.JSSourceElementsTop [$1])) }

-- For debugging/other entry points
LiteralMain :: { AST.JSNode }
LiteralMain : Literal Eof { $1 }

PrimaryExpressionMain :: { AST.JSNode }
PrimaryExpressionMain : PrimaryExpression Eof { $1 }

StatementMain :: { AST.JSNode }
StatementMain : Statement Eof { $1 }


-- SourceElements :                                                           See clause 14
--        SourceElement
--        SourceElements SourceElement
SourceElements :: { [AST.JSNode] }
SourceElements : SourceElement                { [$1]     {- SourceElements -} }
               | SourceElements SourceElement { $1++[$2] {- SourceElements -} }

SourceElementsTop :: { AST.JSNode }
SourceElementsTop : SourceElement                   { fp (AST.NN (AST.JSSourceElementsTop [$1]) ) }
                  | SourceElementsTop SourceElement { (combineSourceElementsTop $1 $2) }

-- SourceElement :
--       Statement
--       FunctionDeclaration
SourceElement :: { AST.JSNode }
SourceElement : Statement            { $1 {- SourceElement1 -} }
              | FunctionDeclaration  { $1 {- SourceElement2 -} }

{
combineSourceElementsTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineSourceElementsTop (AST.NN (AST.JSSourceElementsTop xs)) x1 = fp (AST.NN (AST.JSSourceElementsTop (xs++[x1])))

combineTop :: AST.JSNode -> AST.JSNode -> AST.JSNode
combineTop (AST.NN (AST.JSSourceElementsTop xs)) x1 = fp (AST.NN (AST.JSSourceElementsTop (xs++[x1])))


parseError :: Token -> Alex a
parseError tok = alexError (show tok)

-- --------------------------------

ss :: Token -> TokenPosn
ss = tokenSpan

-- ------------------------------

gc :: Token -> [CommentAnnotation]
gc = tokenComment
mgc :: [Token] -> [CommentAnnotation]
mgc xs = concatMap tokenComment xs

-- ---------------------------------------------------------------------

fp :: AST.JSNode -> AST.JSNode
fp (AST.NN x)      = (AST.NN x)
fp (AST.NT x p cs) = (AST.NT x p cs)

}
