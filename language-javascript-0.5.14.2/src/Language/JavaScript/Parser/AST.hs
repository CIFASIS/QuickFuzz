{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.AST
       (
           Node (..)
         , JSNode(..)
         -- , TokenPosn (..)
         , showStripped
       ) where

import Data.Data
import Data.List
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

-- |The JSNode is the building block of the AST.
-- Each has a syntactic part 'Node'. In addition, the leaf elements
-- (terminals) have a position 'TokenPosn', as well as an array of comments
-- and/or whitespace that was collected while parsing.

data JSNode = NN Node -- ^Non Terminal node, does not have any position or comment information
            | NT Node TokenPosn [CommentAnnotation] -- ^Terminal node, including position and comment/whitespace information
    deriving (Show, Eq, Read, Data, Typeable)

data Node =
              -- | Terminals
                JSIdentifier String
              | JSDecimal String
              | JSLiteral String
              | JSHexInteger String
              | JSOctal String
              | JSStringLiteral Char [Char]
              | JSRegEx String

              -- | Non Terminals

              | JSArguments JSNode [JSNode] JSNode    -- ^lb, args, rb
              | JSArrayLiteral JSNode [JSNode] JSNode -- ^lb, contents, rb
              | JSBlock [JSNode] [JSNode] [JSNode]      -- ^optional lb,optional block statements,optional rb
              | JSBreak JSNode [JSNode] JSNode        -- ^break, optional identifier, autosemi
              | JSCallExpression String [JSNode] [JSNode] [JSNode]  -- ^type : ., (), []; opening [ or ., contents, closing
              | JSCase JSNode JSNode JSNode [JSNode]    -- ^case,expr,colon,stmtlist
              | JSCatch JSNode JSNode JSNode [JSNode] JSNode JSNode -- ^ catch,lb,ident,[if,expr],rb,block
              | JSContinue JSNode [JSNode] JSNode     -- ^continue,optional identifier,autosemi
              | JSDefault JSNode JSNode [JSNode] -- ^default,colon,stmtlist
              | JSDoWhile JSNode JSNode JSNode JSNode JSNode JSNode JSNode -- ^do,stmt,while,lb,expr,rb,autosemi
              | JSElision JSNode               -- ^comma
              | JSExpression [JSNode]          -- ^expression components
              | JSExpressionBinary String [JSNode] JSNode [JSNode] -- ^what, lhs, op, rhs
              | JSExpressionParen JSNode JSNode JSNode -- ^lb,expression,rb
              | JSExpressionPostfix String [JSNode] JSNode -- ^type, expression, operator
              | JSExpressionTernary [JSNode] JSNode [JSNode] JSNode [JSNode] -- ^cond, ?, trueval, :, falseval
              | JSFinally JSNode JSNode -- ^finally,block
              | JSFor JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode -- ^for,lb,expr,semi,expr,semi,expr,rb.stmt
              | JSForIn JSNode JSNode [JSNode] JSNode JSNode JSNode JSNode -- ^for,lb,expr,in,expr,rb,stmt
              | JSForVar JSNode JSNode JSNode [JSNode] JSNode [JSNode] JSNode [JSNode] JSNode JSNode -- ^for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
              | JSForVarIn JSNode JSNode JSNode JSNode JSNode JSNode JSNode JSNode -- ^for,lb,var,vardecl,in,expr,rb,stmt
              | JSFunction JSNode JSNode JSNode [JSNode] JSNode JSNode  -- ^fn,name, lb,parameter list,rb,block
              -- | JSFunctionBody [JSNode] -- ^body
              | JSFunctionExpression JSNode [JSNode] JSNode [JSNode] JSNode JSNode  -- ^fn,[name],lb, parameter list,rb,block`
              | JSIf JSNode JSNode JSNode JSNode [JSNode] [JSNode] -- ^if,(,expr,),stmt,optional rest
              | JSLabelled JSNode JSNode JSNode -- ^identifier,colon,stmt
              | JSMemberDot [JSNode] JSNode JSNode -- ^firstpart, dot, name
              | JSMemberSquare [JSNode] JSNode JSNode JSNode -- ^firstpart, lb, expr, rb
              | JSObjectLiteral JSNode [JSNode] JSNode -- ^lbrace contents rbrace
              | JSOperator JSNode -- ^opnode
              | JSPropertyAccessor JSNode JSNode JSNode [JSNode] JSNode JSNode -- ^(get|set), name, lb, params, rb, block
              | JSPropertyNameandValue JSNode JSNode [JSNode] -- ^name, colon, value
              | JSReturn JSNode [JSNode] JSNode -- ^return,optional expression,autosemi
              -- | JSSourceElements [JSNode] -- ^source elements
              | JSSourceElementsTop [JSNode] -- ^source elements
              -- | JSStatementBlock JSNode JSNode JSNode -- ^lb,block,rb
              -- | JSStatementList [JSNode] -- ^statements
              | JSSwitch JSNode JSNode JSNode JSNode JSNode -- ^switch,lb,expr,rb,caseblock
              | JSThrow JSNode JSNode -- ^throw val
              | JSTry JSNode JSNode [JSNode] -- ^try,block,rest
              | JSUnary String JSNode -- ^type, operator
              | JSVarDecl JSNode [JSNode] -- ^identifier, optional initializer
              | JSVariables JSNode [JSNode] JSNode -- ^var|const, decl, autosemi
              | JSWhile JSNode JSNode JSNode JSNode JSNode -- ^while,lb,expr,rb,stmt
              | JSWith JSNode JSNode JSNode JSNode [JSNode] -- ^with,lb,expr,rb,stmt list
    deriving (Show, Eq, Read, Data, Typeable)

-- Strip out the location info, leaving the original JSNode text representation
showStripped :: JSNode -> String
showStripped = ss

-- Alias for internal use
ss :: JSNode -> String
ss (NN node    ) = showStrippedNode node
ss (NT node _ _) = showStrippedNode node

sss :: [JSNode] -> String
--sss xs = "[" ++ (concatMap ss xs) ++ "]"
sss xs = "[" ++ (concat (intersperse "," $ map ss xs)) ++ "]"

showStrippedNode :: Node -> String
showStrippedNode (JSArguments _lb xs _rb) = "JSArguments " ++ sss xs
showStrippedNode (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " ++ sss xs
showStrippedNode (JSBlock _lb xs _rb) = "JSBlock (" ++ sss xs ++ ")"
showStrippedNode (JSBreak _b x1s as) = "JSBreak " ++ sss x1s ++ " " ++ ss as
showStrippedNode (JSCallExpression s _os xs _cs) = "JSCallExpression " ++ show s ++ " " ++ sss xs
showStrippedNode (JSCase _ca x1 _c x2s) = "JSCase (" ++ ss x1 ++ ") (" ++ sss x2s ++ ")"
showStrippedNode (JSCatch _c _lb x1 x2s _rb x3) = "JSCatch (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSContinue _c xs as) = "JSContinue " ++ sss xs ++ " " ++ ss as
showStrippedNode (JSDecimal s) = "JSDecimal " ++ show s
showStrippedNode (JSDefault _d _c xs) = "JSDefault (" ++ sss xs ++ ")"
showStrippedNode (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSElision c) = "JSElision " ++ ss c
showStrippedNode (JSExpression xs) = "JSExpression " ++ sss xs
showStrippedNode (JSExpressionBinary s x2s _op x3s) = "JSExpressionBinary " ++ show s ++ " " ++ sss x2s ++ " " ++ sss x3s
showStrippedNode (JSExpressionParen _lp x _rp) = "JSExpressionParen (" ++ ss x ++ ")"
showStrippedNode (JSExpressionPostfix s xs _op) = "JSExpressionPostfix " ++ show s ++ " " ++ sss xs
showStrippedNode (JSExpressionTernary x1s _q x2s _c x3s) = "JSExpressionTernary " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s
showStrippedNode (JSFinally _f x) = "JSFinally (" ++ ss x ++ ")"
showStrippedNode (JSFor _f _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
showStrippedNode (JSForIn _f _lb x1s _i x2 _rb x3) = "JSForIn " ++ sss x1s ++ " (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSForVar _f _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " ++ sss x1s ++ " " ++ sss x2s ++ " " ++ sss x3s ++ " (" ++ ss x4 ++ ")"
showStrippedNode (JSForVarIn _f _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" ++ ss x1 ++ ") (" ++ ss x2 ++ ") (" ++ ss x3 ++ ")"
showStrippedNode (JSFunction _f x1 _lb x2s _rb x3) = "JSFunction (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
--showStrippedNode (JSFunctionBody xs) = "JSFunctionBody " ++ sss xs
showStrippedNode (JSFunctionExpression _f x1s _lb x2s _rb x3) = "JSFunctionExpression " ++ sss x1s ++ " " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSHexInteger s) = "JSHexInteger " ++ show s
showStrippedNode (JSOctal s) = "JSOctal " ++ show s
showStrippedNode (JSIdentifier s) = "JSIdentifier " ++ show s
showStrippedNode (JSIf _i _lb x1 _rb x2s x3s) = "JSIf (" ++ ss x1 ++ ") (" ++ sss x2s ++ ") (" ++ sss x3s ++ ")"
showStrippedNode (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSLiteral s) = "JSLiteral " ++ show s
showStrippedNode (JSMemberDot x1s _d x2 ) = "JSMemberDot " ++ sss x1s ++ " (" ++ ss x2 ++ ")"
showStrippedNode (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare " ++ sss x1s ++ " (" ++ ss x2 ++ ")"
showStrippedNode (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " ++ sss xs
showStrippedNode (JSOperator n) = "JSOperator " ++ ss n
showStrippedNode (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ show s ++ " (" ++ ss x1 ++ ") " ++ sss x2s ++ " (" ++ ss x3 ++ ")"
showStrippedNode (JSRegEx s) = "JSRegEx " ++ show s
showStrippedNode (JSReturn _r xs as) = "JSReturn " ++ sss xs ++ " " ++ ss as
--showStrippedNode (JSSourceElements xs) = "JSSourceElements " ++ sss xs
showStrippedNode (JSSourceElementsTop xs) = "JSSourceElementsTop " ++ sss xs
-- showStrippedNode (JSStatementBlock _lb x _rb) = "JSStatementBlock (" ++ ss x ++ ")"
-- showStrippedNode (JSStatementList xs) = "JSStatementList " ++ sss xs
showStrippedNode (JSStringLiteral c s) = "JSStringLiteral " ++ show c ++ " " ++ show s
showStrippedNode (JSSwitch _s _lb x _rb x2) = "JSSwitch (" ++ ss x ++ ") " ++ ss x2
showStrippedNode (JSThrow _t x) = "JSThrow (" ++ ss x ++ ")"
showStrippedNode (JSTry _t x1 x2s) = "JSTry (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSUnary s _x) = "JSUnary " ++ show s
showStrippedNode (JSVarDecl x1 x2s) = "JSVarDecl (" ++ ss x1 ++ ") " ++ sss x2s
showStrippedNode (JSVariables n xs _as) = "JSVariables " ++ ss n ++ " " ++ sss xs
showStrippedNode (JSWhile _w _lb x1 _rb x2) = "JSWhile (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
showStrippedNode (JSWith _w _lb x1 _rb x2s) = "JSWith (" ++ ss x1 ++ ") " ++ sss x2s

-- EOF
