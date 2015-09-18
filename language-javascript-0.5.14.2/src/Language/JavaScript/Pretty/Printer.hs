module Language.JavaScript.Pretty.Printer
    ( -- * Printing
      renderJS
    , renderToString
    ) where

import Data.Char
import Data.List
import Data.Monoid (Monoid, mappend, mempty)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Codec.Binary.UTF8.String as US

-- ---------------------------------------------------------------------

data Foo = Foo (Int,Int) BB.Builder

-- ---------------------------------------------------------------------
-- Pretty printer stuff via blaze-builder

(<>) :: BB.Builder -> BB.Builder -> BB.Builder
(<>) a b = mappend a b

-- (<+>) :: BB.Builder -> BB.Builder -> BB.Builder
-- (<+>) a b = mconcat [a, (text " "), b]

--() ((Int, Int), BB.Builder) -> ((Int, Int), BB.Builder) -> ((Int, Int), BB.Builder)
--() a b =

-- hcat :: (Monoid a) => [a] -> a
-- hcat xs = mconcat xs

empty :: BB.Builder
empty = mempty

text :: String -> BB.Builder
text s = BS.fromString s

-- char :: Char -> BB.Builder
-- char c = BS.fromChar c

-- comma :: BB.Builder
-- comma = BS.fromChar ','

-- punctuate :: a -> [a] -> [a]
-- punctuate p xs = intersperse p xs

-- ---------------------------------------------------------------------

renderJS :: JSNode -> BB.Builder
renderJS node = bb
  where
    Foo _ bb = rn node (Foo (1,1) empty)

-- Take in the current
-- rn :: (Int, Int) -> JSNode -> ((Int, Int), BB.Builder)
rn :: JSNode -> Foo -> Foo

-- Terminals
rn (NT (JSIdentifier s     ) p cs) foo = rcs cs p s  foo
rn (NT (JSDecimal i        ) p cs) foo = rcs cs p i foo
rn (NT (JSLiteral l        ) p cs) foo = rcs cs p l foo
rn (NT (JSHexInteger i     ) p cs) foo = rcs cs p i foo
rn (NT (JSOctal i          ) p cs) foo = rcs cs p i foo
rn (NT (JSStringLiteral s l) p cs) foo = rcs cs p ((s:l)++[s]) foo
rn (NT (JSRegEx s          ) p cs) foo = rcs cs p s foo

-- Non-Terminals
rn (NN (JSArguments lb xs rb))                    foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (NN (JSArrayLiteral lb xs rb))                 foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (NN (JSBlock lb x rb))                         foo = rJS (lb ++ x ++ rb) foo
rn (NN (JSBreak b x1s as))                        foo = rJS ([b]++x1s++[as]) foo
rn (NN (JSCallExpression _s os xs cs))            foo = rJS (os ++ xs ++ cs) foo
rn (NN (JSCase ca x1 c x2s))                      foo = rJS ([ca,x1,c]++x2s) foo
rn (NN (JSCatch c lb x1 x2s rb x3))               foo = rJS ([c,lb,x1]++x2s++[rb,x3]) foo
rn (NN (JSContinue c xs as))                      foo = rJS ([c]++xs++[as]) foo
rn (NN (JSDefault d c xs))                        foo = rJS ([d,c]++xs) foo
rn (NN (JSDoWhile d x1 w lb x2 rb x3))            foo = rJS ([d,x1,w,lb,x2,rb,x3]) foo
rn (NN (JSElision c))                             foo = rJS [c] foo
rn (NN (JSExpression xs))                         foo = rJS xs foo
rn (NN (JSExpressionBinary _s lhs op rhs))        foo = rJS (lhs ++ [op] ++ rhs) foo
rn (NN (JSExpressionParen lb e rb))               foo = rJS ([lb,e,rb]) foo
rn (NN (JSExpressionPostfix _s xs op))            foo = rJS (xs ++ [op]) foo
rn (NN (JSExpressionTernary cond h v1 c v2))      foo = rJS (cond ++[h] ++ v1 ++ [c] ++ v2) foo
rn (NN (JSFinally f x))                           foo = rJS [f,x] foo
rn (NN (JSFor f lb x1s s1 x2s s2 x3s rb x4))      foo = rJS ([f,lb]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) foo
rn (NN (JSForIn f lb x1s i x2 rb x3))             foo = rJS ([f,lb]++x1s++[i,x2,rb,x3]) foo
rn (NN (JSForVar f lb v x1s s1 x2s s2 x3s rb x4)) foo = rJS ([f,lb,v]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]) foo
rn (NN (JSForVarIn f lb v x1 i x2 rb x3))         foo = rJS [f,lb,v,x1,i,x2,rb,x3] foo
rn (NN (JSFunction f x1 lb x2s rb x3))            foo = rJS ([f,x1,lb]++x2s++[rb,x3]) foo
-- rn (NN (JSFunctionBody xs))                       foo = rJS xs foo
rn (NN (JSFunctionExpression f x1s lb x2s rb x3)) foo = rJS ([f] ++ x1s ++ [lb] ++ x2s ++ [rb,x3]) foo
rn (NN (JSIf i lb x1 rb x2s x3s))                 foo = rJS ([i,lb,x1,rb]++x2s++x3s) foo
rn (NN (JSLabelled l c v))                        foo = rJS [l,c,v] foo
rn (NN (JSMemberDot xs dot n))                    foo = rJS (xs ++ [dot,n]) foo
rn (NN (JSMemberSquare xs lb e rb))               foo = rJS (xs ++ [lb,e,rb]) foo
rn (NN (JSObjectLiteral lb xs rb))                foo = rJS ([lb] ++ xs ++ [rb]) foo
rn (NN (JSOperator n))                            foo = rJS [n] foo
rn (NN (JSPropertyAccessor s n lb1 ps rb1 b))     foo = rJS ([s,n,lb1] ++ ps ++ [rb1,b]) foo
rn (NN (JSPropertyNameandValue n colon vs))       foo = rJS ([n,colon] ++ vs) foo
rn (NN (JSReturn r xs as))                        foo = rJS ([r] ++ xs ++ [as]) foo
-- rn (NN (JSSourceElements    xs))                  foo = rJS xs foo
rn (NN (JSSourceElementsTop xs))                  foo = rJS xs foo
-- rn (NN (JSStatementBlock lb x rb))                foo = rJS [lb,x,rb] foo
-- rn (NN (JSStatementList xs))                      foo = rJS xs foo
rn (NN (JSSwitch s lb x rb x2))                   foo = rJS ([s,lb,x,rb,x2]) foo
rn (NN (JSThrow t x))                             foo = rJS [t,x] foo
rn (NN (JSTry t x1 x2s))                          foo = rJS ([t,x1]++x2s) foo
rn (NN (JSUnary _l n))                            foo = rJS [n] foo
rn (NN (JSVarDecl x1 x2s))                        foo = rJS ([x1]++x2s) foo
rn (NN (JSVariables n xs as))                     foo = rJS ([n]++xs++[as]) foo
rn (NN (JSWhile w lb x1 rb x2))                   foo = rJS [w,lb,x1,rb,x2] foo
rn (NN (JSWith w lb x1 rb x2s))                   foo = rJS ([w,lb,x1,rb]++x2s) foo

-- Debug helper
rn what foo = rs (show what) foo

--rn _ _ = undefined

-- ---------------------------------------------------------------------
-- Helper functions

-- ---------------------------------------------------------------------
-- Need a function that
-- a) renders all comments, according to their positions
-- b) advances to the position of the required string
-- c) renders the string, advancing the position
rcs :: [CommentAnnotation] -> TokenPosn -> String -> Foo -> Foo
rcs cs p s foo = rps p s (rc cs foo)

rc :: [CommentAnnotation] -> Foo -> Foo
rc cs foo = foldl' go foo cs
  where
    go :: Foo -> CommentAnnotation -> Foo
    go bar NoComment = bar
    go bar (CommentA   p s) = rps p s bar
    go bar (WhiteSpace p s) = rps p s bar

-- Render a string at the given position
rps :: TokenPosn -> String -> Foo -> Foo
rps p s foo = (rs s foo')
  where
    foo' = (goto p foo)

-- Render a string
rs :: String -> Foo -> Foo
rs s (Foo (r,c) bb) = (Foo (r',c') (bb <> (text s)))
  where
    (r',c') = foldl' (\(row,col) ch -> go (row,col) ch) (r,c) s

    go (r'',_)   '\n' = (r''+1,1)
    go (r'',c'') '\t' = (r'',c''+8)
    go (r'',c'') _    = (r'',c''+1)


goto :: TokenPosn -> Foo -> Foo
goto (TokenPn _ ltgt ctgt) (Foo (lcur,ccur) bb) = (Foo (lnew,cnew) (bb <> bb'))
-- goto (TokenPn _ ltgt ctgt) (Foo (lcur,ccur) bb) = trace ("goto " ++ (show $ (ltgt,ctgt)) ++ "," ++ (show $ (lcur,ccur)) ++ "," ++ (show $ (lnew,cnew)) ) $  (Foo (lnew,cnew) (bb <> bb'))
  where
    (bbline,ccur') = if (lcur < ltgt) then (text $ (take (ltgt - lcur) $ repeat '\n'),1) else (mempty,ccur)
    bbcol  = if (ccur' < ctgt) then (text $ take (ctgt - ccur') $ repeat ' ' ) else mempty
    bb' = bbline <> bbcol
    lnew = if (lcur < ltgt) then ltgt else lcur
    cnew = if (ccur' < ctgt) then ctgt else ccur'


rJS :: [JSNode] -> Foo -> Foo
rJS xs foo = foldl' (flip rn) foo xs

renderToString :: JSNode -> String
-- need to be careful to not lose the unicode encoding on output
renderToString js = US.decode $ LB.unpack $ BB.toLazyByteString $ renderJS js


-- ---------------------------------------------------------------------
-- Test stuff

_r :: JSNode -> String
_r js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

_t :: String -> String
_t str = _r $ readJs str


-- readJs "/*a*/x"
_ax :: JSNode
_ax = (NN
     (JSExpression
       [NT
        (JSIdentifier "x")
        (TokenPn 5 1 6)
        [CommentA (TokenPn 0 1 1) "/*a*/"]])
      )


-- readJs "//j\nthis_"
-- NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier "this_") (TokenPn 4 2 1) [CommentA (TokenPn 0 1 1) "//j"]]) (TokenPn 4 2 1) []]) (TokenPn 4 2 1) []

_r1 :: JSNode
_r1 = NN
      (
        JSExpression
          [
            NT
              (JSIdentifier "this_")
              (TokenPn 4 2 1)
              [CommentA (TokenPn 0 1 1) "//j"]
          ])


-- EOF

