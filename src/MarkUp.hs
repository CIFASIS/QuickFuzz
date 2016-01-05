{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module MarkUp where

import Args
import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.Blaze.Internal
import Text.Blaze.Html
import Text.Blaze.Html5 as H5
import Text.Blaze.Html.Renderer.Pretty
import Data.Text
import Data.Char (chr)

instance Show Markup where
    show = renderHtml


genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

genText :: Gen Text
genText = do
    s <- genName
    return $ pack s

instance Arbitrary String where
    arbitrary = genName

instance Arbitrary StaticString where
    arbitrary = do
        ns <- genName
        t <- genText
        bt <- arbitrary 
        return $ StaticString (const ns) bt t

instance Arbitrary Attribute where
    arbitrary = do
        t <- arbitrary
        t1 <- arbitrary
        oneof $ Prelude.map return [ mempty
                , t1 `mappend` t ]

instance Arbitrary ChoiceString where
    arbitrary = do
        st <- arbitrary
        str <- genName
        tx <- genText
        bt <- arbitrary
        oneof $ Prelude.map return [Static st, String str, Text tx, ByteString bt]

instance Arbitrary (MarkupM a) where
    arbitrary = do
        s1 <- arbitrary
        s2 <- arbitrary
        s3 <- arbitrary
        ch <- arbitrary
        bo <- arbitrary
        b <- arbitrary
        c <- arbitrary
        oneof $ Prelude.map return 
            [ Parent s1 s2 s3 b
            , CustomParent ch b
            , Leaf s1 s2 s3
            , CustomLeaf ch bo
            , Content ch
            , Comment ch
            , Append b c
            , AddAttribute s1 s2 ch b
            , AddCustomAttribute ch ch b
            , Empty
            ]

basura :: Gen (MarkupM a)
basura = arbitrary

tags :: Gen Html
tags = arbitrary

instance Arbitrary Html where
    arbitrary = do
        ht <- oneof [tags , basura ]
        att <- arbitrary
        att1 <- arbitrary
        oneof $ (Prelude.map return
                    [ docType
                    , att ! att1
                    , area 
                    , base
                    , br, col, embed, hr, img, input
                    , keygen, link, menuitem, meta
                    , param, source, track, wbr
                    ])
                ++ (Prelude.map (\f -> return $ f ht)
                    [ docTypeHtml 
                    , a 
                    , abbr 
                    , address 
                    , article 
                    , aside 
                    , audio 
                    , b 
                    , bdo 
                    , blockquote 
                    , body
                    , button
                    , canvas , caption, cite, code
                    , colgroup, command, datalist
                    , dd, del, details, dfn, H5.div
                    , dl, dt, em, fieldset
                    , figcaption, figure, footer
                    , form, h1, h2, h3, h4, h5, h6
                    , H5.head, header, hgroup, html, i
                    , ins, kbd, H5.label, legend
                    , iframe, main, H5.map, mark, menu
                    , meter, nav, noscript, object, ol
                    , optgroup, option, H5.output, p, pre
                    , progress, q, rp, rt, ruby, samp, script
                    , section, select, small, H5.span, strong
                    , style, sub, summary, sup, table
                    , tbody, td, textarea, tfoot, th, thead, time, title
                    , tr, ul, var, video             
                    ]
                    )

mencode :: Html -> LC8.ByteString
mencode x = LC8.pack $ renderHtml x
