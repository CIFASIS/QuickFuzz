{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, TupleSections, OverloadedStrings #-}

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
import Text.Blaze.Html5.Attributes as HAtt
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

instance Arbitrary AttributeValue where
    arbitrary = do
        t <- arbitrary
        return $ stringValue t

instance Arbitrary Attribute where
    arbitrary = do
        attr <- arbitrary
        oneof $ Prelude.map (\f -> return $ f attr)
            [ alt, src, accept, acceptCharset
            , accesskey, action, async, autocomplete
            , autofocus, autoplay, challenge, charset, checked
            , HAtt.cite, class_, cols, colspan, content, contenteditable
            , contextmenu, controls, coords, data_, HAtt.datetime, defer
            , dir, disabled, draggable, enctype, for, HAtt.form, formaction
            , formenctype, formmethod, formnovalidate, formtarget, headers
            , height, hidden, high, href, hreflang, httpEquiv, icon, HAtt.id
            , ismap, item, itemprop, itemscope, itemtype, keytype, HAtt.label
            , lang, list, HAtt.loop, low, manifest, HAtt.max, maxlength, media, method
            , HAtt.min, multiple, name, novalidate, onbeforeonload, onbeforeprint
            , onblur, oncanplay, oncanplaythrough, onchange, onclick
            , oncontextmenu, ondblclick, ondrag, ondragend, ondragenter
            , ondragleave, ondragover, ondragstart, ondrop, ondurationchange
            , onemptied, onended, onerror, onfocus, onformchange, onforminput
            , onhaschange, oninput, oninvalid, onkeydown, onkeyup, onload
            , onloadeddata, onloadedmetadata, onloadstart, onmessage
            , onmousedown, onmousemove, onmouseout, onmouseover, onmouseup
            , onmousewheel, ononline, onpagehide, onpageshow, onpause, onplay
            , onplaying, onprogress, onpropstate, onratechange
            , onreadystatechange, onredo, onresize, onscroll, onseeked
            , onseeking, onselect, onstalled, onstorage, onsubmit, onsuspend
            , ontimeupdate, onundo, onunload, onvolumechange, onwaiting
            , open, optimum, pattern, ping, placeholder, preload, pubdate
            , radiogroup, readonly, rel, required, reversed, rows, rowspan
            , sandbox, scope, scoped, seamless, selected, shape, size, sizes
            , HAtt.span, spellcheck, srcdoc, start, step, HAtt.style, subject, HAtt.summary
            , tabindex, target, HAtt.title, type_, usemap, value, width, wrap
            , xmlns
            ]

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
        att1 <- (arbitrary :: Gen Attribute)
        --oneof $
        frequency $ 
                ( (15, return $ att ! att1) :
                   (Prelude.map ((10,) . return)
                       [ 
                       --att ! att1
                        link
                       , source 
                       , meta
                       , hr, img
                       ])
                   ++ (Prelude.map (\f -> (10, return $ f ht))
                       [ iframe
                       , a, figure
                       , article, ol, tr, ul
                       , H5.form, h1, h2, h3, h4, h5, h6
                       , body
                       , tbody, td, textarea, tfoot, th, thead, time, H5.title
                       ])
                   ++ (Prelude.map ((1,) . return)
                       [ docType
                       , area 
                       , base
                       , br, col, embed, input
                       , keygen, menuitem
                       , param, track, wbr
                       ])
                   ++ (Prelude.map (\f -> (1, return $ f ht))
                       [ docTypeHtml 
                       , abbr 
                       , address 
                       , aside 
                       , audio 
                       , b 
                       , bdo 
                       , blockquote 
                       , button
                       , canvas , caption, H5.cite, code
                       , colgroup, command, datalist
                       , dd, del, details, dfn, H5.div
                       , dl, dt, em, fieldset
                       , figcaption, footer
                       , H5.head, header, hgroup, html, i
                       , ins, kbd, H5.label, legend
                       , main, H5.map, mark, menu
                       , meter, nav, noscript, object
                       , optgroup, option, H5.output, p, pre
                       , progress, q, rp, rt, ruby, samp, script
                       , section, select, small, H5.span, strong
                       , H5.style, sub, H5.summary, sup, table
                       , var, video             
                       ]))

mencode :: Html -> LC8.ByteString
mencode x = LC8.pack $ renderHtml x
