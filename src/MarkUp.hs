{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, TupleSections, OverloadedStrings #-}

module MarkUp where

import Args
import Test.QuickCheck
import Check

import qualified Text.Blaze.Renderer.XmlHtml as XML
import qualified Text.XmlHtml as RXML
import qualified Blaze.ByteString.Builder as PB
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )


import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.Blaze.Internal
import Text.Blaze.Html
import Text.Blaze.Html5 as H5
import Text.Blaze.Html5.Attributes as HAtt
import Text.Blaze.Html.Renderer.Pretty

import Network.URI
import Data.List           (intercalate)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (pack, Text(..) )
import Data.Char (chr)


genWord :: Gen String
genWord = vectorOf 8 (choose ('a', 'z'))

genCanonicalURI :: Gen URI
genCanonicalURI =
    URI <$> elements ["http:", "https:"]
        <*> (Just <$> genURIAuthority)
        <*> (('/':) <$> genPaths)
        <*> pure ""
        <*> pure ""
  where
    genURIAuthority =
      URIAuth <$> pure ""
              <*> genRegName
              <*> pure ""
    genRegName = do
      domainName <- elements ["noomii", "google", "yahoo"]
      return $ Prelude.concat ["www.", domainName, ".com"]
    genPaths = resize 10 (intercalate "/" <$> vectorOf 2 genWord)

genNormalURI :: URI -> Gen URI
genNormalURI uri = do
    qs  <- genQueryString
    fragment <-  genFragment
    return $ uri { uriQuery = qs, uriFragment = fragment }
  where
    genParam = do
      name  <- genWord
      value <- genWord
      return $ name ++ "=" ++ value
    genQueryString = resize 10 $
      ('?':) <$> (intercalate "&" <$> vectorOf 2 genParam)
    genFragment = ('#':) <$> genWord

instance Arbitrary URI where
  arbitrary = do
    uri <- genCanonicalURI
    genNormalURI uri

--derive makeArbitrary ''URI
--derive makeArbitrary ''URIAuth

instance Show Markup where
    show = renderHtml

genName :: Gen String
genName = vectorOf 8 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)
--genName = return "a"

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

instance Arbitrary Tag where
    arbitrary = do
        t <- arbitrary
        return $ stringTag t

instance Arbitrary AttributeValue where
    arbitrary = do
        s <- arbitrary :: Gen String
        n <- arbitrary :: Gen Int
        b <- arbitrary :: Gen Bool
        f <- arbitrary :: Gen Float
        u <- arbitrary :: Gen URI
        oneof $ Prelude.map return [toValue s, toValue n, toValue b, toValue f, toValue (show u)]
        --oneof $ Prelude.map return [toValue (show u)]

instance Arbitrary Attribute where
    arbitrary = do
        attr <- arbitrary
        tag1 <- arbitrary
        tag2 <- arbitrary
        oneof $
            ( Prelude.map return
                [
                 attribute tag1 tag2 attr
                , dataAttribute tag1 attr
                , customAttribute tag1 attr
                ]
            ++
             Prelude.map (\f -> return $ f attr)
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
            )

instance Arbitrary ChoiceString where
    arbitrary = do
        st <- arbitrary
        str <- genName
        bstr <- genName
        tx <- genText
        --bt <- arbitrary
        oneof $ Prelude.map return [Static st, String str, Text tx, ByteString (C8.pack bstr)]

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
        --att <- arbitrary
        att1 <- (arbitrary :: Gen Attribute)
        tag <- arbitrary
        boop <- arbitrary
        frequency $
                Prelude.map (\x -> (1, return $ x ! att1))
 
                ( 
                   
                       [ 
                        link
                       , source 
                       , meta
                       , hr, img
                       ]
                    ++
                    [customParent tag ht, customLeaf tag boop]                 
                   ++ (Prelude.map (\f -> f ht)
                       [ iframe
                       , a, figure
                       , article, ol, tr, ul
                       , H5.form, h1, h2, h3, h4, h5, h6
                       , body
                       , tbody, td, textarea, tfoot, th, thead, time, H5.title
                       ])
                   ++ 
                       [ docType
                       , area 
                       , base
                       , br, col, embed, input
                       , keygen, menuitem
                       , param, track, wbr
                       ]
                   ++ (Prelude.map (\f -> f ht)
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


type MHtml = (NonEmptyList Html)

merge (x:[]) = x
merge (x:xs) = do 
                 _ <- x
                 merge xs


mencodeHtml :: MHtml -> LC8.ByteString
mencodeHtml (NonEmpty xs) = LC8.pack $ renderHtml $ merge xs

mencodeXml :: MHtml -> LB.ByteString
mencodeXml (NonEmpty xs) = PB.toLazyByteString $ RXML.render $ XML.renderHtml $ merge xs
