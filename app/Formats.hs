{-# LANGUAGE TemplateHaskell, CPP #-}
module Formats 
( formats
) where

import Test.QuickFuzz.Gen.FormatInfo

-- Base
import Test.QuickFuzz.Gen.Base

-- Document
#ifdef DOCS
import Test.QuickFuzz.Gen.Document
#endif

-- Archives
#ifdef ARCHS
import Test.QuickFuzz.Gen.Archive
#endif

-- Media
#ifdef MEDIA
import Test.QuickFuzz.Gen.Media
#endif

-- Media
#ifdef IMAGE
import Test.QuickFuzz.Gen.Image
#endif

-- Code
#ifdef CODE
import Test.QuickFuzz.Gen.Code
#endif

-- PKI
#ifdef PKI
import Test.QuickFuzz.Gen.Pki
#endif

-- Network
#ifdef NET
import Test.QuickFuzz.Gen.Network
#endif


formats = [

    -- Archives 
#ifdef ARCHS
    ("tar", 'tarInfo),
    ("zip", 'zipInfo),

#endif

    -- Media
#ifdef MEDIA
    ("wav", 'wavInfo),
#endif

    -- Document
#ifdef DOCS
    ("html", 'htmlInfo),
    ("css", 'cssInfo),
    ("pdf", 'pdfInfo),
    ("ps", 'psInfo),
    ("eps", 'epsInfo),
    ("xml", 'xmlInfo),
#endif

    -- Image
#ifdef IMAGE
    ("svg", 'svgInfo),
    ("png", 'pngInfo),
    ("gif", 'gifInfo),
    ("tiff", 'tiffInfo),

#endif

    -- Source Code
#ifdef CODE
    ("c",  'cInfo),
    ("js", 'jsInfo),
    ("py", 'pyInfo),
    ("go", 'goInfo),
    ("lua", 'luaInfo),

#endif

    -- PKI
#ifdef PKI
    ("asn1", 'asn1Info),
    ("crl",  'crlInfo),
    ("x509", 'x509Info),

    -- lua

#endif


    -- Network
#ifdef NET
    ("http", 'httpResponseInfo),
#endif

    -- Base
    ("regex", 'regexInfo) 
    
    ]


