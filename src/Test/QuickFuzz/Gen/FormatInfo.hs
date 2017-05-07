{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Test.QuickFuzz.Gen.FormatInfo where

import Data.Default
import Data.Maybe

import Data.ByteString.Lazy
import Test.QuickCheck (Gen)

data NoActions

instance (Show NoActions) where
    show _ = "<no-actions>"


-- | A data type describing the operations needed in order to manipulate
-- supported file formats values
data FormatInfo base actions = FormatInfo
        { decode  :: ByteString -> base 
        , encode  :: base -> ByteString
        , random  :: Gen base
        , value   :: base -> String
        , mutate  :: base -> Gen base
        , shrink  :: base -> [base]
        , actions :: Maybe (ActionInfo base actions)
        , ext     :: String 
        }

data ActionInfo base actions = ActionInfo
        { randomActions  :: Gen actions
        , shrinkActions  :: actions -> [actions]
        , performActions :: actions -> base
        , valueActions    :: actions -> String
        }


instance Default (FormatInfo base actions) where
    def = FormatInfo 
        { decode  = unsupported "decode"
        , encode  = unsupported "encode"
        , random  = unsupported "random"
        , value   = unsupported "value"
        , mutate  = unsupported "mutate"
        , shrink  = unsupported "shrink"
        , actions = Nothing
        , ext     = unsupported "ext"
        }

instance Default (ActionInfo base actions) where
    def = ActionInfo
        { randomActions  = unsupported "randomActions"
        , shrinkActions  = unsupported "shrinkActions"
        , performActions = unsupported "performActions"
        , valueActions   = unsupported "valueActions"
        }

hasActions :: FormatInfo base actions -> Bool
hasActions = isJust . actions

getActionInfo :: FormatInfo base actions -> ActionInfo base actions
getActionInfo fmt | hasActions fmt = fromJust (actions fmt)
                  | otherwise      = error "Trying to get ActionInfo for a unsupported format"

unsupported op = error $ "internal operation '" ++ op ++
                         "' is not supported for this file format." 
