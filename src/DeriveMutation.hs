{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List

--import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.Class as TC

import DeriveArbitrary

-- | Mutation Class
class Arbitrary a => Mutation a where
    mutt :: a -> Gen a -- ^ Given a value, mutate it.
    mut :: Gen a
    mutt = arbitrary


howm :: Con -> (Name, Int)
howm (NormalC n xs) = (n,length xs)
howm (RecC n xs) = (n,length xs)
howm (ForallC _ _ t) = howm t
howm (InfixC _ ) = error "not yet"

devMutation :: Name -> Q [Dec]
devMutation name = do
    def <- reify name
    case def of -- We need constructors...
        TyConI (DataD _ _ params constructors _) -> 
            let fnm = mkName $ "mutt."++name in
            return $ funD fnm $ foldl (\ p x ->
                     let (n,cant) = howm x
                     clouse [conP n (take cant as)] BODYQ DECQ) [] constructors
        -- TyConI (NewtypeD _ _ params con _) -> do
        
