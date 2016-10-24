{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module PS where

import Test.QuickCheck
import Data.CG.Minus 

--import Data.Binary( Binary(..), encode )
import Graphics.PS.PS
import Graphics.PS.GS
import Graphics.PS.Image
import Graphics.PS.Paper
import Graphics.PS.Path
import Graphics.PS.Glyph
import Graphics.PS.Font

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
--import Data.DeriveTH

import DeriveArbitrary
import DeriveMArbitrary

import ByteString
import Vector
import Strings

type MPS  = (String, Paper,[Image])

instance Arbitrary String where
   arbitrary = mgenName

--instance Arbitrary a => Arbitrary (Pt a) where
--   arbitrary = do 
--     (x,y) <- arbitrary
--     return $ Pt x y

instance Arbitrary (Matrix Double) where
   arbitrary = do 
     (n1,n2,n3,n4,n5,n6) <- arbitrary
     return $ Matrix n1 n2 n3 n4 n5 n6

$(devActions ["Graphics.PS.Paper"] ''Paper False [])
$(devArbitrary ''PaperAction)
$(devArbitraryWithActions False ''Paper)


$(devActions ["Graphics.PS.Path"] ''Path False [])
$(devArbitrary ''PathAction)
$(devArbitraryWithActions False ''Path)

$(devArbitrary ''GS)

-- instance Arbitrary Image where
--       arbitrary
--         = sized go_aJ10
--         where
--             go_aJ10 n_aJ11
--               = if (n_aJ11 <= 1) then
--                     oneof
--                       [Stroke <$> resize (max 0 (n_aJ11 - 1)) arbitrary
--                        <*> resize (max 0 (n_aJ11 - 1)) arbitrary,
--                        Fill <$> resize (max 0 (n_aJ11 - 1)) arbitrary
--                        <*> resize (max 0 (n_aJ11 - 1)) arbitrary,
--                        return Empty]
--                 else
--                     oneof
--                       [Stroke <$> resize (max 0 (n_aJ11 - 1)) arbitrary
--                        <*> resize (max 0 (n_aJ11 - 1)) arbitrary,
--                        Fill <$> resize (max 0 (n_aJ11 - 1)) arbitrary
--                        <*> resize (max 0 (n_aJ11 - 1)) arbitrary,
--                        ITransform <$> resize (max 0 (n_aJ11 - 1)) arbitrary
--                        <*> (go_aJ10 $ (n_aJ11 - 1)),
--                        Over <$> (go_aJ10 $ (n_aJ11 `div` 2))
--                        <*> (go_aJ10 $ (n_aJ11 `div` 2)),
--                        return Empty]
-- 
mencode :: MPS -> LC8.ByteString
mencode (title, paper,imgs) = LC8.pack $ stringFromPS title paper imgs

