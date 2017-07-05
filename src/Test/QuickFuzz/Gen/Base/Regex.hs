{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TemplateHaskell     #-}

module Test.QuickFuzz.Gen.Base.Regex where

import Control.DeepSeq
import Data.Default
import Data.Char (chr)
import Data.List (intersperse, partition, subsequences)
import Data.Set (Set)
import qualified Data.Set as Set (toAscList, toList)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Test.QuickCheck hiding (shrink)
import Test.QuickFuzz.Derive
import Test.QuickFuzz.Derive.Generator
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.FormatInfo


data Pattern = PEmpty | PCarat | PDollar
             | PGroup  PatternIndex Pattern
--             | PGroup' PatternIndex (Maybe PatternIndex) Pattern -- used in longest match
             | POr     [Pattern]
             | PConcat [Pattern]
             | PQuest  Pattern
             | PPlus   Pattern
             | PStar   Pattern
             | PBound  Int (Maybe Int) Pattern
             -- | PLazy indicates the pattern should find the shortest match first
             | PLazy   Pattern    -- non-greedy wrapper (for ?+*{} followed by ?)
             -- | PPossessive indicates the pattern can only find the longest match
             | PPossessive Pattern -- possessive modifier (for ?+*{} followed by +)
             | PDot               -- Any character (newline?) at all
             | PAny    PatternSet -- Square bracketed things
             | PAnyNot PatternSet -- Inverted square bracketed things
             | PEscape Char       -- Backslashed Character
             | PBack   PatternIndex -- Backslashed digits as natural number
             | PChar   Char       -- Specific Character
               -- After simplify / mergeCharToString, adjacent PChar are merge'd into PString
             | PString String
               deriving (Eq,Show)

showPattern :: Pattern -> String
showPattern pIn =
  case pIn of
    PEmpty -> "()"
    PCarat -> "^"
    PDollar -> "$"
    PGroup _ p -> ('(':showPattern p)++")"
    POr ps -> concat $ intersperse "|" (map showPattern ps)
    PConcat ps -> concatMap showPattern ps
    PQuest p -> (showPattern p)++"?"
    PPlus p -> (showPattern p)++"+"
    PStar p -> (showPattern p)++"*"
    PLazy p -> (showPattern p)++"?"
            -- | otherwise -> "<Cannot print PLazy of "++show p++">"
    PPossessive p -> (showPattern p)++"+"
                  -- | otherwise -> "<Cannot print PPossessive of "++show p++">"
    PBound i (Just j) p | i==j -> showPattern p ++ ('{':show i)++"}"
    PBound i mj p -> showPattern p ++ ('{':show i) ++ maybe ",}" (\j -> ',':show j++"}") mj
    PDot -> "."
    PAny (PatternSet s scc sce sec) ->
        let (special,normal) = maybe ("","") ((partition (`elem` "]-")) . Set.toAscList) s
            charSpec = (if ']' `elem` special then (']':) else id) (byRange normal)
            scc' = maybe "" ((concatMap (\ss -> "[:"++unSCC ss++":]")) . Set.toList) scc
            sce' = maybe "" ((concatMap (\ss -> "[."++unSCE ss++".]")) . Set.toList) sce
            sec' = maybe "" ((concatMap (\ss -> "[="++unSEC ss++"=]")) . Set.toList) sec
        in concat ['[':charSpec,scc',sce',sec',if '-' `elem` special then "-]" else "]"]
    PAnyNot (PatternSet s scc sce sec) ->
        let (special,normal) = maybe ("","") ((partition (`elem` "]-")) . Set.toAscList) s
            charSpec = (if ']' `elem` special then (']':) else id) (byRange normal)
            scc' = maybe "" ((concatMap (\ss -> "[:"++unSCC ss++":]")) . Set.toList) scc
            sce' = maybe "" ((concatMap (\ss -> "[."++unSCE ss++".]")) . Set.toList) sce
            sec' = maybe "" ((concatMap (\ss -> "[="++unSEC ss++"=]")) . Set.toList) sec
        in concat ["[^",charSpec,scc',sce',sec',if '-' `elem` special then "-]" else "]"]
    PEscape c -> '\\':c:[]
    PBack i -> '\\':(show i)
    PChar c -> [c]
    PString s -> s
  where byRange xAll@(x:xs) | length xAll <=3 = xAll
                            | otherwise = groupRange x 1 xs
        byRange _ = ""
        groupRange x n (y:ys) = if (fromEnum y)-(fromEnum x) == n then groupRange x (succ n) ys
                                else (if n <=3 then take n [x..]
                                      else x:'-':(toEnum (pred n+fromEnum x)):[]) ++ groupRange y 1 ys
        groupRange x n [] = if n <=3 then take n [x..]
                            else x:'-':(toEnum (pred n+fromEnum x)):[]


data PatternSet = PatternSet (Maybe (Set Char))  (Maybe (Set (PatternSetCharacterClass)))
   (Maybe (Set PatternSetCollatingElement)) (Maybe (Set PatternSetEquivalenceClass)) deriving (Eq,Show)

newtype PatternSetCharacterClass   = PatternSetCharacterClass   {unSCC::String} deriving (Eq,Ord,Show) -- [: :]
newtype PatternSetCollatingElement = PatternSetCollatingElement {unSCE::String} deriving (Eq,Ord,Show) -- [. .]
newtype PatternSetEquivalenceClass = PatternSetEquivalenceClass {unSEC::String} deriving (Eq,Ord,Show) -- [= =]

-- | PatternIndex is for indexing submatches from  parenthesized groups (PGroup)
type PatternIndex = Int

-- helper function
isPostAtom :: Pattern -> Bool
isPostAtom p = case p of
                 PQuest _ -> True
                 PPlus _ -> True
                 PStar _ -> True
                 PBound _ _ _ -> True
                 _ -> False

$(devArbitrary ''Pattern)
$(devNFData ''Pattern)
$(devGenerator "genPattern" ''Pattern)

type Regex = Pattern


shrinkRegex :: Regex -> [Regex]
shrinkRegex PEmpty = []
shrinkRegex PCarat = []
shrinkRegex PDollar = []
shrinkRegex PDot = []
shrinkRegex x = [PEmpty, PCarat, PDollar, PDot] ++ shrinkRegex' x

shrinkRegex' PEmpty = [PEmpty]
shrinkRegex' PCarat = [PCarat]
shrinkRegex' PDollar = [PDollar]
shrinkRegex' PDot = [PDot]
shrinkRegex' (PGroup i p) = [p] ++ [PGroup i' p' | i' <- shrinkInt i, p' <- shrinkRegex' p]
shrinkRegex' (POr ps) = [POr ps'' | ps'' <- subsequences ps, ps' <- shrinkListOfPattern ps]
shrinkRegex' (PConcat ps) = [POr ps' | ps' <- shrinkListOfPattern ps]
shrinkRegex' (PQuest p) = [p] ++ [PQuest p' | p' <- shrinkRegex' p]
shrinkRegex' (PPlus p) = [p] ++ [PPlus p' | p' <- shrinkRegex' p]
shrinkRegex' (PStar p) = [p] ++[PStar p' | p' <- shrinkRegex' p]
shrinkRegex' (PBound i mi p) = [p] ++ [PBound i' mi' p'| i' <- shrinkInt i, mi' <- shrinkMaybe mi, p' <- shrinkRegex' p]
shrinkRegex' (PLazy p) = [p] ++[PLazy p' | p' <- shrinkRegex' p]
shrinkRegex' (PPossessive p) = [p] ++ [PPossessive p' | p' <- shrinkRegex' p]
shrinkRegex' (PAny pset) = [PAny pset' | pset' <- shrinkPatternSet pset]
shrinkRegex' (PAnyNot pset) = [PAnyNot pset' | pset' <- shrinkPatternSet pset]
shrinkRegex' (PEscape c) = [PEscape c' | c' <- shrinkChar c]
shrinkRegex' (PBack pi) = [PBack pi' | pi' <- shrinkPatternIndex pi]
shrinkRegex' (PChar c) = [PChar c' | c' <- shrinkChar c]
shrinkRegex' (PString str) = [PString str' | str' <- shrinkString str]


shrinkInt x = [x]
shrinkMaybe x = [x]
shrinkPatternSet x = [x]
shrinkPatternIndex x = [x]
shrinkChar c = [c]
shrinkString = subsequences
shrinkListOfPattern xs = map shrinkRegex' xs


mencode :: Regex -> LC8.ByteString
mencode x = LC8.pack $ "/" ++ showPattern x ++ "/"

regexInfo :: FormatInfo Regex NoActions
regexInfo = def
    { encode = mencode
    , random = arbitrary
    , value = show
    , shrink = shrinkRegex
    , ext = "regex"
    }
