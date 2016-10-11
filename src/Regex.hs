{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Regex where

import Test.QuickCheck

--import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
--import Text.XML.Light.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.DeriveTH
import DeriveArbitrary

import ByteString
import Strings

import Data.Char (chr)

import Data.List(intersperse,partition)
import qualified Data.Set as Set(toAscList,toList)
import Data.Set(Set)

--instance {-# OVERLAPPING #-} Arbitrary String where
--   arbitrary = mgenName

-- taken from regex-parsec

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
        byRange _ = undefined
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

type MRegex = Pattern

mencode :: MRegex -> LC8.ByteString
mencode x = LC8.pack $ "/" ++ showPattern x ++ "/"
