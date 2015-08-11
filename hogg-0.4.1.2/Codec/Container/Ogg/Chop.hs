--
-- Module      : Chop
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Chop (
  chop,
  chopWithSkel
) where

import Control.Monad.Identity
import Control.Monad.State

import qualified Data.ByteString.Lazy as L
import Data.List
import Data.Maybe

import Codec.Container.Ogg.Chain
import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.List
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Skeleton
import Codec.Container.Ogg.Timestamp
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- ChopState
--

type ChopState = [ChopTrackState]

data ChopTrackState =
  ChopTrackState {
    ctsTrack :: OggTrack,
    ctsBOS :: [OggPage],
    ctsHdrs :: [OggPage],
    
    headersRemaining :: Int,

    ctsStartgranule :: Granulepos,

    -- Greatest previously inferred keyframe value
    prevK :: Integer,

    -- Just to spice things up (and simplify the algorithm)
    -- the page accumulator is kept in reverse order
    pageAccum :: [OggPage],

    -- Whether or not this track has delivered beyond the chop end
    ended :: Bool
  }

-- An initial ChopState (used for a chop without adding skeleton)
emptyChopState :: ChopState
emptyChopState = []

-- Initial state for a new track
newChopTrackState :: OggTrack -> ChopTrackState
newChopTrackState t = ChopTrackState t [] [] 0 (Granulepos Nothing) 0 [] False

------------------------------------------------------------
-- Chop monad
--

type Chop a = (StateT ChopState Identity) a

-- | Run the Chop monad
runChop :: ChopState -> Chop a -> (a, ChopState)
runChop st x = runIdentity (runStateT x st)

------------------------------------------------------------
-- Chop functions
--

-- | Chop a bitstream, do NOT add a Skeleton bitstream
-- | chop start end chain
chop :: Maybe Timestamp -> Maybe Timestamp -> OggChain -> IO OggChain
chop start end chain =
  return $ fst $ runChop emptyChopState (chopTop start end chain)

-- | Chop a bitstream, adding a Skeleton bitstream
-- | chopWithSkel start end chain
chopWithSkel :: Maybe Timestamp -> Maybe Timestamp -> OggChain -> IO OggChain
chopWithSkel start end chain = case hasSkel of
  True  -> do
    return $ fst $ runChop emptyChopState (chopTop start end chain)
  False -> do
    -- Construct a new track for the Skeleton
    s <- genSerial
    let skelTrack = (newTrack s){trackType = Just skeleton}
        st = [newChopTrackState skelTrack]
    return $ fst $ runChop st (chopTop start end chain)
  where
    hasSkel = any (contentTypeIs skeleton) (chainTracks chain)

-- | Top-level bitstream chopper -- handles headers
chopTop :: Maybe Timestamp -> Maybe Timestamp -> OggChain -> Chop OggChain
chopTop mStart mEnd (OggChain tracks pages _) = do
  pages' <- chopTop' mStart mEnd pages
  let packets' = pagesToPackets pages' 
  return $ OggChain tracks pages' packets'

chopTop' :: Maybe Timestamp -> Maybe Timestamp -> [OggPage] -> Chop [OggPage]
chopTop' _ _ [] = return []
chopTop' Nothing Nothing gs = return gs
chopTop' Nothing mEnd@(Just _) gs = chopTo mEnd gs
chopTop' (Just start) mEnd (g:gs)
  | pageBOS g = do
    pushBOS g -- Remember this BOS page
    addHeaders g -- Add the number of headers for this track
    subHeaders g -- Subtract the number contained in this page
    chopTop' (Just start) mEnd gs
  | otherwise = do
    p <- doneHeaders
    case p of
      False -> do
        subHeaders g -- Subtract the number contained in this page
        pushHdr g -- Remember this header
        chopTop' (Just start) mEnd gs
      True  -> chopRaw (Just start) mEnd (g:gs)

-- | Raw bitstream chopper -- after headers
chopRaw :: Maybe Timestamp -> Maybe Timestamp -> [OggPage] -> Chop [OggPage]
chopRaw _ _ [] = return []
chopRaw Nothing Nothing gs = return gs
chopRaw Nothing mEnd@(Just _) gs = chopTo mEnd gs
chopRaw (Just start) mEnd (g:gs) = case (timestampOf g) of
  Nothing -> do
    -- Add this page to accum buffer
    chopAccum g
    return g >> (chopRaw (Just start) mEnd gs)
  (Just gTime) -> do
    p <- changedK g
    case p of
      False -> do
        -- Add this page to accum buffer
        chopAccum g
      True -> do
        pruneAccum g
        setK g
        -- Add this page to accum buffer
        chopAccum g
    case (compare start gTime) of
      LT -> do
        -- Prepend Control section
        ctrl <- chopCtrl (Just start)
        cs <- chopRaw Nothing mEnd gs
        return $ ctrl ++ cs
      _  -> do
        chopRaw (Just start) mEnd gs

-- | Dump the control section
chopCtrl :: Maybe Timestamp -> Chop [OggPage]
chopCtrl mStart = do
    l <- get
    let skelTrack = ctsTrack $ head l
        haveSkel = (trackType skelTrack == Just skeleton)
        presentation = fromMaybe zeroTimestamp mStart
        base = zeroTimestamp
        fh = fisheadToPage skelTrack $ OggFishead presentation base
        fbs = Data.Maybe.mapMaybe (chopFisbone skelTrack) (tail l)
        -- Generate an EOS page for the Skeleton track
        sEOS = (uncutPage L.empty skelTrack sEOSgp){pageEOS = True}
        sEOSgp = Granulepos (Just 0)

    boss <- popBOSs
    hdrs <- popHdrs
    -- Include accum buffer in bitstream
    as <- getAccum
    case haveSkel of
      True -> return $ [fh] ++ boss ++ fbs ++ hdrs ++ [sEOS] ++ as
      False -> return $ boss ++ hdrs ++ as

-- | Create a Fisbone page out of a ChopTrackState
chopFisbone :: OggTrack -> ChopTrackState -> Maybe OggPage
chopFisbone skelTrack cts = do
    fb <- trackToFisbone $ ctsTrack cts
    let fb' = fb{fisboneStartgranule = gpUnpack $ ctsStartgranule cts}
    return $ fisboneToPage skelTrack fb'

-- | Chop to the specified end time
chopTo :: Maybe Timestamp -> [OggPage] -> Chop [OggPage]
chopTo _ [] = return []
chopTo mEnd (g:gs)
  | before mEnd g = do
      cs <- chopTo mEnd gs
      return $ g : cs
  | otherwise = chopEnd mEnd (g:gs)

-- | Handle last pages of all tracks
chopEnd :: Maybe Timestamp -> [OggPage] -> Chop [OggPage]
chopEnd _ [] = return []
chopEnd mEnd (g:gs) = do
    ts <- findState g
    case (ended ts) of
      True -> do
        isEnded <- allEnded
        case isEnded of
          True -> return []
          False -> chopEnd mEnd gs
      False -> do
        replState ts{ended = True}
        cs <- chopEnd mEnd gs
        return $ g{pageEOS = True} : cs

-- | Find the ChopTrackState associated with this OggPage (indexed by track)
findState :: OggPage -> Chop ChopTrackState
findState g = do
    l <- get
    let t = pageTrack g
        mSt = find (\x -> ctsTrack x == t) l
    return $ fromMaybe (newChopTrackState t) mSt

-- | Replace a ChopTrackState (only if already existing)
replState :: ChopTrackState -> Chop ()
replState st = do
    l <- get
    let l' = foldr (\x -> if (sameTrack x) then (:) st else (:) x) [] l
    put l'
  where
    sameTrack x = (ctsTrack x == ctsTrack st)

-- | Push a Beginning-Of-Stream page onto ChopState
pushBOS :: OggPage -> Chop ()
pushBOS g = do
    l <- get
    let st = (newChopTrackState t){ctsBOS = [g]}
        l' = l ++ [st]
    put l'
  where
    t = pageTrack g

-- | Push a header page onto ChopState
pushHdr :: OggPage -> Chop ()
pushHdr g = do
    ts <- findState g
    let hdrs = ctsHdrs ts
        h' = hdrs++[g]
    replState ts{ctsHdrs = h'}

-- | Pop all Beginning-Of-Stream pages from ChopState
popBOSs :: Chop [OggPage]
popBOSs = popPages ctsBOS

-- | Pop all header pages from ChopState
popHdrs :: Chop [OggPage]
popHdrs = popPages ctsHdrs

-- | Generic page popper for BOS, headers
popPages :: (ChopTrackState -> [OggPage]) -> Chop [OggPage]
popPages f = do
    l <- get
    let gs = foldr (\a b -> (f a)++b) [] l
    return $ filter (not . contentTypeIs skeleton) gs

-- | Determine whether all tracks are ended
allEnded :: Chop Bool
allEnded = do
    l <- get
    return $ all ended l

-- | Get prevK for a given track
getK :: OggPage -> Chop Integer
getK g = do
  ts <- findState g
  let c = prevK ts
  return c

-- | Set prevK for a given track
setK :: OggPage -> Chop ()
setK g = case (pageGranulepos g) of
  Granulepos Nothing -> return ()
  _ -> do
    let k = fromJust $ pageKeyGranule g
    ts <- findState g
    replState ts{prevK = k}

-- | Has the K part of the granulepos changed?
changedK :: OggPage -> Chop Bool
changedK g = case (pageGranulepos g) of
  Granulepos Nothing -> return False
  _ -> do
    c <- getK g
    let k = fromJust $ pageKeyGranule g
    return (k /= c)

-- | Accumulate a page
chopAccum :: OggPage -> Chop ()
chopAccum g = case (trackGranuleshift t) of
    Nothing -> return ()
    _ -> do
      ts <- findState g
      let gs = pageAccum ts
      replState ts{pageAccum = (g:gs)}
  where 
    t = pageTrack g

-- | Prune accumulated pages
pruneAccum :: OggPage -> Chop ()
pruneAccum g = case (trackGranuleshift t) of
    Nothing -> return ()
    _ -> do
      k <- getK g
      ts <- findState g
      let ts' = pruneTrackAccum g k ts
      replState ts'
  where 
    t = pageTrack g

pruneTrackAccum :: OggPage -> Integer -> ChopTrackState -> ChopTrackState
pruneTrackAccum g k ts = ts{pageAccum = g:gs, ctsStartgranule = sg}
  where
    as = pageAccum ts
    t = pageTrack g
    (gs, sgs) = spanB later as
    sg = gpOfHead sgs
    gpOfHead [] = Granulepos Nothing
    gpOfHead (x:_) = pageGranulepos x
    later x = case (pageGranulepos x) of
      Granulepos Nothing -> True
      _ -> (fromJust $ gpToGranules (pageGranulepos x) t) >= k

-- | get accumulated pages
getAccum :: Chop [OggPage]
getAccum = do
  l <- get
  let accums = foldr (\x b -> (reverse . pageAccum) x : b) [] l
      as = listMerge accums
  return as

-- | Add the total number of headers that this track expects
addHeaders :: OggPage -> Chop ()
addHeaders g = do
  let t = pageTrack g
      h = trackHeaders t
  modifyHeaders g h

-- | Subtract the number of completed header packets provided by this page
subHeaders :: OggPage -> Chop ()
subHeaders g = do
  let segs = length $ pageSegments g
      incmplt = pageIncomplete g
      n = if incmplt then (segs-1) else segs
  modifyHeaders g (-n)

-- | State modifier to change the number of headers remaining
modifyHeaders :: OggPage -> Int -> Chop ()
modifyHeaders g n = do
  ts <- findState g
  let r = headersRemaining ts
  replState ts{headersRemaining = r + n}

-- | Determine whether all tracks have no headers remaining
doneHeaders :: Chop Bool
doneHeaders = do
  l <- get
  return $ foldr (\t b -> (headersRemaining t <= 0) && b) True l

-- | a version of span that includes the first bounding failure
spanB :: (a -> Bool) -> [a] -> ([a], [a])
spanB _ [] = ([],[])
spanB p (x:xs) 
    | p x       =  (x:ys,zs) 
    | otherwise =  ([x],xs)
                   where (ys,zs) = spanB p xs
