module Fuzzers where

data Fuzzer = Zzuf | Radamsa deriving Show
type FuzzerCommand = Int -> (String, [String])

defaultZzuf :: FuzzerCommand
defaultZzuf seed = ("zzuf", ["-r", "0.000001:0.04", "-s", show seed])

defaultRadamsa :: FuzzerCommand
defaultRadamsa seed = ("radamsa", ["-s", show seed])

-- | Fuzzers to raw commands mapping
getFuzzerCommand :: Fuzzer -> FuzzerCommand
getFuzzerCommand Zzuf = defaultZzuf
getFuzzerCommand Radamsa = defaultRadamsa
