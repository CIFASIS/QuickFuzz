module Process where

import Args
import Test.QuickCheck
import Check
import Data.List.Split
import Data.ByteString.Lazy 

process :: (Show a, Arbitrary a) => (a -> ByteString) -> Bool -> FilePath -> String -> String -> Int -> Int -> FilePath -> IO Result 
process mencode par filename cmd prop maxSuccess maxSize outdir =
    let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    in (case prop of
        "zzuf" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ zzufprop filename prog args mencode outdir)
        "radamsa" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ radamprop filename prog args mencode outdir)
        "check" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ genprop filename prog args mencode outdir)
        "exec" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ execprop filename prog args mencode outdir)
        --"serve" -> 
            --quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            --stdArgs {chatty = True, maxSuccess = maxSuccess , maxSize = maxSize } 
            --(noShrinking $ serveprop filename 8099 [] mencode)

        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

_main mencode (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = process mencode b filename cmd prop maxSuccess maxSize outdir 

main mencode fargs False = (\x -> (_main mencode x) >> return ()) $ fargs ""
main mencode fargs True  = processPar fargs (\x -> (_main mencode x) >> return ())


