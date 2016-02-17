module Process where

import Args
import Test.QuickCheck
import Check
import Data.List.Split
import Data.ByteString.Lazy 
import Network.Socket

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
        "honggfuzz" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ honggprop filename prog args mencode outdir)

        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

_main mencode (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = process mencode b filename cmd prop maxSuccess maxSize outdir 

main mencode fargs False = (\x -> (_main mencode x) >> return ()) $ fargs ""
main mencode fargs True  = processPar fargs (\x -> (_main mencode x) >> return ())



--netprocess :: (Show a, Arbitrary a) => (a -> ByteString) -> Bool -> FilePath -> String -> String -> Int -> Int -> FilePath -> IO Result 
netprocess mencode par _ host prop maxSuccess maxSize outdir =
    --let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    case prop of
        "serve" -> 
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ serveprop 6055 [] mencode)

        "connect" -> 
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ cconnectprop 6055 host mencode)


        _     -> error "Invalid action selected"

_netmain mencode (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = netprocess mencode b filename cmd prop maxSuccess maxSize outdir 

netmain mencode fargs False = (\x -> (_netmain mencode x) >> return ()) $ fargs ""
netmain mencode fargs True  = processPar fargs (\x -> (_netmain mencode x) >> return ())


