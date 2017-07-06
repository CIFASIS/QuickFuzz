module Test.QuickFuzz.Derive.Mutators where

import Test.QuickCheck

insertAt :: Int -> a -> [a] -> [a] 
insertAt idx y xs = as ++ (y:bs)
                  where (as,bs) = splitAt idx xs

expander :: (Arbitrary a) => [a] -> Gen [a]
expander [] = do
               y <- arbitrary
               return $ [y]
expander xs = do
          idx <- arbitrary :: Gen Int
          y   <- arbitrary 
          let idx'  = mod (abs idx) (length xs)
          return $ insertAt idx' y xs

deleteAt :: Int -> [a] -> [a]
deleteAt _ []   = []
deleteAt idx xs = ys ++ (tail zs)
                  where (ys,zs) = splitAt idx xs   

deleter :: [a] -> Gen [a]
deleter [] = return []
deleter xs = do
          idx <- arbitrary :: Gen Int 
          let idx' = mod (abs idx) (length xs)
          return $ deleteAt idx' xs


-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = let elemI = xs !! i
                    elemJ = xs !! j
                    left = take i xs
                    middle = take (j - i - 1) (drop (i + 1) xs)
                    right = drop (j + 1) xs
                in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

swaper ::  [a] -> Gen [a]
swaper [] = return []
swaper [x] = return [x]
swaper xs = do
          idx0 <- arbitrary :: Gen Int 
          idx1 <- arbitrary :: Gen Int
          let idx0'  = mod (abs idx0) (length xs)
              idx1'  = mod (abs idx1) (length xs)
          return $ swapAt idx0' idx1' xs

repeatAt :: Int -> Int -> [a] -> [a] 
repeatAt _ _ [] = []
repeatAt idx size xs = as ++ take size (repeat b) ++ (bs)
                  where (as,b:bs) = splitAt idx xs

magic = 42
maxsize = 100

repeater ::  [a] -> Gen [a]
repeater [] = return []
repeater xs = do
          size <- arbitrary :: Gen Int
          idx <- arbitrary  :: Gen Int
          let size' = mod (magic*(abs size)) maxsize
              idx'  = mod (abs idx) (length xs)
          return $ repeatAt idx' size' xs
