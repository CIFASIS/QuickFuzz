module Main where

import Test.HUnit
import Codec.Container.Ogg.List

list0 :: [[Int]]
list0 = []

list0Sorted :: [Int]
list0Sorted = []

list0Test =
  TestCase $ assertEqual "listMerge of empty list" list0Sorted (listMerge list0)

list1 :: [[Int]]
list1 = [[2,4,6],[1,5,9],[3,7,8]]

list1Sorted :: [Int]
list1Sorted = [1..9]

list1Test =
  TestCase $ assertEqual "listMerge of list1" list1Sorted (listMerge list1)

tests = TestList
  [ TestLabel "empty list" list0Test
  , TestLabel "test1" list1Test
  ]

main = runTestTT tests
