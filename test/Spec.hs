module Main (main) where

import Test.QuickCheck                      (Gen, Arbitrary(..), choose, shrinkIntegral)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (Assertion,(@?=))
import Data.Coerce

import Data.Word
import Data.Function (on)
import Data.List (groupBy)
import Control.Monad
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace

import qualified Data.Heap.Mutable.ModelA as HeapA

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Heaps"
    [ testProperty "Model A Push Pop" heapPushPop
    , testProperty "Model A List" heapMatchesList
    ]
  ]

testElements :: Int
testElements = 6

newtype MyElement = MyElement { getMyElement :: Word16 }
  deriving (Show,Read,Eq,Ord)

instance Arbitrary MyElement where
  arbitrary = fmap MyElement (choose (0,fromIntegral testElements))
  shrink (MyElement a) = fmap MyElement $ filter (>= 0) $ shrinkIntegral a -- fmap MyElement (enumFromTo 0 (a - 1))

heapPushPop :: [(Char,MyElement)] -> Bool
heapPushPop xs =
  let res = runST $ do
        h <- HeapA.new testElements
        forM xs $ \(c,MyElement i) -> do
          HeapA.push c i h
          HeapA.pop h
   in sequence res == coerce (Just xs)

heapMatchesList :: [(Char,MyElement)] -> Bool
heapMatchesList xs' =
  let xs = coerce xs' :: [(Char,Word16)]
      xsSet = fmap (\(c,i) -> (c,Set.singleton i)) xs
      ys = Map.fromListWith Set.union xsSet
      listRes = Map.toList ys
      heapResOriginal = runST $ do
        h <- HeapA.new testElements
        HeapA.pushList xs h
        HeapA.toList h
      heapRes = id
        $ map (\xs@((c,_) : _) -> (c,Set.fromList (map snd xs)))
        $ groupBy ((==) `on` fst) heapResOriginal
  in heapRes == listRes

