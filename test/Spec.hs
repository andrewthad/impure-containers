{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace

import qualified Data.Heap.Mutable.ModelD as HeapD

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Heaps"
    [ testProperty "Model D Push No Crash" multipush
    , testProperty "Model D Push Pop" heapPushPop
    , testProperty "Model D List" heapMatchesList
    ]
  ]

testElements :: Int
testElements = 15

newtype Min = Min { getMin :: Word32 }
  deriving (Show,Read,Eq,Ord)

instance Arbitrary Min where
  arbitrary = fmap Min (choose (0,20))
  shrink (Min a) = fmap Min $ filter (>= 0) $ shrinkIntegral a

instance Monoid Min where
  mempty = Min 0
  mappend (Min a) (Min b) = Min (min a b)

newtype MyElement = MyElement { getMyElement :: Int }
  deriving (Show,Read,Eq,Ord)

instance Arbitrary MyElement where
  arbitrary = fmap MyElement (choose (0,fromIntegral testElements - 1))
  shrink (MyElement a) = fmap MyElement $ filter (>= 0) $ shrinkIntegral a -- fmap MyElement (enumFromTo 0 (a - 1))

multipush :: [(Min,MyElement)] -> Bool
multipush xs = runST $ do
  h <- trace "Running Test" (HeapD.new testElements)
  HeapD.pushList (coerce xs :: [(Min,Int)]) h
  return True

heapPushPop :: [(Min,MyElement)] -> Bool
heapPushPop xs =
  let res = runST $ do
        h <- HeapD.new testElements
        forM xs $ \(c,MyElement i) -> do
          HeapD.push c i h
          HeapD.pop h
   in sequence res == coerce (Just xs)

heapMatchesList :: [(Min,MyElement)] -> Bool
heapMatchesList xs' =
  let xs = coerce xs' :: [(Min,Int)]
      xsSet = fmap (\(p,e) -> (e,p)) xs
      ys = Map.fromListWith mappend xsSet
      listRes = Map.toList $ Map.fromListWith Set.union $ map (\(e,p) -> (p,Set.singleton e)) (Map.toList ys)
      heapRes = runST $ do
        h <- HeapD.new testElements
        HeapD.pushList xs h
        HeapD.popAll h
      heapResSet = map (\pairs@((p,_) : _) -> (p,Set.fromList $ map snd pairs))
        $ groupBy (on (==) fst) heapRes
  in heapResSet == listRes

