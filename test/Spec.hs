{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Test.QuickCheck                      (Gen, Arbitrary(..), choose, shrinkIntegral,
                                             listOf, vectorOf)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (Assertion,(@?=))
import Data.Monoid                          (All(..))
import Data.Traversable
import Control.Applicative
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

import qualified Data.Vector as V
import qualified Data.ArrayList.Generic as ArrayList
import qualified Data.Heap.Mutable.ModelD as HeapD
import qualified Data.Graph.Mutable as MGraph
import qualified Data.Graph.Immutable as Graph

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Heaps"
    [ testProperty "Model D Push No Crash" multipush
    , testProperty "Model D Push Pop" heapPushPop
    , testProperty "Model D List" heapMatchesList
    ]
  , testGroup "ArrayList"
    [ testProperty "Insertion followed by freezing" arrayListWorks
    ]
  , testGroup "Graph"
    [ testProperty "Building only from vertices" graphBuildingVertices
    , testProperty "Trivial case for Dijkstras Algorithm" dijkstraEasyDistance
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
  mempty = Min maxBound
  mappend (Min a) (Min b) = Min (min a b)

newtype MyElement = MyElement { getMyElement :: Int }
  deriving (Show,Read,Eq,Ord)

instance Arbitrary MyElement where
  arbitrary = fmap MyElement (choose (0,fromIntegral testElements - 1))
  shrink (MyElement a) = fmap MyElement $ filter (>= 0) $ shrinkIntegral a -- fmap MyElement (enumFromTo 0 (a - 1))

newtype TenElemsOrLessList a = TenElemsOrLessList [a]
  deriving (Read,Show,Eq,Ord)

instance Arbitrary a => Arbitrary (TenElemsOrLessList a) where
  arbitrary = fmap TenElemsOrLessList $ flip vectorOf arbitrary =<< choose (0 :: Int,10)
  shrink (TenElemsOrLessList a) = fmap TenElemsOrLessList $ shrink a

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

arrayListWorks :: [Int] -> Bool
arrayListWorks xs =
  let ys = runST $ do
        a <- ArrayList.new 1
        forM_ xs $ \x -> ArrayList.push a x
        ArrayList.freeze a
   in xs == V.toList ys

-- This makes sure that when you insert a bunch of vertices into
-- a graph, you get the same vertices back out. This does not
-- do anything to check for edges.
graphBuildingVertices :: [Int] -> Bool
graphBuildingVertices xs =
  let sg = runST $ Graph.create $ \mg -> do
        forM_ xs $ \x -> do
          MGraph.insertVertex mg x
      ys = Graph.with sg (Graph.verticesToVector . Graph.vertices)
   in List.nub (List.sort xs) == List.sort (V.toList ys)

graphBuildingEdgesEverywhere :: TenElemsOrLessList Int -> Bool
graphBuildingEdgesEverywhere (TenElemsOrLessList xs) =
  let onlyEdge = 77 :: Int
      sg = runST $ Graph.create $ \mg -> do
        vertices <- forM xs $ \x -> do
          MGraph.insertVertex mg x
        forM_ vertices $ \source -> do
          forM_ vertices $ \dest -> do
            MGraph.insertEdge mg source dest onlyEdge
   in getAll $ getConst $ Graph.with sg $ \g ->
             let v = Graph.vertices g -- Graph.vertices g Graph.verticesToVector . Graph.vertices)
                 vlist = Graph.verticesToVertexList v
              in for vlist $ \va ->
                   for vlist $ \vb ->
                     Const (All $ Graph.lookupEdge va vb g == Just onlyEdge)

data Thing = Thing

-- Every node is connected to at most two other nodes. The end
-- nodes only have one neighbor. Go from one end node to the other.
dijkstraEasyDistance :: [Word32] -> Bool
dijkstraEasyDistance xs =
  let sg = runST $ Graph.create $ \mg -> do
        start <- MGraph.insertVertex mg (0 :: Int)
        let insertNext prevVertex zs i = case zs of
              [] -> return ()
              y : ys -> do
                vertex <- MGraph.insertVertex mg i
                MGraph.insertEdge mg prevVertex vertex y
                insertNext vertex ys (i + 1)
        insertNext start xs 1
   in Graph.with sg $ \g -> case (Graph.lookupVertex 0 g, Graph.lookupVertex (List.length xs) g) of
        (Nothing,Nothing) -> False
        (Nothing,Just _)  -> False
        (Just _,Nothing)  -> False
        (Just start, Just end) ->
          let expected = Min (sum xs)
           in expected == Graph.dijkstra (\_ _ (Min x) distance -> Min (x + distance)) (Min 0) start end g

