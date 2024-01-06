{-# LANGUAGE LambdaCase #-}
module Mult3Plus1.TreeTransformations.Utils
  ( countSuccessRate
  , avgBranchingFactor
  , depthFirstIteration
  , depthFirstIterationDelta
  , depthFirstIterationDeltaFibs
  ) where

import Mult3Plus1.Tree (Tree(..), SubTree)

import Data.Ratio ((%))
import Data.Function ((&))
import qualified Data.List.NonEmpty as LNE

countSuccessRate :: SubTree state end -> Rational
countSuccessRate = \case
  Leaf (Left _) -> 1
  Leaf (Right _) -> 0
  Node _ (t LNE.:| []) -> countSuccessRate t
  Node _ (t0 LNE.:| [t1]) -> countSuccessRate t0 / 2 + countSuccessRate t1 / 2
  Node _ _ -> error "not supported" -- why not?

avgBranchingFactor :: Tree state end -> Rational
avgBranchingFactor tree =
  let fs = avgBranchingFactor' tree
  in sum fs % fromIntegral (length fs)
avgBranchingFactor' = \case
  Leaf _ -> []
  Node _ (t LNE.:| []) -> 1 : avgBranchingFactor' t
  Node _ (t0 LNE.:| [t1]) -> 2 : avgBranchingFactor' t0 ++ avgBranchingFactor' t1
  Node _ _ -> error "not supported" -- why not?

elemInMonotonic :: Integer -> [Integer] -> Bool
elemInMonotonic n = elem n . takeWhile (<= n)

isFib :: Integer -> Bool
isFib = (`elemInMonotonic` fibs)
  where fibs :: [Integer]
        fibs = 0 : 1 : map (uncurry (+)) (zip fibs (tail fibs))

treeWithDepths :: Tree state end -> Tree (state, Integer) (end, Integer)
treeWithDepths = treeWithDepths' 0
  where treeWithDepths' depth = \case
          Leaf end -> Leaf (end, depth)
          Node state ts -> Node (state, depth) $ fmap (treeWithDepths' (depth + 1)) ts

-- Do a depth-first traversal of the tree and find how many steps it takes to
-- prove each subpart.
depthFirstIteration :: Tree state end -> [Integer]
depthFirstIteration tree = depthFirstIteration' (treeWithDepths tree)
  where depthFirstIteration' :: Tree (state, Integer) (end, Integer) -> [Integer]
        depthFirstIteration' = \case
          Leaf (_, depth) -> [depth]
          Node _ ts -> concatMap depthFirstIteration' ts

depthFirstIterationDelta :: Tree state end -> [Integer]
depthFirstIterationDelta tree =
  map (uncurry (-)) $ zip (tail (depthFirstIteration tree)) (depthFirstIteration tree)

depthFirstIterationDeltaFibs :: Tree state end -> Int -> (Int, Int)
depthFirstIterationDeltaFibs tree chunkSize =
  map isFib (depthFirstIterationDelta tree)
  & take chunkSize
  & boolSplit

boolSplit :: [Bool] -> (Int, Int)
boolSplit bs =
  let trueLen = length (filter id bs)
  in (trueLen, length bs)
