{-# LANGUAGE LambdaCase #-}
module Mult3Plus1.TreeTransformations.ShapeFormatter (formatShape) where

import Mult3Plus1.Expander (WillReachEnd)
import qualified Mult3Plus1.Tree as Tr

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T
import Control.Monad (foldM)
import qualified Data.Array.ST as ST
import qualified Data.Array as A

type CharArray = A.Array (Int, Int) Char

data Tree = Node (LNE.NonEmpty Tree)
          | Leaf
  deriving (Show, Eq)

toShapeOnlyTree :: Tr.Tree state end -> Tree
toShapeOnlyTree = \case
  Tr.Node _ ts -> Node $ LNE.map toShapeOnlyTree ts
  Tr.Leaf _ -> Leaf

treeChars :: Tree -> (Int, Int, [(Int, Int, Char)])
treeChars = normalize . treeChars' 0 0
  where normalize :: (Int, Int, Int, [(Int, Int, Char)])
                  -> (Int, Int, [(Int, Int, Char)])
        normalize (yMax, xMin, xMax, tcs) =
          (yMax, xMax - xMin, map (\(y, x, c) -> (y, x - xMin, c)) tcs)

-- FIXME: This is broken.
treeChars' :: Int -> Int -> Tree -> (Int, Int, Int, [(Int, Int, Char)])
treeChars' y x = \case
  Node (t LNE.:| []) ->
    let (yMax, xMin, xMax, tcs) = treeChars' (y + 1) x t
    in (yMax, xMin, xMax, (y, -xMin, '|') : tcs)
  Node (t0 LNE.:| [t1]) ->
    let (yMax0, xMin0, xMax0, tcs0) = treeChars' (y + 1) (x - 1) t0
        (yMax1, xMin1, xMax1, tcs1) = treeChars' (y + 1) 0 t1
        tcs1' = map (\(y, x, c) -> (y, xMax0 + x - xMin1, c)) tcs1
        tcs = (y, x - 1, '/') : (y, xMax0, '\\') : tcs0 ++ tcs1'
    in (max yMax0 yMax1, xMin0, xMax0 + xMax1 - xMin1, tcs)
  Node _ -> error "not supported"
  Leaf -> (y, x, x + 1, [(y, x, '!')])

buildArray :: Int -> Int -> [(Int, Int, Char)] -> CharArray
buildArray yMax xMax chars = ST.runSTArray $ do
  let xMax' = xMax + 1
      chars' = map (\y -> (y, xMax', '\n')) [0..yMax] ++ chars
  a <- ST.newArray ((0, 0), (yMax, xMax')) ' '
  foldM (\a (y, x, c) -> ST.writeArray a (y, x) c >> pure a) a chars'

arrayToText :: CharArray -> T.Text
arrayToText = T.pack . A.elems

formatShape :: WillReachEnd state end => Tr.Tree state end -> T.Text
formatShape tree =
  let (yMax, xMax, chars) = treeChars $ toShapeOnlyTree tree
      array = buildArray yMax xMax chars
  in arrayToText array
