{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mult3Plus1.TreeTransformations.Graph (toGraph, toGraphViz) where

import Mult3Plus1.Expander (WillReachEnd, ExpanderFormatter(..))
import Mult3Plus1.Tree (Tree(..), SubTree)

import qualified Data.List.NonEmpty as LNE
import Data.Graph.Inductive.Graph (Node, mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as GraphViz
import qualified Data.Text.Lazy as T
import Control.Arrow ((>>>))

type NodeLabel = T.Text
type EdgeLabel = ()

toGraph :: forall state end.
           (WillReachEnd state end,
            ExpanderFormatter state,
            ExpanderFormatter end)
        => Tree state end -> Gr NodeLabel EdgeLabel
toGraph t =
  let (nodes, edges, _) = toGraph' t 0 0
      edges' = map (\(from, to) -> (from, to, ())) edges
  in mkGraph ((0, "root") : nodes) edges'
  where toGraph' :: Tree state end -> Int -> Int -> ([(Node, T.Text)], [(Node, Node)], Int)
        toGraph' t parent i =
          let i' = i + 1
          in case t of
            Leaf end ->
              ([(i', T.fromStrict (format end))], [(parent, i')], i')
            Node s (t' LNE.:| []) ->
              let (nodes, edges, i'') = toGraph' t' i' i'
              in ((i', T.fromStrict (format s)) : nodes, (parent, i') : edges, i'')
            Node s (t0 LNE.:| [t1]) ->
              let (nodes0, edges0, i'') = toGraph' t0 i' i'
                  (nodes1, edges1, i''') = toGraph' t1 i' i''
              in ((i', T.fromStrict (format s)) : nodes0 ++ nodes1, (parent, i') : edges0 ++ edges1, i''')

toGraphViz :: (WillReachEnd state end,
               ExpanderFormatter state,
               ExpanderFormatter end)
           => Tree state end -> T.Text
toGraphViz =
  toGraph
  >>> graphToDot nonClusteredParams { fmtNode = \(n, l) -> [GraphViz.Label (GraphViz.StrLabel l)] }
  >>> printDotGraph
