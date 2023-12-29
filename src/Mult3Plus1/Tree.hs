{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Mult3Plus1.Tree (Tree(..), tree, TreeAborted(..), SubTree, subTree) where

import Mult3Plus1.Expander ( ExpanderInit(..)
                           , ExpanderNext(..)
                           , ExpanderFormatter(..)
                           , WitnessForExpander
                           , WillReachEnd)

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T

data Tree state end = Node state (LNE.NonEmpty (Tree state end))
                    | Leaf end
  deriving (Show, Eq)

data TreeAborted = TreeAborted
  deriving (Show, Eq)

type SubTreeEnd end = Either end TreeAborted
type SubTree state end = Tree state (SubTreeEnd end)

instance ExpanderFormatter end => ExpanderFormatter (SubTreeEnd end) where
  format = \case
    Left state -> format state
    Right TreeAborted -> T.pack "<aborted>"

instance ExpanderNext state end => WillReachEnd state (SubTreeEnd end)

tree :: forall witness state end. WitnessForExpander witness state end => witness -> Tree state end
tree witness = case initial witness of
  Left state -> tree' state
  Right end -> Leaf end
  where tree' :: state -> Tree state end
        tree' state = case next state of
          Left states -> Node state $ fmap tree' states
          Right end -> Leaf end

subTree :: Int -> Tree state end -> SubTree state end
subTree = \case
  0 -> const $ Leaf (Right TreeAborted)
  depth -> \case
    Leaf end -> Leaf (Left end)
    Node state ts -> Node state $ fmap (subTree (depth - 1)) ts
