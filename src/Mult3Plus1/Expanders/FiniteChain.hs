{-# LANGUAGE MultiParamTypeClasses #-}
module Mult3Plus1.Expanders.FiniteChain (finiteChain) where

import Mult3Plus1.Expander ( ExpanderInit(..)
                           , ExpanderNext(..)
                           , ExpanderFormatter(..)
                           , WitnessForExpander
                           , WillReachEnd)

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T

data Witness = Witness Int

newtype State = State Int
  deriving (Show)

data End = End
  deriving (Show)

instance WillReachEnd State End

instance ExpanderInit Witness State End where
  initial (Witness depth) = Left (State depth)

instance ExpanderNext State End where
  next (State 0) = Right End
  next (State depth) = Left $ State (depth - 1) LNE.:| []

instance ExpanderFormatter State where
  format (State depth) = T.pack (show depth)

instance ExpanderFormatter End where
  format End = T.empty

instance WitnessForExpander Witness State End

finiteChain :: Int -> Witness
finiteChain n = Witness n
