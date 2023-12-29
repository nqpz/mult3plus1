{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Mult3Plus1.Expanders.InfiniteBinarySplitter (infiniteBinarySplitter) where

import Mult3Plus1.Expander ( ExpanderInit(..)
                           , ExpanderNext(..)
                           , ExpanderFormatter(..)
                           , WitnessForExpander)

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T

data Witness = Witness

data State = NoState
  deriving (Show)

data End
  deriving (Show)

instance ExpanderInit Witness State End where
  initial Witness = Left NoState

instance ExpanderNext State End where
  next NoState = Left $ NoState LNE.:| [NoState]

instance ExpanderFormatter State where
  format = const T.empty

instance ExpanderFormatter End where
  format = undefined -- impossible

instance WitnessForExpander Witness State End

infiniteBinarySplitter :: Witness
infiniteBinarySplitter = Witness
