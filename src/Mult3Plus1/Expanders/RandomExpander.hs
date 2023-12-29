{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Mult3Plus1.Expanders.RandomExpander (randomExpander) where

import Mult3Plus1.Expander ( ExpanderInit(..)
                           , ExpanderNext(..)
                           , ExpanderFormatter(..)
                           , WitnessForExpander)

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T
import System.Random (StdGen, mkStdGen, uniformR, split)

data Witness = Witness StdGen

data State = State StdGen
  deriving (Show)

data End
  deriving (Show)

instance ExpanderInit Witness State End where
  initial (Witness gen) = Left $ State gen

instance ExpanderNext State End where
  next (State gen) =
    let (branch, gen') = uniformR (False, True) gen
    in Left $ if branch
              then let (gen'', gen''') = split gen'
                   in State gen'' LNE.:| [State gen''']
              else State gen' LNE.:| []

instance ExpanderFormatter State where
  format = const T.empty

instance ExpanderFormatter End where
  format = undefined -- impossible

instance WitnessForExpander Witness State End

randomExpander :: Int -> Witness
randomExpander = Witness . mkStdGen
