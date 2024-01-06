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

data End = End
  deriving (Show)

instance ExpanderInit Witness State End where
  initial (Witness gen) = Left $ State gen

data Action = Branch | Continue | Stop
  deriving (Enum)

instance ExpanderNext State End where
  next (State gen) =
    let (actionI, gen') = uniformR (0, 2) gen -- FIXME: Make it possible to adjust the odds of each action.
    in case toEnum actionI of
      Branch ->
        let (gen'', gen''') = split gen'
        in Left $ State gen'' LNE.:| [State gen''']
      Continue -> Left $ State gen' LNE.:| []
      Stop -> Right End

instance ExpanderFormatter State where
  format = const T.empty

instance ExpanderFormatter End where
  format = const T.empty

instance WitnessForExpander Witness State End

randomExpander :: Int -> Witness
randomExpander = Witness . mkStdGen
