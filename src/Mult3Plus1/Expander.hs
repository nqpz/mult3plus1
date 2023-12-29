{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Mult3Plus1.Expander ( ExpanderInit(..)
                           , ExpanderNext(..)
                           , ExpanderFormatter(..)
                           , WitnessForExpander
                           , WillReachEnd)
where

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T

class (Show state, Show end) => ExpanderInit initialInput state end where
  initial :: initialInput -> Either state end

class (Show state, Show end) => ExpanderNext state end where
  next :: state -> Either (LNE.NonEmpty state) end

class ExpanderFormatter t where
  format :: t -> T.Text

class (ExpanderInit witnessAndInitialInput state end,
       ExpanderNext state end,
       ExpanderFormatter state,
       ExpanderFormatter end) => WitnessForExpander witnessAndInitialInput state end | witnessAndInitialInput -> state
                                                                                     , witnessAndInitialInput -> end

class WillReachEnd state end
