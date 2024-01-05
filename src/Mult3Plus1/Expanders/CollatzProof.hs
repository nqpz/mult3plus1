{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Mult3Plus1.Expanders.CollatzProof (collatzProof) where

import Mult3Plus1.Expander ( ExpanderInit(..)
                           , ExpanderNext(..)
                           , ExpanderFormatter(..)
                           , WitnessForExpander)
import Mult3Plus1.Expanders.CollatzProof.Expression

import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T

data Witness = Witness

data State = State { stateParity :: Maybe Parity
                   , stateNDividedBy :: Integer
                   , stateExp :: Expression
                   }
  deriving (Show)

data End = BasicSuccess
         | FiniteSuccess { endK :: Integer
                         , endNs :: Integer
                         , endNDividedBy :: Integer
                         }
           -- ^ A success if endK < endNs * (n / endNDividedBy).  We assume this
           -- is a success, since there are only a finite number of integers to
           -- check if the condition does not hold.
  deriving (Show)

instance ExpanderInit Witness State End where
  initial Witness = Left State { stateParity = Nothing
                               , stateNDividedBy = 1
                               , stateExp = Expression { expNs = 1, expK = 0 }
                               }

instance ExpanderNext State End where
  next s =
    if exp `expLessThan` d
    then Right $ if expK exp == 0
                 then BasicSuccess
                 else FiniteSuccess { endK = expK exp
                                    , endNs = d - expNs exp
                                    , endNDividedBy = d
                                    }
    else Left $ case expParity exp of
                Just p -> State { stateParity = Just p
                                , stateNDividedBy = d
                                , stateExp = ($ exp) $ case p of
                                    Even -> expDiv2
                                    Odd -> expMult3Plus1
                                } LNE.:| []
                Nothing -> let d' = d * 2
                               sEven = State { stateParity = Just Even
                                             , stateNDividedBy = d'
                                             , stateExp = expReplaceEven exp
                                             }
                               sOdd = State { stateParity = Just Odd
                                            , stateNDividedBy = d'
                                            , stateExp = expReplaceOdd exp
                                            }
                           in sEven LNE.:| [sOdd]
    where exp = stateExp s
          d = stateNDividedBy s

instance ExpanderFormatter State where
  format s = T.intercalate ""
    [ "assumed parity: "
    , format (stateParity s)
    , "\ncondition: step("
    , format (stateExp s, stateNDividedBy s)
    , ") < n?"
    ]

instance ExpanderFormatter End where
  format BasicSuccess = "success"
  format end = T.intercalate ""
    [ "success if "
    , T.pack (show (endK end))
    , " >= "
    , T.pack (show (endNs end))
    , " * (n / "
    , T.pack (show (endNDividedBy end))
    , ")"
    ]

instance WitnessForExpander Witness State End

collatzProof :: Witness
collatzProof = Witness
