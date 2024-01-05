{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Mult3Plus1.Expanders.CollatzProof.Expression
  ( Expression(..)
  , Parity(..)
  , expParity
  , expDiv2
  , expMult3Plus1
  , expReplaceEven
  , expReplaceOdd
  , expLessThan
  ) where

import Mult3Plus1.Expander ( ExpanderFormatter(..)
                           )

import qualified Data.Text as T

data Expression = Expression { expNs :: Integer
                             , expK :: Integer
                             }
  deriving (Show)

data Parity = Even | Odd
  deriving (Show)

instance ExpanderFormatter Parity where
  format = \case
    Even -> "even"
    Odd -> "odd"

instance ExpanderFormatter (Maybe Parity) where
  format = \case
    Just p -> format p
    Nothing -> "even/odd"

instance ExpanderFormatter (Expression, Integer) where
  format (exp, nDividedBy) =
    T.pack (show (expNs exp) ++ " n / " ++ show nDividedBy ++ " + " ++ show (expK exp))

expParity :: Expression -> Maybe Parity
expParity exp = case (expNs exp `mod` 2 == 0, expK exp `mod` 2 == 0) of
                  (True, True) -> Just Even
                  (True, False) -> Just Odd
                  _ -> Nothing -- could be either, depending on N

expDiv2 :: Expression -> Expression
expDiv2 exp = Expression { expNs = expNs exp `div` 2
                         , expK = expK exp `div` 2
                         }

expMult3Plus1 :: Expression -> Expression
expMult3Plus1 exp = Expression { expNs = 3 * expNs exp
                               , expK = 3 * expK exp + 1
                               }

expReplaceEven :: Expression -> Expression
expReplaceEven exp = exp { expNs = 2 * expNs exp }

expReplaceOdd :: Expression -> Expression
expReplaceOdd exp = Expression { expNs = 2 * expNs exp
                               , expK = expK exp + expNs exp }

expLessThan :: Expression -> Integer -> Bool
expLessThan exp nDividedBy = expNs exp < nDividedBy
