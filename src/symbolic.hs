module Main where

data Parity = Odd | Even

data Expression = Expression { expNs :: Integer
                             , expK :: Integer
                             }

data Success = BasicSuccess
             | FiniteSuccess { treeK :: Integer
                             , treeNs :: Integer
                             }
               -- ^ A success if treeK < treeNs * (N / treeNDividedBy).  We
               -- assume this is a success, since there are only a finite number
               -- of integers to check if the condition does not hold.

data TreeStructure = Success Success
                   | IsOdd Tree
                   | IsEven Tree
                   | If { treeOdd :: Tree
                        , treeEven :: Tree
                        }
                     -- ^ A split where both children need to be successful.

data Tree = Tree { treeNDividedBy :: Integer
                 , treeExp :: Expression
                 , treeStruct :: TreeStructure
                 }

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

expLessThan :: Expression -> Integer -> Maybe Success
expLessThan exp nDividedBy =
  if expNs exp < nDividedBy
  then Just $ if expK exp == 0
              then BasicSuccess
              else FiniteSuccess { treeK = expK exp
                                 , treeNs = nDividedBy - expNs exp
                                 }
  else Nothing

tree :: Tree
tree = tree' (Expression { expNs = 1, expK = 0 }) 1
  where tree' exp d =
          Tree { treeExp = exp
               , treeNDividedBy = d
               , treeStruct = struct exp d
               }

        struct exp d =
          case expLessThan exp d of
            Just success -> Success success
            Nothing ->
              case expParity exp of
                Just Even -> IsEven (tree' (expDiv2 exp) d)
                Just Odd -> IsOdd (tree' (expMult3Plus1 exp) d)
                Nothing -> let d' = d * 2
                           in If { treeOdd = tree' (expReplaceOdd exp) d'
                                 , treeEven = tree' (expReplaceEven exp) d'
                                 }

main :: IO ()
main = return ()
