module Main where

data Expression = Expression { expNs :: Integer
                             , expConst :: Integer
                             }

data TreeStructure = If { treeNDividedBy :: Integer
                        , treeOdd :: Tree
                        , treeEven :: Tree
                        }
                   -- ^ A split where both children need to be successful.
                   | Success
                   -- ^ A straightforward success.
                   | FiniteSuccess { treeNDividedBy :: Integer
                                   , treeConst :: Integer
                                   , treeNs :: Integer
                                   }
                   -- ^ A success if treeConst < treeNs * (N / treeNDividedBy).
                   -- We assume this is a success, since there are only a finite
                   -- number of integers to check if the condition does not
                   -- hold.

data Tree = Tree Expression TreeStructure

main :: IO ()
main = return ()
