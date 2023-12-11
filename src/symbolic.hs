module Main where

data Tree = Tree { treeNDividedBy :: Integer
                 , treeExp :: Expression
                 , treeStruct :: TreeStructure
                 }

data Expression = Expression { expNs :: Integer
                             , expK :: Integer
                             }

data TreeStructure = If { treeOdd :: Tree
                        , treeEven :: Tree
                        }
                   -- ^ A split where both children need to be successful.
                   | Success
                   -- ^ A straightforward success.
                   | FiniteSuccess { treeK :: Integer
                                   , treeNs :: Integer
                                   }
                   -- ^ A success if treeK < treeNs * (N / treeNDividedBy).  We
                   -- assume this is a success, since there are only a finite
                   -- number of integers to check if the condition does not
                   -- hold.

tree :: Tree
tree = tree' 1 0 1
  where tree' ns k d =
          let d' = d * 2
          in Tree { treeExp = Expression { expNs = ns, expK = k }
                  , treeNDividedBy = d
                  , treeStruct = If { treeOdd = tree' (ns * 2) (k + ns) d'
                                    , treeEven = tree' (ns * 2) k d' }
                  }

main :: IO ()
main = return ()
