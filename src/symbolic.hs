module Main where

import Data.Ratio ((%))
import System.Environment (getArgs)

data Parity = Odd | Even

data Expression = Expression { expNs :: Integer
                             , expK :: Integer
                             }

showExp :: Expression -> Integer -> String
showExp exp nDividedBy =
  show (expNs exp) ++ " n / " ++ show nDividedBy ++ " + " ++ show (expK exp)

data Success = BasicSuccess
             | FiniteSuccess { treeK :: Integer
                             , treeNs :: Integer
                             }
               -- ^ A success if treeK < treeNs * (n / treeNDividedBy).  We
               -- assume this is a success, since there are only a finite number
               -- of integers to check if the condition does not hold.

showSuccess :: Success -> Integer -> String
showSuccess BasicSuccess _ = "success"
showSuccess s nDividedBy =
  "success if " ++ show (treeK s) ++ " >= " ++ show (treeNs s) ++ " * (n / " ++ show nDividedBy ++ ")"

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

-- Do a depth-first traversal of the tree and find how many steps it takes to
-- prove each subpart.
depthFirstIteration :: [Integer]
depthFirstIteration = depthFirstIteration' tree 0
  where depthFirstIteration' :: Tree -> Integer -> [Integer]
        depthFirstIteration' t depth =
          let depth' = depth + 1
          in case treeStruct t of
            Success _ -> [depth]
            IsOdd t' -> depthFirstIteration' t' depth'
            IsEven t' -> depthFirstIteration' t' depth'
            If tOdd tEven ->
              -- It's important to iterate on tEven first, as every success must
              -- include at least one division by 2, which can only happen when
              -- an integer is even.
              depthFirstIteration' tEven depth'
              ++ depthFirstIteration' tOdd depth'

countSuccesses :: Tree -> Integer -> Rational
countSuccesses _ 0 = 0
countSuccesses t depth =
  let depth' = depth - 1
  in case treeStruct t of
    Success _ -> 1
    IsOdd t' -> countSuccesses t' depth'
    IsEven t' -> countSuccesses t' depth'
    If tOdd tEven -> let pOdd = countSuccesses tOdd depth'
                         pEven = countSuccesses tEven depth'
                     in pOdd / 2 + pEven / 2

printTree :: Tree -> Integer -> IO ()
printTree t depth = mapM_ putStrLn $ printTreeLines t depth
  where printTreeLines :: Tree -> Integer -> [String]
        printTreeLines _ 0 = [ "MAX DEPTH REACHED" ]
        printTreeLines t depth =
          let (status, struct) =
                let depth' = depth - 1
                in case treeStruct t of
                     Success s -> (showSuccess s (treeNDividedBy t), [])
                     IsOdd t' -> ("is odd", printTreeLines t' depth')
                     IsEven t' -> ("is even", printTreeLines t' depth')
                     If tOdd tEven -> ("is odd or even",
                                       [ "if odd" ]
                                       ++ printTreeLines tOdd depth'
                                       ++ [ "-----"
                                          , "if even" ]
                                       ++ printTreeLines tEven depth')
          in ("Expression: " ++ showExp (treeExp t) (treeNDividedBy t))
             : status
             : map ("  " ++) struct

printShape :: Tree -> Integer -> IO ()
printShape t depth = printShape' t 0
  where printShape' :: Tree -> Integer -> IO ()
        printShape' t indent
          | indent == depth = putStrLn "-"
          | otherwise =
            let indent' = indent + 1
            in case treeStruct t of
                 Success s -> putStrLn "|"
                 IsOdd t' -> do
                   putStr " "
                   printShape' t' indent'
                 IsEven t' -> do
                   putStr " "
                   printShape' t' indent'
                 If tOdd tEven -> do
                   printShape' tOdd indent'
                   mapM_ (const (putStr " ")) [0..indent-1]
                   printShape' tEven indent'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "print", depth ] ->
      printTree tree (read depth)
    [ "shape", depth ] ->
      printShape tree (read depth)
    [ "percent", depth ] -> putStrLn (show (fromRational (100 * countSuccesses tree (read depth))) ++ "%")
    [ "depthfirst" ] -> mapM_ print depthFirstIteration
    _ -> return ()
