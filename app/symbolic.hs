{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Function ((&))
import Data.Ratio ((%))
import System.Environment (getArgs)
import Data.Graph.Inductive.Graph (Node, mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as GraphViz
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

data Parity = Odd | Even

showParity :: Parity -> String
showParity = \case
  Odd -> "odd"
  Even -> "even"

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
  "success if " ++ show (treeK s) ++ " >= "
  ++ show (treeNs s) ++ " * (n / " ++ show nDividedBy ++ ")"

data TreeStructure = Success Success
                   | Is Parity Tree
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
                Just Even -> Is Even (tree' (expDiv2 exp) d)
                Just Odd -> Is Odd (tree' (expMult3Plus1 exp) d)
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
            Is _ t' -> depthFirstIteration' t' depth'
            If tOdd tEven ->
              -- It's important to iterate on tEven first, as every success must
              -- include at least one division by 2, which can only happen when
              -- an integer is even.
              depthFirstIteration' tEven depth'
              ++ depthFirstIteration' tOdd depth'

depthFirstIterationDelta :: [Integer]
depthFirstIterationDelta =
  map (uncurry (-)) $ zip (tail depthFirstIteration) depthFirstIteration

fibs :: [Integer]
fibs = 0 : 1 : map (uncurry (+)) (zip fibs (tail fibs))

elemInMonotonic :: Integer -> [Integer] -> Bool
elemInMonotonic n = elem n . takeWhile (<= n)

isFib :: Integer -> Bool
isFib = (`elemInMonotonic` fibs)

depthFirstIterationDeltaFibs :: [Bool]
depthFirstIterationDeltaFibs = map isFib depthFirstIterationDelta

boolSplit :: [Bool] -> Rational
boolSplit bs =
  let trueLen = length (filter id bs)
  in (fromIntegral trueLen) % (fromIntegral (length bs - trueLen))

countSuccesses :: Tree -> Integer -> Rational
countSuccesses _ 0 = 0
countSuccesses t depth =
  let depth' = depth - 1
  in case treeStruct t of
    Success _ -> 1
    Is _ t' -> countSuccesses t' depth'
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
                     Is Odd t' -> ("is odd", printTreeLines t' depth')
                     Is Even t' -> ("is even", printTreeLines t' depth')
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
                 Is _ t' -> do
                   putStr " "
                   printShape' t' indent'
                 If tOdd tEven -> do
                   printShape' tOdd indent'
                   mapM_ (const (putStr " ")) [0..indent-1]
                   printShape' tEven indent'

type NodeLabel = T.Text
type EdgeLabel = ()

toGraph :: Tree -> Integer -> Gr NodeLabel EdgeLabel
toGraph t depth =
  let (nodes, edges, _) = toGraph' t depth 0 0
      edges' = map (\(from, to) -> (from, to, ())) edges
  in mkGraph ((0, "") : nodes) edges'
  where toGraph' :: Tree -> Integer -> Int -> Int -> ([(Node, T.Text)], [(Node, Node)], Int)
        toGraph' _ 0 _ i = ([], [], i)
        toGraph' t depth parent i =
          let depth' = depth - 1
              i' = i + 1
          in case treeStruct t of
            Success _ ->
              ([(i', "success")], [(parent, i')], i')
            Is parity t' ->
              let (nodes, edges, i'') = toGraph' t' depth' i' i'
              in ((i', T.pack (showParity parity)) : nodes, (parent, i') : edges, i'')
            If tOdd tEven ->
              let (nodesEven, edgesEven, i'') = toGraph' tEven depth' i' i'
                  (nodesOdd, edgesOdd, i''') = toGraph' tOdd depth' i' i''
              in ((i', "even/odd") : nodesEven ++ nodesOdd, (i, i') : edgesEven ++ edgesOdd, i''')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "print", depth ] ->
      printTree tree (read depth)
    [ "shape", depth ] ->
      printShape tree (read depth)
    [ "percent", depth ] ->
      let p = fromRational (100 * countSuccesses tree (read depth))
      in putStrLn (show p ++ "%")
    [ "depthfirst" ] -> mapM_ print depthFirstIteration
    [ "depthfirstdelta" ] -> mapM_ print depthFirstIterationDelta
    [ "depthfirstdelta", "fibratio", depth ] ->
      print $ fromRational $ boolSplit $ take (read depth) depthFirstIterationDeltaFibs
    [ "graph", depth ] ->
      toGraph tree (read depth)
      & graphToDot nonClusteredParams { fmtNode = \(n, l) -> [GraphViz.Label (GraphViz.StrLabel l)] }
      & printDotGraph
      & T.putStr
    _ -> return ()