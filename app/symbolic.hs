module Main where

import Mult3Plus1.Tree (tree, subTree)
import Mult3Plus1.Expanders.InfiniteBinarySplitter (infiniteBinarySplitter)
import Mult3Plus1.Expanders.FiniteChain (finiteChain)
import Mult3Plus1.Expanders.RandomExpander (randomExpander)
import Mult3Plus1.Expanders.CollatzProof (collatzProof)
import qualified Mult3Plus1.TreeTransformations.Utils as Utils
import Mult3Plus1.TreeTransformations.Graph (toGraphViz)
import Mult3Plus1.TreeTransformations.ShapeFormatter (formatShape)

import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as T
import Control.Monad (forM_)

ctree = tree collatzProof

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "graph", depth ] ->
      T.putStr
      $ toGraphViz (subTree (read depth) ctree)
    [ "successrate", depth ] -> do
      putStr $ show $ fromRational (100 * Utils.countSuccessRate (subTree (read depth) ctree))
      putStrLn "%"
    [ "averagebranchingfactor", depth ] -> do
      let d = read depth
          f = fromRational $ Utils.avgBranchingFactor (subTree d ctree)
      print f
      putStrLn (show f ++ "^" ++ depth ++ " =~ " ++ show (f**fromIntegral d))
    [ "depthfirst" ] ->
      mapM_ print (Utils.depthFirstIteration ctree)
    [ "depthfirstdelta" ] ->
      mapM_ print (Utils.depthFirstIterationDelta ctree)
    [ "depthfirstdelta", "fibratio", depth ] ->
      print $ Utils.depthFirstIterationDeltaFibs ctree (read depth)
    [ "depthfirstdelta", "fibratio", "comparerandom", seed ] ->
      let rtree = tree (randomExpander (read seed))
          p t d = print $ Utils.depthFirstIterationDeltaFibs t d
      in forM_ [1..20] $ \depth -> do
        print depth
        p ctree depth
        p rtree depth
    _ ->
      pure ()
