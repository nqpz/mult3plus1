module Mult3Plus1 where

import Mult3Plus1.Tree (tree, subTree)

import Mult3Plus1.Expanders.InfiniteBinarySplitter (infiniteBinarySplitter)
import Mult3Plus1.Expanders.FiniteChain (finiteChain)
import Mult3Plus1.Expanders.RandomExpander (randomExpander)

import Mult3Plus1.TreeTransformations.ShapeFormatter (formatShape)

import qualified Data.Text as T
import qualified Data.Text.IO as T
