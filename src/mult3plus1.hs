module Main where

import Data.Maybe (maybe, fromMaybe, catMaybes)
import Data.Ratio ((%))
import System.Environment (getArgs)


takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl f (x : xs) | f x = x : takeWhileIncl f xs
                         | otherwise = [x]
takeWhileIncl _ [] = []

class CollatzIntegral a where
  divi :: a -> Integer -> Maybe (Maybe a)
  mult :: a -> Integer -> a
  plus :: a -> Integer -> a
  ge :: a -> a -> Bool

instance CollatzIntegral Integer where
  divi a k
    | a `mod` k == 0 = Just $ Just (a `div` k)
    | otherwise = Just Nothing
  mult = (*)
  plus = (+)
  ge = (>=)

data IntegerMod = IntegerMod { modPart :: Integer
                             , intPart :: Integer
                             }
  deriving (Show)

instance CollatzIntegral IntegerMod where
  divi (IntegerMod m i) k =
    case (m `mod` k == 0, i `mod` k == 0) of
      (True, True) ->
        -- k * a + k * b: Trivial to divide by k
        Just $ Just $ IntegerMod (m `div` k) (i `div` k)
      (False, True) ->
        -- l * a + k * b: Not trivial to divide the first part by k
        Nothing
      (True, False) ->
        -- k * a + l * b: Not trivial to divide the second part by k
        Just Nothing
      (False, False) ->
        -- l * a + m * b: Not trivial to divide any part by k
        Just Nothing
  mult (IntegerMod m i) k = IntegerMod (m * k) (i * k)
  plus (IntegerMod m i) k = IntegerMod m (i + k)
  ge (IntegerMod m1 i1) (IntegerMod m2 i2) = m1 > m2 || (m1 == m2 && i1 >= i2)

step :: CollatzIntegral i => i -> Maybe i
step n = fmap (fromMaybe ((n `mult` 3) `plus` 1)) (n `divi` 2)

stepsUntilSmaller :: CollatzIntegral i => i -> Maybe Integer
stepsUntilSmaller n =
  fmap (subtract 1)
  $ foldl1 (\m1 m2 -> m1 >>= \n1 -> m2 >>= \n2 -> return (n1 + n2))
  $ map (fmap (const 1)) $ takeWhileIncl (maybe False (`ge` n))
  $ iterate (>>= step) $ Just n

data Cur = Cur { curMod :: Integer
               , curTests :: [(Integer, Integer)]
               }
  deriving (Show)

initCur :: Cur
initCur = Cur 2 [(1, 2)]

searchStep :: Cur -> Cur
searchStep (Cur m ts) = Cur m' ts'
  where m' = m * 2
        ts' =
          concatMap (\(k, kmod) ->
                       let is = [0..m' `div` kmod - 1]
                           is' = catMaybes
                                 $ zipWith (\i res -> case res of
                                               Nothing -> Just i
                                               _ -> Nothing
                                           ) is
                                 $ map (stepsUntilSmaller . IntegerMod m'
                                        . (+ k) . (* kmod)) is
                       in findSmallestMods is' 0 1 k kmod
                    ) ts

        findSmallestMods is offset scale k kmod
          | all (\j -> j `elem` is) tests = [(k, kmod)]
          | all (\j -> j `notElem` is) tests = []
          | otherwise =
              let kmod' = kmod * 2
                  scale' = scale * 2
              in findSmallestMods is offset scale' k kmod' ++
                 findSmallestMods is (offset + scale) scale' (k + kmod) kmod'
          where tests = map (+ offset) $ map (* scale) [0..m' `div` kmod - 1]

search :: Integer -> Cur
search = search' initCur
  where search' cur 0 = cur
        search' cur d = search' (searchStep cur) (d - 1)

curRatio :: Cur -> Rational
curRatio (Cur m ts) = (sum $ map (\(_, kmod) -> m `div` kmod) ts) % m

curRatio' :: Cur -> Double
curRatio' = fromRational . curRatio


main :: IO ()
main = do
  args <- getArgs
  case args of
    [depth] -> do
      let s = search (read depth)
      print $ length $ curTests s
      print $ curRatio' s
    _ -> return ()
