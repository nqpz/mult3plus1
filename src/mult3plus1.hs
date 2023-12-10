{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Data.Maybe (maybe, fromMaybe, catMaybes)
import Data.Ratio ((%), denominator)
import System.Environment (getArgs)


takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl f (x : xs) | f x = x : takeWhileIncl f xs
                         | otherwise = [x]
takeWhileIncl _ [] = []

data Div2Result a = CanDivide a
                  | CannotDivide
                  | Uncertain

class CollatzIntegral a where
  divi2 :: a -> Div2Result a
  mult :: a -> Rational -> a
  plus :: a -> Rational -> a
  ge :: a -> a -> Bool

isDivisibleBy2 :: Rational -> Bool
isDivisibleBy2 x = denominator (x / 2) == 1

instance CollatzIntegral Rational where
  divi2 a
    | isDivisibleBy2 a = CanDivide (a / 2)
    | otherwise = CannotDivide
  mult = (*)
  plus = (+)
  ge = (>=)

data IntegerMod = IntegerMod { modPart :: Rational
                             , intPart :: Rational
                             }
  deriving (Show)

instance CollatzIntegral IntegerMod where
  divi2 (IntegerMod m i) =
    if isDivisibleBy2 m
    then
      if iEven
      then
        -- (2 * a) * n + (2 * b)
        --
        -- Trivial to divide by 2.
        CanDivide $ IntegerMod (m / 2) (i / 2)
      else
        -- (2 * a) * n + (2 * b + 1)
        --
        -- Guaranteed to never divide by 2, no matter the value of a
        -- or b.
        CannotDivide
    else
      -- If iEven:
      --
      --   (2 * a + 1) * n + (2 * b)
      --
      --   Is divisible by 2 if n is even.
      --
      -- If not iEven:
      --
      --   (2 * a + 1) * n + (2 * b + 1)
      --
      --   Is divisible by 2 if n is odd.
      Uncertain
    where iEven = isDivisibleBy2 i

  mult (IntegerMod m i) k = IntegerMod (m * k) (i * k)
  plus (IntegerMod m i) k = IntegerMod m (i + k)
  ge (IntegerMod m1 i1) (IntegerMod m2 i2) = m1 > m2 || (m1 == m2 && i1 >= i2)

handleDiv2Result :: CollatzIntegral i => Div2Result i -> Maybe (Maybe i)
handleDiv2Result (CanDivide a) = Just $ Just a
handleDiv2Result CannotDivide = Just Nothing
handleDiv2Result Uncertain = Nothing

step :: CollatzIntegral i => i -> Maybe i
step n = fmap (fromMaybe ((n `mult` 3) `plus` 1)) (handleDiv2Result $ divi2 n)

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
                                 $ map (stepsUntilSmaller . IntegerMod (fromIntegral m')
                                        . fromIntegral . (+ k) . (* kmod)) is
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
