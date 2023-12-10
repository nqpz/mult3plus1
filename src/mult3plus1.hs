{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Function ((&))
import Data.Maybe (maybe, fromMaybe, catMaybes)
import Data.Ratio ((%), denominator)
import System.Environment (getArgs)


takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl f (x : xs) | f x = x : takeWhileIncl f xs
                         | otherwise = [x]
takeWhileIncl _ [] = []

isDivisibleBy2 :: Rational -> Bool
isDivisibleBy2 x = denominator (x / 2) == 1

data Div2Result a = CanDivide a
                  | CannotDivide
                  | OneOf (Div2Result a) (Div2Result a)
                    -- ^ either one could be the result
                  | Uncertain

class CollatzIntegral a where
  divi2 :: a -> Div2Result a
  mult :: a -> Rational -> a
  plus :: a -> Rational -> a
  ge :: a -> a -> Bool

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
        canDivide
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
      --
      -- Try both cases and check if they both result in a smaller integer.
      OneOf canDivide CannotDivide
    where iEven = isDivisibleBy2 i
          canDivide = CanDivide $ IntegerMod (m / 2) (i / 2)

  mult (IntegerMod m i) k =
    IntegerMod (m * k) (i * k)

  plus (IntegerMod m i) k =
    IntegerMod m (i + k)

  ge (IntegerMod m1 i1) (IntegerMod m2 i2) =
    m1 > m2 || (m1 == m2 && i1 >= i2)

handleDiv2Result :: CollatzIntegral i => Div2Result i -> Maybe (Maybe i)
handleDiv2Result = \case
  CanDivide a -> Just $ Just a
  CannotDivide -> Just Nothing
  OneOf a b -> Nothing -- FIXME: Handle this case
  Uncertain -> Nothing

step :: CollatzIntegral i => i -> Maybe i
step n = fromMaybe ((n `mult` 3) `plus` 1) <$> handleDiv2Result (divi2 n)

stepsUntilSmaller :: CollatzIntegral i => i -> Maybe Integer
stepsUntilSmaller n =
  Just n
  & iterate (>>= step)
  & takeWhileIncl (maybe False (`ge` n))
  & map (fmap (const 1))
  & foldl1 (\m1 m2 -> m1 >>= \n1 -> m2 >>= \n2 -> pure (n1 + n2))
  & fmap (subtract 1)

data Cur = Cur { curMod :: Integer
               , curTests :: [(Integer, Integer)]
               }
  deriving (Show)

initCur :: Cur
initCur = Cur 2 [(1, 2)]

searchStep :: Cur -> Cur
searchStep (Cur m ts) = Cur m' (concatMap findTests ts)
  where m' = m * 2
        indices kmod = [0..m' `div` kmod - 1]
        findTests (k, kmod) =
          let is = indices kmod
              is' = map (stepsUntilSmaller . IntegerMod (fromIntegral m')
                          . fromIntegral . (+ k) . (* kmod)) is
                    & zipWith (\i -> \case
                                  Nothing -> Just i
                                  Just _ -> Nothing
                              ) is
                    & catMaybes
          in findSmallestMods is' 0 1 k kmod
        findSmallestMods is offset scale k kmod
          | all (`elem` is) tests = [(k, kmod)]
          | all (`notElem` is) tests = []
          | otherwise =
              let kmod' = kmod * 2
                  scale' = scale * 2
              in findSmallestMods is offset scale' k kmod'
                 ++ findSmallestMods is (offset + scale) scale' (k + kmod) kmod'
          where tests = map (+ offset) $ map (* scale) (indices kmod)

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
