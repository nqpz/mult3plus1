{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Ratio
import Data.Either

import M3P1.Random

data Action = Mult3Plus1Div2
            | Div2
  deriving (Eq, Show)

checkForMaybeCycle :: [Action] -> Either Rational Integer
checkForMaybeCycle as =
  let t = fromIntegral $ length $ filter (== Mult3Plus1Div2) as
      u = fromIntegral $ length as
      a = 3^t % 2^u
      b = foldl (flip ($)) (0 % 1)
          $ map (\case Mult3Plus1Div2 -> \x -> (3 * x + 1) / 2
                       Div2 -> (/ 2)
                ) as
      solve_n = -b / (a - 1)
  in if denominator solve_n == 1 &&
        numerator solve_n /= 1 &&
        numerator solve_n /= 2 &&
        numerator solve_n /= 4 &&
        numerator solve_n > 0
     then Right $ numerator solve_n
     else Left solve_n

generatePermutations :: Integer -> [[Action]]
generatePermutations n = concatMap generate' [0..n]
  where generate' t =
          let u = n - t
          in generate'' t u
        generate'' 0 0 = [[]]
        generate'' t u = g1 t u ++ g2 t u
        g1 0 _ = []
        g1 t u = map (Mult3Plus1Div2 :) $ generate'' (t - 1) u
        g2 _ 0 = []
        g2 t u = map (Div2 :) $ generate'' t (u - 1)

checkForMaybeCyclesAt :: Integer -> [Integer]
checkForMaybeCyclesAt =
  rights . map checkForMaybeCycle . generatePermutations

-- n > 2
--searchForMaybeCyclesAt :: Integer -> Integer
searchForMaybeCyclesAt n =
  let findTMax0 t acc | acc < 2^n = findTMax0 (t + 1) (acc * 3)
                      | otherwise = t

      t_min = 2
      t_max0 = findTMax0 0 3

      findTMax t =
        let t' = t + 1
        in case checkForMaybeCycle (replicate (fromInteger t) Mult3Plus1Div2 ++
                                    replicate (fromInteger (n - t)) Div2) of
             Right res -> error ("unexpected result: " ++ show (n, t))
             Left err -> if err >= 1 % 1
                         then t
                         else if t' == t_max0
                              then t'
                              else findTMax t'

      t_max = findTMax t_min

      Left m = checkForMaybeCycle (replicate (fromInteger (n - t_max0)) Div2 ++
                                   replicate (fromInteger t_max0) Mult3Plus1Div2)


  in (t_max0, t_max, fromRational m)
-- n_mpd always just 2
-- 3^t % 2^u < 1

generateRandomActions :: Integer -> RandomM [Action]
generateRandomActions 0 = return []
generateRandomActions n = do
  a <- choice [Mult3Plus1Div2, Div2]
  (a :) <$> generateRandomActions (n - 1)

generateRandomActionsIO :: Integer -> IO [Action]
generateRandomActionsIO = evalRandIO . generateRandomActions

main :: IO ()
main = putStrLn "run this in the repl"
