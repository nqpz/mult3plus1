{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module M3P1.Random (
  CMR.Random,
  CMR.MonadRandom,
  CMR.evalRand,
  CMR.evalRandIO,
  CMR.evalRandT,
  CMR.runRandT,
  CMR.runRand,
  runRandTIO,
  evalRandTIO,
  CMR.mkStdGen,
  RandomM,
  RandomMT,
  randomSt,
  randomR,
  weightedChoice,
  weightedChoiceMay,
  choice,
  shuffle,
  randomTest
  ) where
import Control.Monad
import Data.Functor ((<$>))
import qualified Control.Monad.Random as CMR
import qualified Data.Random as DR
import qualified System.Random.Shuffle as SRS
import Data.Random.Distribution.Uniform
import Data.Word (Word64)

type RandomM = CMR.Rand CMR.StdGen
type RandomMT m = CMR.RandT CMR.StdGen m

-- | Makes a function that returns a DR.RVar usable by e.g. CMR.Rand.
randomSt :: forall m a . CMR.MonadRandom m => DR.RVar a -> m a
randomSt rvar = DR.runRVar rvar (CMR.getRandom :: m Word64)

-- | Random number in range @a..b@.
randomR :: (CMR.Random a, CMR.MonadRandom m) => a -> a -> m a
randomR a b = CMR.getRandomR (a, b)

-- | Random element from a weighted list.
weightedChoice :: CMR.MonadRandom m => [(a, Rational)] -> m a
weightedChoice = CMR.fromList

-- | Random element from a list.
choice :: CMR.MonadRandom m => [a] -> m a
choice = weightedChoice . flip zip (repeat 1)

weightedChoiceMay :: CMR.MonadRandom m => [(a, Rational)] -> Maybe (m a)
weightedChoiceMay [] = Nothing
weightedChoiceMay xs = Just (CMR.fromList xs)

-- | Shuffles list.
shuffle :: CMR.MonadRandom m => [a] -> m [a]
shuffle = SRS.shuffleM

-- | Runs in IO.
runRandTIO :: Monad m => RandomMT m a -> IO (m (a, CMR.StdGen))
runRandTIO x = liftM (CMR.runRandT x) CMR.newStdGen

-- | Evaluates in IO.
evalRandTIO :: Monad m => RandomMT m a -> IO (m a)
evalRandTIO x = liftM (CMR.evalRandT x) CMR.newStdGen

-- | Test and print.
randomTest :: Show a => Int -> RandomM a -> IO ()
randomTest n m = mapM_ print =<< CMR.evalRandIO (replicateM n m)
