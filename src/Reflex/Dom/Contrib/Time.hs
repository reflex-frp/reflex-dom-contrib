{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Reflex.Dom.Contrib.Time (
    poissonLossyFrom
  , poissonLossy
  , inhomogeneousPoissonFrom
  , inhomogeneousPoisson
  ) where

import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Old
import Reflex.Dom.Time hiding (poissonLossyFrom,inhomogeneousPoissonFrom,poissonLossy,inhomogeneousPoisson)

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import System.Random


-- | Send events with Poisson timing with the given basis and rate
--   Each occurence of the resulting event will contain the index of
--   the current interval, with 0 representing the basis time
poissonLossyFrom
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Double
  -- ^ Poisson event rate (Hz)
  -> UTCTime
  -- ^ Baseline time for events
  -> Event t a
  -- ^ Event that starts a tick generation thread. Usually you want this to
  -- be something like the result of getPostBuild that only fires once. But
  -- there could be uses for starting multiple timer threads.
  -- Start sending events in response to the event parameter.
  -> m (Event t TickInfo)
poissonLossyFrom rnd rate t0 t =
  inhomogeneousPoissonFrom rnd (current $ constDyn rate) rate t0 t


-- | Send events with Poisson timing with the given basis and rate
--   Each occurence of the resulting event will contain the index of
--   the current interval, with 0 representing the basis time.
--   Automatically begin sending events when the DOM is built
poissonLossy
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Double
  -- ^ Poisson event rate (Hz)
  -> UTCTime
  -- ^ Baseline time for events
  -> m (Event t TickInfo)
poissonLossy rnd rate t0 = poissonLossyFrom rnd rate t0 =<< getPostBuild

-- | Send events with inhomogeneous Poisson timing with the given basis
--   and variable rate. Provide a maxRate that you expect to support.
inhomogeneousPoissonFrom
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Behavior t Double
  -> Double
  -> UTCTime
  -> Event t a
  -> m (Event t TickInfo)
inhomogeneousPoissonFrom rnd rate maxRate t0 e = do

  -- Create a thread for producing homogeneous poisson events
  -- along with random Doubles (usage of Double's explained below)
  ticksWithRateRand <- performEventAsync $
                       fmap callAtNextInterval e

  -- Filter homogeneous events according to associated
  -- random values and the current rate parameter
  return $ attachWithMaybe filterFun rate ticksWithRateRand

  where

    -- Inhomogeneous poisson processes are built from faster
    -- homogeneous ones by randomly dropping events from the
    -- fast process. For each fast homogeneous event, choose
    -- a uniform random sample from (0, rMax). If the
    -- inhomogeneous rate at this moment is greater than the
    -- random sample, then keep this event, otherwise drop it
    filterFun :: Double -> (TickInfo, Double) -> Maybe TickInfo
    filterFun r (tInfo, p)
      | r >= p    = Just tInfo
      | otherwise = Nothing

    callAtNextInterval _ cb = void $ liftIO $ forkIO $ go t0 rnd cb 0

    go tTargetLast lastGen cb lastN = do
      t <- getCurrentTime

      -- Generate random numbers for this poisson interval (u)
      -- and sample-retention likelihood (p)
      let (u, nextGen)            = randomR (0,1) lastGen
          (p :: Double, nextGen') = randomR (0,maxRate) nextGen

      -- Inter-event interval is drawn from exponential
      -- distribution accourding to u
      let dt             = realToFrac $ -1 * log(u)/maxRate :: NominalDiffTime
          nEvents        = lastN + 1
          alreadyElapsed = diffUTCTime t tTargetLast
          tTarget        = addUTCTime dt tTargetLast
          thisDelay      = realToFrac $ diffUTCTime tTarget t :: Double
      threadDelay $ ceiling $ thisDelay * 1000000
      void $ cb $ (TickInfo t nEvents alreadyElapsed, p)
      go tTarget nextGen' cb nEvents

-- | Send events with inhomogeneous Poisson timing with the given basis
--   and variable rate. Provide a maxRate that you expect to support
inhomogeneousPoisson
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Behavior t Double
  -> Double
  -> UTCTime
  -> m (Event t TickInfo)
inhomogeneousPoisson rnd rate maxRate t0 =
  inhomogeneousPoissonFrom rnd rate maxRate t0 =<< getPostBuild
