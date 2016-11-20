{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}

module Reflex.Dom.Contrib.Geoposition where

import           Control.Concurrent (forkIO)
import           Control.Exception (catch, throwIO)
import           Control.Monad.IO.Class (liftIO)

import           Reflex
import           Reflex.Dom

import           GHCJS.DOM (currentWindow)
import           GHCJS.DOM.PositionError (PositionException(..), PositionErrorCode(..))
import qualified GHCJS.DOM.Coordinates as Coord
import           GHCJS.DOM.Geolocation (getCurrentPosition)
import           GHCJS.DOM.Geoposition (getCoords)
import           GHCJS.DOM.Navigator (getGeolocation)
import           GHCJS.DOM.Window (getNavigator)


defaultPosException :: PositionException
defaultPosException = PositionException PositionUnavailable "Failed to get geolocation"

data GeopositionInfo = GeopositionInfo
  { geoLatitude               :: Double
  , geoLongitude              :: Double
  , geoAltitudeMeters         :: Maybe Double
  , geoAccuracyMeters         :: Double
  , geoAltitudeAccuracyMeters :: Maybe Double
  , geoHeadingDegrees         :: Maybe Double
  , geoSpeedMetersPerSec      :: Maybe Double
  } deriving (Show, Eq)

getGeopositionInfo :: IO (Either PositionException GeopositionInfo)
getGeopositionInfo = (Right <$> getInfo) `catch` (pure . Left)
  where
    getInfo = do
      window <- currentWindow >>= orBombOut
      nav    <- getNavigator window >>= orBombOut
      geoloc <- getGeolocation nav >>= orBombOut
      geopos <- getCurrentPosition geoloc Nothing
      coord  <- getCoords geopos >>= orBombOut
      GeopositionInfo
        <$> Coord.getLatitude coord
        <*> Coord.getLongitude coord
        <*> Coord.getAltitude coord
        <*> Coord.getAccuracy coord
        <*> Coord.getAltitudeAccuracy coord
        <*> Coord.getHeading coord
        <*> Coord.getSpeed coord

    orBombOut = maybe (throwIO defaultPosException) pure

attachGeoposition :: MonadWidget t m => Event t a -> m (Event t (Either PositionException GeopositionInfo, a))
attachGeoposition event = performEventAsync (fetchInfoAsync <$> event)
  where
    fetchInfoAsync a callback = liftIO $ do
        _ <- forkIO $ do
          info <- getGeopositionInfo
          callback (info, a)
        pure ()
