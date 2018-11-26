{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}

module Reflex.Dom.Contrib.Geoposition where

------------------------------------------------------------------------------
import           Control.Concurrent (forkIO)
import           Control.Monad.Catch (catch, throwM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Typeable
import           GHCJS.DOM (currentWindow)
import           GHCJS.DOM.PositionError (PositionException(..), PositionErrorCode(..))
import qualified GHCJS.DOM.Coordinates as Coord
import           GHCJS.DOM.Geolocation
import           GHCJS.DOM.Geoposition
import           GHCJS.DOM.Navigator (getGeolocation)
import           GHCJS.DOM.Window (getNavigator)
import           Language.Javascript.JSaddle.Monad
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------

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
  } deriving (Eq, Ord, Show, Read, Typeable)

getGeopositionInfo :: JSM (Either PositionException GeopositionInfo)
getGeopositionInfo = (Right <$> getInfo) `catch` (pure . Left)
  where
    getInfo = do
      window <- currentWindow >>= orBombOut
      nav    <- getNavigator window
      geoloc <- getGeolocation nav
      geopos <- getCurrentPosition geoloc Nothing
      coord  <- getCoords geopos
      GeopositionInfo
        <$> Coord.getLatitude coord
        <*> Coord.getLongitude coord
        <*> Coord.getAltitude coord
        <*> Coord.getAccuracy coord
        <*> Coord.getAltitudeAccuracy coord
        <*> Coord.getHeading coord
        <*> Coord.getSpeed coord

    orBombOut = maybe (throwM defaultPosException) pure

attachGeoposition :: MonadWidget t m => Event t a -> m (Event t (Either PositionException GeopositionInfo, a))
attachGeoposition event = do
  context <- askJSM
  performEventAsync $ ffor event $ \e callback -> do
    _ <- liftIO $ forkIO $ do
      info <- runJSM getGeopositionInfo context
      callback (info, e)
    pure ()
