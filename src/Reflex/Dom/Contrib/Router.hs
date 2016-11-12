{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.Contrib.Router where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import           Reflex.Dom                hiding (Window)
#if ghcjs_HOST_OS
import           GHCJS.DOM.History         (back, forward, pushState)
import           GHCJS.DOM.Window          (getLocation, popState)
import           GHCJS.DOM.Location        (toString)
import           GHCJS.DOM.Window          (Window, getHistory)
import           GHCJS.Marshal.Pure
import qualified GHCJS.DOM                 as DOM
import qualified GHCJS.DOM.Document        as DOM
import qualified GHCJS.DOM.EventM          as DOM
#else
-- import Graphics.UI.Gtk.Types.Document
#endif

------------------------------------------------------------------------------
data RouteConfig t = RouteConfig
  { _routeConfig_forward   :: Event t () -- ^ Move the browser history forward
  , _routeConfig_back      :: Event t () -- ^ Move the browser history back
  , _routeConfig_pushState :: Event t T.Text -- ^ Push to the URL state
  -- , _routeConfig_pathBase  :: T.Text
  --   -- ^ The part of the URL not related to SPA routing
  }

data Route t = Route {
    _route_value :: Dynamic t T.Text -- ^ URL value
  }

-- | Manipulate and track the URL text for dynamic routing of a widget
route :: (HasWebView m, MonadWidget t m) => RouteConfig t -> m (Route t)
route (RouteConfig goForward goBack sSet) = do
  win <- askDomWindow
  loc <- getLocation' win
  Just hist <- liftIO $ getHistory win
  performEvent_ $ ffor goForward $ \_ -> liftIO (forward hist)
  performEvent_ $ ffor goBack    $ \_ -> liftIO (back hist)
  setLoc <- performEvent $ ffor sSet $ \t -> do
    pushState hist (pToJSVal (0 :: Int)) ("" :: T.Text) t
    getLocation' win
  newLocs <- getPopState
  Route <$> holdDyn loc (leftmost [setLoc, newLocs])

-- | Get the DOM window object.
askDomWindow :: (HasWebView m, MonadIO m) => m Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO . DOM.webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ DOM.getDefaultView doc
  return window

getLocation' :: MonadIO m => Window -> m T.Text
getLocation' w = toString . fromJust =<< liftIO (getLocation w)

getPopState :: (MonadWidget t m) => m (Event t T.Text)
getPopState = do
  window <- askDomWindow
  wrapDomEventMaybe window (`DOM.on` popState) $ do
    l <- getLocation window
    case l of
      Nothing -> return Nothing
      Just loc -> do t <- toString loc; return (Just t)


#if ghcjs_HOST_OS
#else
data Location
data Window
data JSVal
data History

data SerializedScriptValue =
  SerializedScriptValue { unSerializedScriptValue :: JSVal }

forward :: History -> IO ()
forward = undefined

back :: History -> IO ()
back = undefined

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)

getLocation :: Window -> IO (Maybe Location)
getLocation = undefined

getHistory :: Window -> IO (Maybe History)
getHistory = undefined

getState :: History -> IO (Maybe SerializedScriptValue)
getState = undefined

toString :: Location -> IO T.Text
toString = undefined

getDefaultView = undefined

pushState = undefined
#endif
