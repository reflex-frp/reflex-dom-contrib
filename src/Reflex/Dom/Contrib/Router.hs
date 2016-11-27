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
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Default
import           Data.Text                 (Text)
import           Reflex.Dom                hiding (Window)
#if ghcjs_HOST_OS
import           Data.Maybe                (fromJust)
import           Control.Monad.IO.Class    (liftIO)
import qualified GHCJS.DOM                 as DOM
import qualified GHCJS.DOM.Document        as DOM
import qualified GHCJS.DOM.EventM          as DOM
import           GHCJS.DOM.History         (back, forward, pushState)
import           GHCJS.DOM.Location        (toString)
import           GHCJS.DOM.Window          (Window, getHistory,
                                            getLocation, popState)
import           GHCJS.Marshal.Pure
#else
#endif

------------------------------------------------------------------------------
data RouteConfig t = RouteConfig
  { _routeConfig_forward   :: Event t () -- ^ Move the browser history forward
  , _routeConfig_back      :: Event t () -- ^ Move the browser history back
  , _routeConfig_pushState :: Event t Text -- ^ Push to the URL state
  -- , _routeConfig_pathBase  :: Text
  --   -- ^ The part of the URL not related to SPA routing
  }

instance Reflex t => Default (RouteConfig t) where
  def = RouteConfig never never never

instance Reflex t => Monoid (RouteConfig t) where
  mempty = def
  mappend (RouteConfig f1 b1 p1) (RouteConfig f2 b2 p2) =
    RouteConfig (mappend f1 f2) (mappend b1 b2) (leftmost [p1, p2])

  mconcat rcs =
    RouteConfig (mconcat $ map _routeConfig_forward rcs)
                (mconcat $ map _routeConfig_back rcs)
                (leftmost $ map _routeConfig_pushState rcs)

data Route t = Route {
    _route_value :: Dynamic t Text -- ^ URL value
  }

instance HasValue (Route t) where
  type Value (Route t) = Dynamic t Text
  value = _route_value

-- | Manipulate and track the URL text for dynamic routing of a widget
route :: (HasWebView m, MonadWidget t m) => RouteConfig t -> m (Route t)
#if ghcjs_HOST_OS
route (RouteConfig goForward goBack sSet) = do
  win <- askDomWindow
  loc <- getLocation' win
  Just hist <- liftIO $ getHistory win
  performEvent_ $ ffor goForward $ \_ -> liftIO (forward hist)
  performEvent_ $ ffor goBack    $ \_ -> liftIO (back hist)
  setLoc <- performEvent $ ffor sSet $ \t -> do
    pushState hist (pToJSVal (0 :: Int)) ("" :: Text) t
    getLocation' win
  newLocs <- getPopState
  Route <$> holdDyn loc (leftmost [setLoc, newLocs])
#else
route = error "route is only available to ghcjs"
#endif

#if ghcjs_HOST_OS
-- | Get the DOM window object.
askDomWindow :: (HasWebView m, MonadIO m) => m Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO . DOM.webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ DOM.getDefaultView doc
  return window
#else
askDomWindow :: (MonadIO m) => m Window
askDomWindow = error "askDomWindow is only available to ghcjs"
#endif

getLocation' :: MonadIO m => Window -> m Text
#if ghcjs_HOST_OS
getLocation' w = toString . fromJust =<< liftIO (getLocation w)
#else
getLocation' = error "getLocation' is only available to ghcjs"
#endif

getPopState :: (MonadWidget t m) => m (Event t Text)
#if ghcjs_HOST_OS
getPopState = do
  window <- askDomWindow
  wrapDomEventMaybe window (`DOM.on` popState) $ do
    l <- getLocation window
    case l of
      Nothing -> return Nothing
      Just loc -> do t <- toString loc; return (Just t)
#else
getPopState = error "getPopState is only available to ghcjs"
#endif

setWindowUrl :: MonadWidget t m => Event t Text -> m ()
#if ghcjs_HOST_OS
setWindowUrl url = do
  performEvent_ $ ffor url $ \u -> do
    win <- askDomWindow
    Just hist <- liftIO $ getHistory win
    pushState hist (pToJSVal (0 :: Int)) ("" :: Text) u
#else
setWindowUrl = error "setWindowUrl only available to ghcjs"
#endif

getWindowInitUrl :: MonadWidget t m => m Text
getWindowInitUrl = getLocation' =<< askDomWindow

getWindowUrl :: MonadWidget t m => m (Dynamic t Text)
getWindowUrl = do
  win <- askDomWindow
  loc <- getLocation' win
  newLocs <- getPopState
  holdDyn loc newLocs

#if ghcjs_HOST_OS
#else
data Document
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

toString :: Location -> IO Text
toString = undefined

getDefaultView :: Document -> IO (Maybe Window)
getDefaultView = undefined

pushState :: History -> JSVal -> Text -> Text -> IO ()
pushState = undefined


#endif
