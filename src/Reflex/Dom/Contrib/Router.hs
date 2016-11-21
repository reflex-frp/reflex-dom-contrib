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
import           Data.Bifunctor
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import Data.Monoid ((<>))
import           Data.Default
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Reflex.Dom                hiding (Window)
import qualified Web.Routes.PathInfo       as WR
#if ghcjs_HOST_OS
import           Data.Maybe                (fromJust)
import           Control.Monad.IO.Class    (liftIO)
import qualified GHCJS.DOM                 as DOM
import qualified GHCJS.DOM.Document        as DOM
import           GHCJS.DOM.EventM          (on)
import           GHCJS.DOM.History         (back, forward, pushState)
import           GHCJS.DOM.Location        (toString)
import           GHCJS.DOM.Window          (Window, getHistory,
                                            getLocation, getPathname, popState)
import           GHCJS.Marshal.Pure
#else
#endif

------------------------------------------------------------------------------
data RouteConfig t a = RouteConfig
  { _routeConfig_forward   :: Event t () -- ^ Move the browser history forward
  , _routeConfig_back      :: Event t () -- ^ Move the browser history back
  , _routeConfig_pushState :: Event t a  -- ^ Push to the URL state
  , _routeConfig_pathBase  :: T.Text
  --   -- ^ The part of the URL not related to SPA routing
  }

instance Reflex t => Default (RouteConfig t a) where
  def = RouteConfig never never never ""

data Route t a = Route {
    _route_value :: Dynamic t (Either T.Text a) -- ^ URL value
  }

instance HasValue (Route t a) where
  type Value (Route t a) = Dynamic t (Either T.Text a)
  value = _route_value

-- | Manipulate and track the URL text for dynamic routing of a widget
route
  :: (HasWebView m, MonadWidget t m)
  => (T.Text -> Either T.Text a)
  -> (a -> T.Text)
  -> RouteConfig t a
  -> m (Route t a)
-- #if ghcjs_HOST_OS
route to from (RouteConfig goForward goBack sSet pBase) = do
  win <- askDomWindow
  path0 :: T.Text <- liftIO $ getLocation win >>= getPathname . fromJust
  let pathVal0 = parsePath path0
  Just hist <- liftIO $ getHistory win
  performEvent_ $ ffor goForward $ \_ -> liftIO (forward hist)
  performEvent_ $ ffor goBack    $ \_ -> liftIO (back hist)

  setLoc :: Event t () <- performEvent $ ffor sSet $ \t -> liftIO $ do
    -- let newPath = 
    pushState hist (pToJSVal (0 :: Int)) ("" :: T.Text) (pBase <> from t)
    -- path <- getLocation win >>= getPathname . fromJust
    -- l <- getLocation win
    -- return $ parsePath path

  newLocs <- getPopState
  let locVals = ffor newLocs $ \l ->
        note "Bad path prefix" (T.stripPrefix pBase l) >>= to
  Route <$> holdDyn pathVal0 (leftmost [Right <$> sSet, locVals])

  where parsePath l = note "Bad path prefix" (T.stripPrefix pBase l) >>= to
  -- Route <$> undefined
-- #else
-- route = error "route is only available to ghcjs"
-- #endif

note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e _        = Left e

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing

webRoute
  :: (HasWebView m, MonadWidget t m, WR.PathInfo a)
  => RouteConfig t a
  -> m (Route t a)
webRoute = route (first T.pack . WR.fromPathInfo . T.encodeUtf8)
                 (WR.toPathInfo)


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

getLocation' :: MonadIO m => Window -> m T.Text
#if ghcjs_HOST_OS
getLocation' w = toString . fromJust =<< liftIO (getLocation w)
#else
getLocation' = error "getLocation' is only available to ghcjs"
#endif

getPopState :: (MonadWidget t m) => m (Event t T.Text)
-- #if ghcjs_HOST_OS
getPopState = do
  window <- askDomWindow
  wrapDomEventMaybe window (`on` popState) $ do
    l <- liftIO $ getLocation window
    case l of
      Nothing -> return Nothing
      Just loc -> liftIO $ Just <$> getPathname loc
-- #else
-- getPopState = error "getPopState is only available to ghcjs"
-- #endif

setWindowUrl :: MonadWidget t m => Event t T.Text -> m ()
#if ghcjs_HOST_OS
setWindowUrl url = do
  performEvent_ $ ffor url $ \u -> do
    win <- askDomWindow
    Just hist <- liftIO $ getHistory win
    pushState hist (pToJSVal (0 :: Int)) ("" :: T.Text) u
#else
setWindowUrl = error "setWindowUrl only available to ghcjs"
#endif

getWindowInitUrl :: MonadWidget t m => m T.Text
getWindowInitUrl = getLocation' =<< askDomWindow

getWindowUrl :: MonadWidget t m => m (Dynamic t T.Text)
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

getPathname :: Location -> IO T.Text
getPathname = undefined

getHistory :: Window -> IO (Maybe History)
getHistory = undefined

getState :: History -> IO (Maybe SerializedScriptValue)
getState = undefined

toString :: Location -> IO T.Text
toString = undefined

getDefaultView :: Document -> IO (Maybe Window)
getDefaultView = undefined

pushState :: History -> JSVal -> T.Text -> T.Text -> IO ()
pushState = undefined

popState = undefined

pToJSVal :: Int -> JSVal
pToJSVal = undefined

on = undefined
#endif
