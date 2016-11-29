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

module Reflex.Dom.Contrib.Router (
  -- == High-level routers
    webRoute
  , textRoute
  , route

  -- == Router configuration
  , RouteConfig(..)

  -- == Routing result
  , Route(..)

  -- == Lenses
  , routeConfig_forward
  , routeConfig_back
  , routeConfig_pushState
  , routeConfig_pathBase
  , route_value

  ) where

------------------------------------------------------------------------------
import           Control.Lens              (makeLenses)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bifunctor            (first)
import           Data.Monoid ((<>))
import           Data.Default
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Reflex.Dom                hiding (EventName, Window)
import qualified Web.Routes.PathInfo       as WR
#if ghcjs_HOST_OS
-- import           Control.Monad.IO.Class    (liftIO)
import qualified GHCJS.DOM                 as DOM
import qualified GHCJS.DOM.Document        as DOM
import           GHCJS.DOM.EventM          (on)
import           GHCJS.DOM.History         (back, forward, pushState)
import           GHCJS.DOM.Location        (getPathname)
import           GHCJS.DOM.Window          (Window, getHistory,
                                            getLocation, popState)
import           GHCJS.Marshal.Pure
#else
import           Control.Monad.Reader      (ReaderT)
#endif

------------------------------------------------------------------------------
data RouteConfig t a = RouteConfig
  { _routeConfig_forward   :: Event t () -- ^ Move the browser history forward
  , _routeConfig_back      :: Event t () -- ^ Move the browser history back
  , _routeConfig_pushState :: Event t a  -- ^ Push to the URL state
  , _routeConfig_pathBase  :: T.Text
    -- ^ The part of the URL not related to SPA routing
  }

makeLenses ''RouteConfig

instance Reflex t => Default (RouteConfig t a) where
  def = RouteConfig never never never ""

data Route t a = Route {
    _route_value :: Dynamic t (Either T.Text a) -- ^ URL value
  }

makeLenses ''Route

instance HasValue (Route t a) where
  type Value (Route t a) = Dynamic t (Either T.Text a)
  value = _route_value

-- | Manipulate and track the URL text for dynamic routing of a widget
route
  :: (HasWebView m, MonadWidget t m)
  => (T.Text -> Either T.Text a)
     -- ^ Decode the part of the path beyond '_routeConfig_pathBase' into an a
  -> (a -> T.Text)
     -- ^ Encode the routing value
  -> RouteConfig t a
    -- ^ Routing widget configuration
  -> m (Route t a)
-- #if ghcjs_HOST_OS
route to from (RouteConfig goForward goBack sSet pBase) = do
  win <- askDomWindow
  path0 :: T.Text <- liftIO $ getLocation win >>= getPathname . fromJust
  let pathVal0 = parsePath path0
  Just hist <- liftIO $ getHistory win
  performEvent_ $ ffor goForward $ \_ -> liftIO (forward hist)
  performEvent_ $ ffor goBack    $ \_ -> liftIO (back hist)

  _ <- performEvent $ ffor sSet $ \t -> liftIO $ do
    pushState hist (pToJSVal (0 :: Int)) ("" :: T.Text) (pBase <> from t)

  newLocs <- getPopState
  let locVals = ffor newLocs $ \l ->
        note "Bad path prefix" (T.stripPrefix pBase l) >>= to
  Route <$> holdDyn pathVal0 (leftmost [Right <$> sSet, locVals])

  where parsePath l = note "Bad path prefix" (T.stripPrefix pBase l) >>= to

-------------------------------------------------------------------------------
-- | Route a single page app according to any 'WR.PathInfo' a => a
webRoute
  :: (MonadWidget t m, WR.PathInfo a)
  => RouteConfig t a
  -> m (Route t a)
webRoute = route (first T.pack . WR.fromPathInfo . T.encodeUtf8)
                 (WR.toPathInfo)


-------------------------------------------------------------------------------
-- | Route a single page app according to the part of the path after
--   pathBase
textRoute
  :: MonadWidget t m
  => RouteConfig t T.Text
  -> m (Route t T.Text)
textRoute = route Right id


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


getPopState :: (MonadWidget t m) => m (Event t T.Text)
-- #if ghcjs_HOST_OS
getPopState = do
  window <- askDomWindow
  wrapDomEventMaybe window (`on` popState) $ do
    l <- liftIO $ getLocation window
    case l of
      Nothing -> return Nothing
      Just loc -> liftIO $ Just <$> getPathname loc


#if ghcjs_HOST_OS
#else
data Location
data Window
data JSVal
data History

forward :: History -> IO ()
forward = undefined

back :: History -> IO ()
back = undefined

getLocation :: Window -> IO (Maybe Location)
getLocation = undefined

getPathname :: Location -> IO T.Text
getPathname = undefined

getHistory :: Window -> IO (Maybe History)
getHistory = undefined

pushState :: History -> JSVal -> T.Text -> T.Text -> IO ()
pushState = undefined

popState :: EventName Window PopStateEvent
popState = undefined

pToJSVal :: Int -> JSVal
pToJSVal = undefined

on :: Window -> EventName t e -> EventM t e () -> IO (IO ())
on = undefined

type EventM t e = ReaderT e IO
data PopStateEvent
data EventName t e
#endif

-------------------------------------------------------------------------------
-- | Helper functions
note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e _        = Left e
