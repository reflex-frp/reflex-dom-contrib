{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.Contrib.Router (
  -- == High-level routers
    webRoute
  , partialPathRoute
  , fullUriRoute
  , route

  -- == Router configuration
  , RouteConfig(..)

  -- == Routing result
  , Route(..)

  , uriOrigin

  -- == Lenses
  , routeConfig_forward
  , routeConfig_back
  , routeConfig_pushState
  , route_value
  , route_fullURI

  -- = Low-level URL bar access
  , getLocation'
  , getUrlText'
  ) where

------------------------------------------------------------------------------
import           Control.Lens              (makeLenses)
import           Control.Monad.Except      (ExceptT(..), lift, runExceptT)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Bifunctor            (first)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Default
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           GHCJS.DOM.Types           (Location(..) )
import           Reflex.Dom                hiding (EventName, Window)
import qualified URI.ByteString            as U
import qualified Web.Routes.PathInfo       as WR
#if ghcjs_HOST_OS
import qualified GHCJS.DOM                 as DOM
import qualified GHCJS.DOM.Document        as DOM
import           GHCJS.DOM.EventM          (on)
import           GHCJS.DOM.History         (back, forward, pushState)
import           GHCJS.DOM.Location        (getPathname, toString)
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
  }

makeLenses ''RouteConfig

instance Reflex t => Default (RouteConfig t a) where
  def = RouteConfig never never never


instance Reflex t => Monoid (RouteConfig t a) where
  mempty = def
  mappend (RouteConfig f1 b1 p1) (RouteConfig f2 b2 p2) =
    RouteConfig (mappend f1 f2) (mappend b1 b2) (leftmost [p1, p2])


data Route t a = Route {
    _route_value   :: Dynamic t (Either T.Text a)     -- ^ Routing value
  , _route_fullURI :: Dynamic t (U.URIRef U.Absolute) -- ^ Full URI
  }

makeLenses ''Route

instance HasValue (Route t a) where
  type Value (Route t a) = Dynamic t (Either T.Text a)
  value = _route_value

-------------------------------------------------------------------------------
-- | Manipulate and track the URL 'GHCJS.DOM.Types.Location' for dynamic
--   routing of a widget
route
  :: (HasWebView m, MonadWidget t m)
  => (Location -> IO (Either T.Text a))
     -- ^ Decode the part of the path beyond '_routeConfig_pathBase' into an a
  -> (a -> IO T.Text)
     -- ^ Encode the routing value to a Text that will be passed to `pushState`
  -> RouteConfig t a
    -- ^ Routing widget configuration
  -> m (Route t a)
route to from (RouteConfig goForward goBack sSet) = do
  win     <- askDomWindow
  loc0    <- liftIO $ windowURI win
  locVal0 <- liftIO $ to =<< fmap (fromMaybe (error "No location"))
                                  (getLocation win)

  Just hist <- liftIO $ getHistory win
  performEvent_ $ ffor goForward $ \_ -> liftIO (forward hist)
  performEvent_ $ ffor goBack    $ \_ -> liftIO (back hist)

  _ <- performEvent $ ffor sSet $ \t -> liftIO $ do
    s <- from t
    pushState hist (pToJSVal (0 :: Int)) ("" :: T.Text) s
    dispatchEvent'

  locVals <- getPopState to
  locs <- performEvent $ ffor locVals $ \_ -> liftIO (windowURI win)

  Route <$> holdDyn locVal0 (leftmost [Right <$> sSet, locVals]) <*> holdDyn loc0 locs


windowURI :: Window -> IO (U.URIRef U.Absolute)
windowURI w = do
  l <- fromMaybe (error "Window has no location") <$> getLocation w
  either (error "No parse of window location") id . U.parseURI U.laxURIParserOptions . T.encodeUtf8 <$> toString l

-------------------------------------------------------------------------------
-- | Route a single page app according to any 'WR.PathInfo' a => a
webRoute
  :: (MonadWidget t m, WR.PathInfo a)
  => T.Text     -- ^ The part of the URL not related to SPA routing
  -> RouteConfig t a
  -> m (Route t a)
webRoute pathBase = route decoder (return . (pathBase <>) . WR.toPathInfo)
  where
    decoder l = runExceptT $ do
      pn <- lift $ getPathname l
      ExceptT . return $ first T.pack . WR.fromPathInfo . T.encodeUtf8 =<<
        note (pfxErr pn pathBase) (T.stripPrefix pathBase pn)


-------------------------------------------------------------------------------
-- | Route a single page app according to the part of the path after
--   pathBase
partialPathRoute
  :: MonadWidget t m
  => T.Text    -- ^ The part of the URL not related to SPA routing
  -> RouteConfig t T.Text
  -> m (Route t T.Text)
partialPathRoute pathBase = route decoder (return . (pathBase <>))
  where
    decoder l = (\pn -> maybe (Left $ pfxErr pn pathBase)
                              (Right . id) (T.stripPrefix pathBase pn)
                ) <$> getPathname l


-------------------------------------------------------------------------------
-- | Route a single page app according to the full URI
--   The dynamic returned to you will be a Text of the full URI
--   The route string you pass in will be forwarded as-is to
--   pushState.
fullUriRoute
  :: MonadWidget t m
  => RouteConfig t (U.URIRef U.Absolute)
  -> m (Route t (U.URIRef U.Absolute))
fullUriRoute cfg = do
  route uriDecoder (return . T.decodeUtf8 . U.serializeURIRef') cfg
    where
      uriDecoder = fmap (first (T.pack . show) .
                         U.parseURI U.laxURIParserOptions .
                         T.encodeUtf8) .
                   toString


uriOrigin :: U.URIRef U.Absolute -> T.Text
uriOrigin r = T.decodeUtf8 $ U.serializeURIRef' r'
  where
    r' = r { U.uriPath = mempty
           , U.uriQuery = mempty
           , U.uriFragment = mempty
           }

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


getPopState :: (MonadWidget t m) => (Location -> IO (Either T.Text a)) -> m (Event t (Either T.Text a))
getPopState decodeLocation = do
  window <- askDomWindow
  wrapDomEventMaybe window (`on` popState) $ do
    liftIO $ getLocation window >>= \case
      Nothing -> return Nothing
      Just l  -> Just <$> decodeLocation l


-------------------------------------------------------------------------------
-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLocation' :: (HasWebView m, MonadIO m) => m Location
#if ghcjs_HOST_OS
getLocation' = fromMaybe (error "Window has no Location") <$>
  (askDomWindow >>= liftIO . getLocation)
#else
getLocation' = error "getLocation' is only available to ghcjs"
#endif


-------------------------------------------------------------------------------
-- | (Unsafely) get the URL text of a window
getUrlText' :: (HasWebView m, MonadIO m) => m T.Text
getUrlText' = getLocation' >>= liftIO . toString


#if ghcjs_HOST_OS
foreign import javascript unsafe "w = window; e = new PopStateEvent('popstate',{'view':window,'bubbles':true,'cancelable':true}); w.dispatchEvent(e);"
  dispatchEvent' :: IO ()
#else
data Window
data JSVal
data History

dispatchEvent' :: IO ()
dispatchEvent' = undefined

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

toString :: Location -> IO T.Text
toString = undefined
#endif

-------------------------------------------------------------------------------
-- | Helper functions
note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e _        = Left e


pfxErr :: T.Text -> T.Text -> T.Text
pfxErr pn pathBase = "Encountered path (" <> pn
            <> ") without expected prefix (" <> pathBase <> ")"
