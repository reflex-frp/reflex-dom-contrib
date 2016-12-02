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
    route
  , route'
  , partialPathRoute

  -- = Low-level URL bar access
  , getLoc
  , getUrlText
  , uriOrigin

  -- = History movement
  , goForward
  , goBack
  ) where

------------------------------------------------------------------------------
import           Control.Lens              ((&), (.~), (^.))
-- import           Control.Monad             ((<=<))
-- import           Control.Monad.Except      (ExceptT(..), lift, runExceptT)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
-- import           Data.Bifunctor            (first)
import qualified Data.ByteString.Char8     as BS
-- import           Data.Maybe                (fromJust, fromMaybe)
import qualified Data.List                 as L
import           Data.Monoid               ((<>))
-- import           Data.Default
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           GHCJS.DOM.Types           (Location(..) )
import           Reflex.Dom                hiding (EventName, Window)
import qualified URI.ByteString            as U
#if ghcjs_HOST_OS
import qualified GHCJS.DOM                 as DOM
import qualified GHCJS.DOM.Document        as DOM
import           GHCJS.DOM.EventM          (on)
import           GHCJS.DOM.History         (History, back, forward, pushState)
import           GHCJS.DOM.Location        (toString)
import           GHCJS.DOM.Window          (Window, getHistory,
                                            getLocation, popState)
import           GHCJS.Marshal.Pure
#else
import           Control.Monad.Reader      (ReaderT)
#endif


type URI = U.URIRef U.Absolute


-------------------------------------------------------------------------------
-- | Manipulate and track the URL 'GHCJS.DOM.Types.Location' for dynamic
--   routing of a widget
--   These sources of URL-bar change will be reflected in the output URI
--     - Input events to 'route'
--     - Browser Forward/Back button clicks
--     - forward/back javascript calls (or 'goForward'/'goBack') Haskell calls
--     - Any URL changes followed by a popState event
--   But external calls to pushState that don't manually fire a popState
--   won't be detected
route
  :: (HasWebView m, MonadWidget t m)
  => Event t T.Text
  -> m (Dynamic t (U.URIRef U.Absolute))
route pushTo = do
  loc0    <- getURI
  _ <- performEvent $ ffor pushTo $ \t -> do
    withHistory $ \h -> pushState h (pToJSVal (0 :: Int)) ("" :: T.Text) t
    liftIO dispatchEvent'

  locUpdates <- getPopState

  -- Route <$> holdDyn locVal0 (leftmost [sSet, locUpdates]) <*> holdDyn loc0 locs
  -- Because we trigger a popState manually, there is no more need to use the
  -- sSet events to update the dynamic uri
  -- TODO make sure this actually works on all the expected URL bar update sources
  holdDyn loc0 locUpdates

route'
  :: forall t m a b. MonadWidget t m
  => (URI -> a -> URI)
  -> (URI -> b)
  -> Event t a
  -> m (Dynamic t b)
route' encode decode routeUpdate = do
  rec rUri <- route (T.decodeUtf8 . U.serializeURIRef' <$> urlUpdates)
      let urlUpdates = attachWith encode (current rUri) routeUpdate
  return $ decode <$> rUri


-------------------------------------------------------------------------------
-- | Route a single page app according to the part of the path after
--   pathBase
partialPathRoute
  :: forall t m. MonadWidget t m
  => [T.Text]  -- ^ The path segments not related to SPA routing
  -> Event t [T.Text] -- ^ Updates to the path segments used for routing
  -> m (Dynamic t [T.Text]) -- ^ Path segments used for routing
partialPathRoute pathBase pathUpdates = do
  route' (flip updateUrl) parseParts pathUpdates
  where

    toPath :: [T.Text] -> BS.ByteString
    toPath parts =
      "/" <> BS.intercalate "/" (T.encodeUtf8 <$> (pathBase <> parts))

    updateUrl :: [T.Text] -> URI -> URI
    updateUrl updateParts u = u & U.pathL .~ toPath updateParts

    parseParts :: URI -> [T.Text]
    parseParts u =
      maybe (error . T.unpack $ pfxErr u pathBase) (map T.decodeUtf8) .
      L.stripPrefix (T.encodeUtf8 <$> pathBase) .
      BS.split '/' . BS.drop 1 $ u ^. U.pathL


-------------------------------------------------------------------------------
uriOrigin :: U.URIRef U.Absolute -> T.Text
uriOrigin r = T.decodeUtf8 $ U.serializeURIRef' r'
  where
    r' = r { U.uriPath = mempty
           , U.uriQuery = mempty
           , U.uriFragment = mempty
           }


-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
getPopState :: (MonadWidget t m) => m (Event t URI)
getPopState = do
  window <- askDomWindow
  wrapDomEventMaybe window (`on` popState) $ liftIO $ do
    Just loc <- getLocation window
    locStr <- toString loc
    return . hush $ U.parseURI U.laxURIParserOptions (T.encodeUtf8 locStr)


-------------------------------------------------------------------------------
goForward :: (HasWebView m, MonadIO m) => m ()
goForward = withHistory forward


-------------------------------------------------------------------------------
goBack :: (HasWebView m, MonadIO m) => m ()
goBack = withHistory back


-------------------------------------------------------------------------------
withHistory :: (HasWebView m, MonadIO m) => (History -> IO a) -> m a
withHistory act = do
  Just h <- liftIO . getHistory =<< askDomWindow
  liftIO $ act h



-------------------------------------------------------------------------------
-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLoc :: (HasWebView m, MonadIO m) => m Location
#if ghcjs_HOST_OS
getLoc = do
  Just win <- liftIO . getLocation =<< askDomWindow
  return win
#else
getLoc = error "getLocation' is only available to ghcjs"
#endif


-------------------------------------------------------------------------------
-- | (Unsafely) get the URL text of a window
getUrlText :: (HasWebView m, MonadIO m) => m T.Text
getUrlText = getLoc >>= liftIO . toString


getURI :: (HasWebView m, MonadIO m) => m URI
getURI = do
  l <- getUrlText
  return $ either (error "No parse of window location") id .
    U.parseURI U.laxURIParserOptions $ T.encodeUtf8 l


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


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

pfxErr :: URI -> [T.Text] -> T.Text
pfxErr pn pathBase = "Encountered path (" <> T.decodeUtf8 (U.serializeURIRef' pn)
            <> ") without expected prefix (" <> T.intercalate "/" pathBase <> ")"
