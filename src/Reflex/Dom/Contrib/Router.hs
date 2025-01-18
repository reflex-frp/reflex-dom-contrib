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
  , getURI
  , getUrlText
  , uriOrigin
  , URI

  -- = History movement
  , goForward
  , goBack
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Fix             (MonadFix)
import           Control.Lens                  ((&), (.~), (^.))
import qualified Data.ByteString.Char8         as BS
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHCJS.Foreign                 (isFunction)
import           GHCJS.DOM.Types               (Location(..), PopStateEvent(..))
import qualified GHCJS.DOM.Types               as DOM
import           Reflex.Dom.Core               hiding (EventName, Window)
import qualified URI.ByteString                as U
import           GHCJS.DOM.Types               (uncheckedCastTo, MonadJSM)
import           GHCJS.DOM.History             (History, back, forward, pushState)
import           GHCJS.DOM                     (currentWindowUnchecked, currentDocumentUnchecked)
import           GHCJS.DOM.Document            (createEvent)
import           GHCJS.DOM.Event               (initEvent)
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.EventTarget         (dispatchEvent_)
import           GHCJS.DOM.Location            (getHref)
import           GHCJS.DOM.PopStateEvent
import           GHCJS.DOM.Window              (getHistory, getLocation)
#if MIN_VERSION_ghcjs_dom(0,8,0)
import           GHCJS.DOM.WindowEventHandlers (popState)
#else
import           GHCJS.DOM.Window              (popState)
#endif
import           GHCJS.Marshal.Pure            (pFromJSVal)
import qualified Language.Javascript.JSaddle   as JS
import           Language.Javascript.JSaddle   (ghcjsPure, JSM, liftJSM, Object(..))
------------------------------------------------------------------------------


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
 :: forall t m.
  ( MonadHold t m
  , PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadJSM m
  , MonadJSM (Performable m))
  => Event t T.Text
  -> m (Dynamic t (U.URIRef U.Absolute))
route pushTo = do
  loc0    <- getURI

  _ <- performEvent $ ffor pushTo $ \t -> do
    let newState =
#if MIN_VERSION_ghcjs_dom(0,8,0)
          Just t
#else
          t
#endif
    withHistory $ \h -> pushState h (0 :: Double) ("" :: T.Text) (newState :: Maybe T.Text)
    liftJSM dispatchEvent'

  locUpdates <- getPopState
  holdDyn loc0 locUpdates

route'
 :: forall t m a b.
  ( MonadHold t m
  , PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , MonadFix m)
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
 :: forall t m.
  ( MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , MonadFix m)
  => T.Text  -- ^ The path segments not related to SPA routing
             --   (leading '/' will be added automaticaly)
  -> Event t T.Text -- ^ Updates to the path segments used for routing
                    --   These values will be appended to the base path
  -> m (Dynamic t [T.Text]) -- ^ Path segments used for routing
partialPathRoute pathBase pathUpdates = do
  route' (flip updateUrl) parseParts pathUpdates
  where

    toPath :: T.Text -> BS.ByteString
    toPath dynpath = T.encodeUtf8 $
      "/" <> cleanT pathBase <>
      "/" <> cleanT dynpath

    updateUrl :: T.Text -> URI -> URI
    updateUrl updateParts u = u & U.pathL .~ toPath updateParts

    parseParts :: URI -> [T.Text]
    parseParts u =
      maybe (error $ pfxErr u pathBase)
            (T.splitOn "/" . T.decodeUtf8 . cleanB) .
      BS.stripPrefix (T.encodeUtf8 $ cleanT pathBase) $
      cleanB (u ^. U.pathL)

    cleanT = T.dropWhile (=='/')
    cleanB = BS.dropWhile (== '/')


-------------------------------------------------------------------------------
uriOrigin :: U.URIRef U.Absolute -> T.Text
uriOrigin r = T.decodeUtf8 $ U.serializeURIRef' r'
  where
    r' = r { U.uriPath = mempty
           , U.uriQuery = mempty
           , U.uriFragment = mempty
           }


-------------------------------------------------------------------------------
getPopState
 :: forall t m.
  ( MonadHold t m
  , TriggerEvent t m
  , MonadJSM m) => m (Event t URI)
getPopState = do
  window <- currentWindowUnchecked
  wrapDomEventMaybe window (`on` popState) $ do
#if MIN_VERSION_ghcjs_dom(0,8,0)
    loc
#else
    Just loc
#endif
      <- getLocation window
    locStr <- getHref loc
    return . hush $ U.parseURI U.laxURIParserOptions (T.encodeUtf8 locStr)


-------------------------------------------------------------------------------
goForward :: MonadJSM m => m ()
goForward = withHistory forward


-------------------------------------------------------------------------------
goBack :: MonadJSM m => m ()
goBack = withHistory back


-------------------------------------------------------------------------------
withHistory :: MonadJSM m => (History -> m a) -> m a
withHistory act = do
  w <- currentWindowUnchecked
#if MIN_VERSION_ghcjs_dom(0,8,0)
  h
#else
  Just h
#endif
    <- getHistory w
  act h


-------------------------------------------------------------------------------
-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLoc :: MonadJSM m => m Location
getLoc = do
  win <- currentWindowUnchecked
#if MIN_VERSION_ghcjs_dom(0,8,0)
  loc
#else
  Just loc
#endif
    <- getLocation win
  return loc


-------------------------------------------------------------------------------
-- | (Unsafely) get the URL text of a window
getUrlText :: MonadJSM m => m T.Text
getUrlText = getLoc >>= getHref


-------------------------------------------------------------------------------
type URI = U.URIRef U.Absolute


-------------------------------------------------------------------------------
getURI :: MonadJSM m => m URI
getURI = do
  l <- getUrlText
  return $ either (error "No parse of window location") id .
    U.parseURI U.laxURIParserOptions $ T.encodeUtf8 l


dispatchEvent' :: JSM ()
dispatchEvent' = do
  window <- currentWindowUnchecked
  obj@(Object o) <- JS.create
  JS.objSetPropertyByName obj ("cancelable" :: Text) True
  JS.objSetPropertyByName obj ("bubbles" :: Text) True
  JS.objSetPropertyByName obj ("view" :: Text) window
  event <- JS.jsg ("PopStateEvent" :: Text) >>= ghcjsPure . isFunction >>= \case
    True -> newPopStateEvent ("popstate" :: Text) $ Just $ pFromJSVal o
    False -> do
      doc <- currentDocumentUnchecked
      event <- createEvent doc ("PopStateEvent" :: Text)
      initEvent event ("popstate" :: Text) True True
      JS.objSetPropertyByName obj ("view" :: Text) window
      return $ uncheckedCastTo PopStateEvent event

  dispatchEvent_ window event


-------------------------------------------------------------------------------
hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing


-------------------------------------------------------------------------------
pfxErr :: URI -> T.Text -> String
pfxErr pn pathBase =
  T.unpack $ "Encountered path (" <> T.decodeUtf8 (U.serializeURIRef' pn)
            <> ") without expected prefix (" <> pathBase <> ")"
