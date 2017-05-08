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
import           Control.Lens                  ((&), (.~), (^.))
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.ByteString.Char8         as BS
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHCJS.DOM.Types               (Location(..))
import           Reflex.Dom                    hiding (EventName, Window)
import qualified URI.ByteString                as U
import           GHCJS.DOM.Types               (MonadJSM, pToJSVal)
import           GHCJS.DOM.History             (History, back, forward, pushState)
import           GHCJS.DOM                     (currentWindow)
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.Location            (getHref)
import           GHCJS.DOM.Window              (getHistory, getLocation)
#if MIN_VERSION_ghcjs_dom(0,8,0)
import           GHCJS.DOM.WindowEventHandlers (popState)
#else
import           GHCJS.DOM.Window              (popState)
#endif
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
  :: (HasJSContext m, MonadWidget t m)
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
    liftIO dispatchEvent'

  locUpdates <- getPopState
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
getPopState :: (MonadWidget t m) => m (Event t URI)
getPopState = do
  Just window <- currentWindow
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
goForward :: (HasJSContext m, MonadJSM m) => m ()
goForward = withHistory forward


-------------------------------------------------------------------------------
goBack :: (HasJSContext m, MonadJSM m) => m ()
goBack = withHistory back


-------------------------------------------------------------------------------
withHistory :: (HasJSContext m, MonadJSM m) => (History -> m a) -> m a
withHistory act = do
  Just w <- currentWindow
#if MIN_VERSION_ghcjs_dom(0,8,0)
  h
#else
  Just h
#endif
    <- getHistory w
  act h


-------------------------------------------------------------------------------
-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLoc :: (HasJSContext m, MonadIO m) => m Location
#if ghcjs_HOST_OS
getLoc = do
#if MIN_VERSION_ghcjs_dom(0,8,0)
  win
#else
  Just win
#endif
    <- liftIO . getLocation =<< currentWindowUnchecked
  return win
#else
getLoc = error "getLocation' is only available to ghcjs"
#endif


-------------------------------------------------------------------------------
-- | (Unsafely) get the URL text of a window
getUrlText :: (HasJSContext m, MonadJSM m) => m T.Text
getUrlText = getLoc >>= getHref


-------------------------------------------------------------------------------
type URI = U.URIRef U.Absolute


-------------------------------------------------------------------------------
getURI :: (HasJSContext m, MonadJSM m) => m URI
getURI = do
  l <- getUrlText
  return $ either (error "No parse of window location") id .
    U.parseURI U.laxURIParserOptions $ T.encodeUtf8 l


#if ghcjs_HOST_OS
foreign import javascript unsafe "w = window; e = new PopStateEvent('popstate',{'view':window,'bubbles':true,'cancelable':true}); w['dispatchEvent'](e);"
  dispatchEvent' :: IO ()
#else
dispatchEvent' :: IO ()
dispatchEvent' = undefined
#endif


-------------------------------------------------------------------------------
hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing


-------------------------------------------------------------------------------
pfxErr :: URI -> T.Text -> String
pfxErr pn pathBase =
  T.unpack $ "Encountered path (" <> T.decodeUtf8 (U.serializeURIRef' pn)
            <> ") without expected prefix (" <> pathBase <> ")"

