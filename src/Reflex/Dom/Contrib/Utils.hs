{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleContexts         #-}

{-|

Misc reflex-dom helper functions.

-}

module Reflex.Dom.Contrib.Utils
  ( tshow
  , alertEvent
  , js_alert
  , confirmEvent
  , js_confirm
  , getWindowLocationPath
  , windowHistoryPushState
  , setWindowLoc
  , widgetHoldHelper
  , putDebugLn
  , putDebugLnE
  , listWithKeyAndSelection
  , waitUntilJust
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Map               (Map)
import           Data.Text              (Text)
import qualified Data.Text           as T
import           GHCJS.DOM.Types hiding (Text, Event)
#ifdef ghcjs_HOST_OS
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Dom      hiding (Window, fromJSString)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Helper function for showing Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

------------------------------------------------------------------------------
-- | Convenient function that pops up a javascript alert dialog box when an
-- event fires with a message to display.
alertEvent :: MonadWidget t m => (a -> String) -> Event t a -> m ()
#ifdef ghcjs_HOST_OS
alertEvent str e = performEvent_ (alert <$> e)
  where
    alert a = liftIO $ js_alert $ toJSString $ str a

foreign import javascript unsafe
  "alert($1)"
  js_alert :: JSString -> IO ()
#else
alertEvent = error "alertEvent: can only be used with GHCJS"
js_alert = error "js_alert: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
-- | Convenient function that pops up a javascript confirmation dialog box
-- when an event fires with a message to display.
confirmEvent :: MonadWidget t m => (a -> String) -> Event t a -> m (Event t a)
#ifdef ghcjs_HOST_OS
confirmEvent str e = liftM (fmapMaybe id) $ performEvent (confirm <$> e)
  where
    confirm a = do
        ok <- liftIO $ js_confirm $ toJSString $ str a
        return $ if ok then Just a else Nothing

foreign import javascript unsafe
  "confirm($1)"
  js_confirm :: JSString -> IO Bool
#else
confirmEvent = error "confirmEvent: can only be used with GHCJS"
js_confirm = error "js_confirm: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
-- | Gets the current path of the DOM Window (i.e., the contents of the
-- address bar after the host, beginning with a "/").
-- https://developer.mozilla.org/en-US/docs/Web/API/Location
getWindowLocationPath :: Window -> IO String
#ifdef ghcjs_HOST_OS
getWindowLocationPath w = do
    liftM fromJSString $ js_windowLocationPath $ unWindow w

foreign import javascript unsafe
  "$1['location']['pathname']"
  js_windowLocationPath :: JSVal ->  IO JSString
#else
getWindowLocationPath = error "getWindowLocationPath: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
-- | Pushes a new URL to the window history.
windowHistoryPushState :: String -> IO ()
#ifdef ghcjs_HOST_OS
windowHistoryPushState = js_windowHistoryPushState . toJSString

foreign import javascript unsafe
  "window['history']['pushState']({},\"\",$1)"
  js_windowHistoryPushState :: JSString -> IO ()
#else
windowHistoryPushState = error "windowHistoryPushState: can only be used with GHCJS"
#endif

setWindowLoc :: String -> IO ()
#ifdef ghcjs_HOST_OS
setWindowLoc = js_setWindowLoc . toJSString

foreign import javascript unsafe
  "window['location'] = window['location']['origin'] + $1;"
  js_setWindowLoc :: JSString -> IO ()
#else
setWindowLoc = error "setWindowLoc: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
-- | A common form for widgetHold calls that mirrors the pattern seen in hold
-- and holdDyn.
widgetHoldHelper
    :: MonadWidget t m
    => (a -> m b)
    -> a
    -> Event t a
    -> m (Dynamic t b)
widgetHoldHelper f eDef e = widgetHold (f eDef) (f <$> e)


------------------------------------------------------------------------------
-- | Simple debug function that prints a message on postBuild.
putDebugLn :: MonadWidget t m => String -> m ()
putDebugLn str = do
    pb <- getPostBuild
    putDebugLnE pb (const str)


------------------------------------------------------------------------------
-- | Prints a string when an event fires.  This differs slightly from
-- traceEvent because it will print even if the event is otherwise unused.
putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
    performEvent_ (liftIO . putStrLn . mkStr <$> e)


------------------------------------------------------------------------------
-- | A generalized version of the one in reflex-dom.
listWithKeyAndSelection
    :: forall t m k v a. (MonadWidget t m, Ord k)
    => Dynamic t k
    -> Dynamic t (Map k v)
    -> (k -> Dynamic t v -> Dynamic t Bool -> m a)
    -> m (Dynamic t (Map k a))
listWithKeyAndSelection selection vals mkChild = do
  let selectionDemux = demux selection
  listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    mkChild k v selected


------------------------------------------------------------------------------
-- | Simple utility function to robustly get things like the current window,
-- DOM document, document body, etc.
waitUntilJust :: IO (Maybe a) -> IO a
waitUntilJust a = do
    mx <- a
    case mx of
      Just x -> return x
      Nothing -> do
        threadDelay 10000
        waitUntilJust a
