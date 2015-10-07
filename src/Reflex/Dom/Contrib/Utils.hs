{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

{-|

Misc reflex-dom helper functions.

-}

module Reflex.Dom.Contrib.Utils
  ( confirmEvent
  , getWindowLocationPath
  , windowHistoryPushState
  , setWindowLoc
  , widgetHoldHelper
  , putDebugLn
  , putDebugLnE
  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           GHCJS.DOM.Types hiding (Event)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------


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
#endif

------------------------------------------------------------------------------
-- | Gets the current path of the DOM Window (i.e., the contents of the
-- address bar after the host, beginning with a "/").
-- https://developer.mozilla.org/en-US/docs/Web/API/Location
getWindowLocationPath :: DOMWindow -> IO String
#ifdef ghcjs_HOST_OS
getWindowLocationPath w = do
    jw <- toJSRef w
    liftM fromJSString $ js_windowLocationPath jw

foreign import javascript unsafe
  "$1['location']['pathname']"
  js_windowLocationPath :: JSRef DOMWindow ->  IO JSString
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
  "window.location = window['location']['origin'] + $1;"
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

