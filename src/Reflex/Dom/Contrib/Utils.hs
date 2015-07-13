{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

module Reflex.Dom.Contrib.Utils where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           GHCJS.DOM.Types hiding (Event)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------


------------------------------------------------------------------------------
#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
  "confirm($1)"
  js_confirm :: JSString -> IO Bool

foreign import javascript unsafe
  "$1.location.pathname"
  js_windowLocationPath :: JSRef DOMWindow ->  IO JSString

foreign import javascript unsafe
  "window.history.pushState({},\"\",$1)"
  js_windowHistoryPushState :: JSString -> IO ()

#else

js_confirm :: JSString -> IO Bool
js_confirm = error "js_confirm only works in GHCJS."

js_windowLocationPath :: JSRef DOMWindow -> IO JSString
js_windowLocationPath _ =
    error "Window location can only be retrieved in GHCJS."

js_windowHistoryPushState :: JSString -> IO ()
js_windowHistoryPushState =
    error "Window pushState can only be changed in GHCJS."

#endif


------------------------------------------------------------------------------
-- | Convenient function that pops up a javascript confirmation dialog box
-- when an event fires with a message to display.
confirmEvent :: MonadWidget t m => (a -> String) -> Event t a -> m (Event t a)
confirmEvent str e = liftM (fmapMaybe id) $ performEvent (confirm <$> e)
  where
    confirm a = do
        ok <- liftIO $ js_confirm $ toJSString $ str a
        return $ if ok then Just a else Nothing


------------------------------------------------------------------------------
-- | Gets the current path of the DOM Window (i.e., the contents of the
-- address bar after the host, beginning with a "/").
-- https://developer.mozilla.org/en-US/docs/Web/API/Location
getWindowLocationPath :: DOMWindow -> IO String
getWindowLocationPath w = do
    jw <- toJSRef w
    liftM fromJSString $ js_windowLocationPath jw


------------------------------------------------------------------------------
-- | Pushes a new URL to the window history.
windowHistoryPushState :: String -> IO ()
windowHistoryPushState = js_windowHistoryPushState . toJSString


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

