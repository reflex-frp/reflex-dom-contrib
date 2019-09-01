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
  , widgetHoldHelper
  , putDebugLn
  , putDebugLnE
  , listWithKeyAndSelection
  , waitUntilJust
  , alertEvent
  , alertEvents
  , confirmEvent
  , getWindowLocationPath
  , windowHistoryPushState
  , setWindowLoc
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Data.Map               (Map)
import           Data.Text              (Text)
import qualified Data.Text           as T
import           GHCJS.DOM              as DOM
import           GHCJS.DOM.History      as DOM
import           GHCJS.DOM.Location     as DOM
import           GHCJS.DOM.Types hiding (Text, Event)
import qualified GHCJS.DOM.Window       as DOM
import qualified Language.Javascript.JSaddle as JS
import           Reflex
import           Reflex.Dom.Core hiding (Window, fromJSString)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Helper function for showing Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

------------------------------------------------------------------------------
-- | Convenient function that pops up a javascript alert dialog box when an
-- event fires with a message to display.
alertEvent
    :: (PerformEvent t m, MonadJSM m, MonadJSM (Performable m), MonadFail m)
    => (a -> String) -> Event t a -> m ()
alertEvent str e = do
  Just window <- currentWindow
  performEvent_ (DOM.alert window . str <$> e)

------------------------------------------------------------------------------
-- | Convenient function that pops up multiple javascript alert dialog box
-- sequentially when an event fires with messages to display.
alertEvents
    :: (PerformEvent t m, MonadJSM m, MonadJSM (Performable m), MonadFail m)
    => (a -> [String]) -> Event t a -> m ()
alertEvents str e = do
  Just window <- currentWindow
  performEvent_ (mapM_ (DOM.alert window) <$> str <$> e)

------------------------------------------------------------------------------
-- | Convenient function that pops up a javascript confirmation dialog box
-- when an event fires with a message to display.
confirmEvent
    :: (MonadJSM (Performable m), MonadFail (Performable m), PerformEvent t m)
    => (a -> String) -> Event t a -> m (Event t a)
confirmEvent str e = liftM (fmapMaybe id) $ performEvent (confirm <$> e)
  where
    confirm a = do
        Just window <- currentWindow
        ok <- DOM.confirm window $ Just $ str a
        return $ if ok then Just a else Nothing

------------------------------------------------------------------------------
-- | Gets the current path of the DOM Window (i.e., the contents of the
-- address bar after the host, beginning with a "/").
-- https://developer.mozilla.org/en-US/docs/Web/API/Location
getWindowLocationPath :: MonadJSM m => Window -> m String
getWindowLocationPath = DOM.getPathname <=< DOM.getLocation

------------------------------------------------------------------------------
-- | Pushes a new URL to the window history.
windowHistoryPushState :: MonadJSM m => String -> m ()
windowHistoryPushState url = do
  history <- DOM.getHistory =<< DOM.currentWindowUnchecked
  DOM.pushState history JS.create (mempty :: JSString) $ Just url

setWindowLoc :: MonadJSM m => String -> m ()
setWindowLoc url = do
  location <- DOM.getLocation =<< currentWindowUnchecked
  DOM.setHref location url

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
