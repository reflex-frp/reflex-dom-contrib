{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE LambdaCase          #-}
-- | This module provides a method of expressing widgets that depend on an external javascript source.
module Reflex.Dom.Contrib.Widgets.ScriptDependent
  ( widgetHoldUntilDefined
  ) where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Monad               (void)
import           Control.Monad.Fix
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Reflex.Dom.Core
#ifdef ghcjs_HOST_OS
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM (atomically)
import           Data.Function               (fix)
import           Data.Time.Clock             (NominalDiffTime, diffUTCTime, getCurrentTime)
import           GHCJS.Foreign.Callback
import           Data.JSString (JSString, pack)
#endif


#ifdef ghcjs_HOST_OS

------------------------------------------------------------------------------
-- | A 15 second timeout.
timeout :: NominalDiffTime
timeout = 15.0


------------------------------------------------------------------------------
-- | Determines if a symbol is defined on the window.
foreign import javascript unsafe
 "(function () { \
   return (typeof window[$1] != 'undefined'); \
  })()"
  definedInWindow_js :: JSString -> IO Bool


------------------------------------------------------------------------------
-- | Determines if a symbol is defined on the window.
definedInWindow :: String -> IO Bool
definedInWindow = definedInWindow_js . pack


------------------------------------------------------------------------------
-- | Does a script tag injection.
foreign import javascript unsafe
  "(function () { \
     var script = document.createElement('script'); \
         script.type   = 'text/javascript'; \
         script.async  = true; \
         script.onload = function () { \
           console.log('loaded script ' + $1); \
           $2(); \
         }; \
         script.src = $1; \
     console.log('loading script ' + $1); \
     document.getElementsByTagName('head')[0].appendChild(script); \
   })()"
  injectScript_js :: JSString -> Callback (IO ()) -> IO ()


------------------------------------------------------------------------------
-- | Inject a script into the current document and wait for it to load or timeout.
-- The timeout is set to 15 seconds.
injectScript :: String -> IO () -> IO ()
injectScript url innerCB = do
  tvar    <- newTVarIO False
  outerCB <- asyncCallback $ atomically $ writeTVar tvar True
  start   <- getCurrentTime
  injectScript_js (pack url) outerCB
  void $ forkIO $ fix $ \loop -> readTVarIO tvar >>= \case
    True  -> releaseCallback outerCB >> innerCB
    False -> do
      threadDelay 10
      now <- getCurrentTime
      if diffUTCTime now start < timeout
        then loop
        else releaseCallback outerCB >> innerCB


#else


------------------------------------------------------------------------------
injectScript :: String -> IO () -> IO ()
injectScript _ innerCB = void $ forkIO $ threadDelay 10 >> innerCB


------------------------------------------------------------------------------
definedInWindow :: String -> IO Bool
definedInWindow = const $ return False


#endif


------------------------------------------------------------------------------
-- | Given a symbol and an event of some url, load the script at that url and
-- proc an event when loading is complete, or the load times out, whichever
-- happens first.
-- If the given symbol already exists, short circuit and proc immediately.
injectScriptEvent
  :: (MonadIO (Performable m), PerformEvent t m, TriggerEvent t m)
  => String
  -> Event t String
  -> m (Event t ())
injectScriptEvent symbol evUrl = performEventAsync $ ffor evUrl $ \url cb ->
  liftIO $ definedInWindow symbol >>= \case
    False -> injectScript url $ cb ()
    True  -> cb ()


------------------------------------------------------------------------------
-- | Render a placeholder widget until a given symbol exists (in the window). If
-- the symbol has already been loaded, this will switch to the main widget
-- immediately. If the symbol is not loaded this will load a script using script
-- tag injection, wait for the load to complete and then switch to the main
-- widget. This switch happens whether or not the symbol gets defined by the
-- script, so make sure you have the correct symbol!
widgetHoldUntilDefined
  :: (MonadIO (Performable m), PerformEvent t m,
      TriggerEvent t m, DomBuilder t m, MonadHold t m,
      MonadFix m)
  => String
  -- ^ Symbol, for instance the value "dhtmlxCalendarObject", as in
  -- window.dhtmlxCalendarObject
  -> Event t String
  -- ^ An event of the url to load
  -> m a
  -- ^ The placeholder widget
  -> m a
  -- ^ The main widget
  -> m (Dynamic t a)
widgetHoldUntilDefined symbol evUrl placeholder widget = do
  evLoaded <- injectScriptEvent symbol =<< debounce 0.75 evUrl
  widgetHold placeholder (widget <$ evLoaded)
