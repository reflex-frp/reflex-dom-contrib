{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.Contrib.Router where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.DOMWindow
import           GHCJS.DOM.HTMLDocument
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.EventM (event, EventM)
import           GHCJS.DOM.UIEvent
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
#endif
import           Prelude hiding (mapM, mapM_, all, sequence)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
setUrl
    :: MonadWidget t m
    => Event t String
    -> m ()
setUrl e = do
    performEvent_ $ ffor e $ \url -> liftIO $ do
        windowHistoryPushState url


------------------------------------------------------------------------------
getWindowLocation :: DOMWindow -> IO String
#ifdef ghcjs_HOST_OS
getWindowLocation w = do
    jw <- toJSRef w
    liftM fromJSString $ js_windowLocationPathname jw

foreign import javascript unsafe
  "$1['location']['pathname']"
  js_windowLocationPathname :: JSRef DOMWindow -> IO JSString
#else
getWindowLocation =
    error "getWindowLocation: only works in GHCJS"
#endif


------------------------------------------------------------------------------
setupHistoryHandler :: DOMWindow -> (String -> IO ()) -> IO ()
#ifdef ghcjs_HOST_OS
setupHistoryHandler w cb = do
    jw <- toJSRef w
    cbRef <- syncCallback1 NeverRetain False (cb . fromJSString)
    js_setupHistoryHandler jw cbRef

foreign import javascript unsafe
  "$1.onpopstate = function(event) { $2($1['location']['pathname']); }"
  js_setupHistoryHandler :: JSRef DOMWindow -> (JSFun (JSString -> IO ())) -> IO ()
#else
setupHistoryHandler =
    error "setupHistoryHandler: only works in GHCJS"
#endif


------------------------------------------------------------------------------
-- | Handles routing for a site.  The argument to this function is a widget
-- function with the effective type signature `String -> m (Event t String)`.
-- The String parameter is the initial value of the window location pathname.
-- The return value is an event that updates the window location.
--routeSite
--    :: (forall t m a. (MonadWidget t m) => (String -> m (Event t String)))
--    -> IO ()
routeSite siteFunc = runWebGUI $ \webView -> do
    w <- waitUntilJust currentWindow
    path <- getWindowLocation w
    --setupHistoryHandler w (\arg -> putStrLn $ "ghcjs history handling!  " ++ arg)
    --wrapDomEvent w domWindowOnpopstate myGetEvent
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
             webViewGetDomDocument webView
    body <- waitUntilJust $ documentGetBody doc
    attachWidget body webView $ do
      changes <- siteFunc path
      setUrl changes
      return ()

--myGetEvent = do
--    e <- event
--    liftIO $ uiEventGetView e
