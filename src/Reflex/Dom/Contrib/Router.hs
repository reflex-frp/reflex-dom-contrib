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
import           Control.Monad
import           Control.Monad.Trans
import           GHCJS.DOM
import           GHCJS.DOM.Document hiding (error)
import           GHCJS.DOM.Types (unWindow)
import           GHCJS.DOM.Window hiding (error)
import           GHCJS.DOM.HTMLDocument
#ifdef ghcjs_HOST_OS
import GHCJS.Foreign.Callback
import           GHCJS.Types
import           GHCJS.Prim
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
getWindowLocation :: Window -> IO String
#ifdef ghcjs_HOST_OS
getWindowLocation w = do
    liftM fromJSString $ js_windowLocationPathname (unWindow w)

foreign import javascript unsafe
  "$1['location']['pathname']"
  js_windowLocationPathname :: JSVal -> IO JSVal
#else
getWindowLocation =
    error "getWindowLocation: only works in GHCJS"
#endif


------------------------------------------------------------------------------
setupHistoryHandler :: Window -> (String -> IO ()) -> IO ()
#ifdef ghcjs_HOST_OS
setupHistoryHandler w cb = do
    cbRef <- syncCallback1 ThrowWouldBlock (cb . fromJSString)
    js_setupHistoryHandler (unWindow w) cbRef

foreign import javascript unsafe
  "$1.onpopstate = function(event) { $2($1['location']['pathname']); }"
  js_setupHistoryHandler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
#else
setupHistoryHandler =
    error "setupHistoryHandler: only works in GHCJS"
#endif


------------------------------------------------------------------------------
-- | Handles routing for a site.  The argument to this function is a widget
-- function with the effective type signature `String -> m (Event t String)`.
-- The String parameter is the initial value of the window location pathname.
-- The return value is an event that updates the window location.
routeSite
    :: (forall t m. MonadWidget t m => String -> m (Event t String))
    -> IO ()
routeSite siteFunc = routeSiteWithHead (\_ -> return ()) siteFunc

------------------------------------------------------------------------------
-- | Handles routing for a site. The 1st argument is head widget and
-- the second one is a widget function with the effective type
-- signature `String -> m (Event t String)`.  The String parameter is
-- the initial value of the window location pathname.  The return
-- value is an event that updates the window location.
--routeSiteWithHead
--  :: forall t m m1.
--     (MonadWidget t m, m1 ~ m)
--  => (String -> m1 ()) -> (String -> m (Event t String)) -> IO ()
routeSiteWithHead h siteFunc = runWebGUI $ \webView -> do
    w <- waitUntilJust currentWindow
    path <- getWindowLocation w
    --setupHistoryHandler w (\arg -> putStrLn $ "ghcjs history handling!  " ++ arg)
    --wrapDomEvent w domWindowOnpopstate myGetEvent
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
             webViewGetDomDocument webView
    head <- waitUntilJust $ getHead doc
    _ <- attachWidget head webView $ h path
    body <- waitUntilJust $ getBody doc
    attachWidget body webView $ do
      changes <- siteFunc path
      setUrl changes
      return ()

--myGetEvent = do
--    e <- event
--    liftIO $ uiEventGetView e
