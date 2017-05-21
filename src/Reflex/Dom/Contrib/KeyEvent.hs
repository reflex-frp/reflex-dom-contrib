{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

{-|

API for dealing with keyboard events.

-}

module Reflex.Dom.Contrib.KeyEvent
  ( KeyEvent(..)
  , key
  , shift
  , ctrlKey
#if ghcjs_HOST_OS || GHCSTUBS
  , Reflex.Dom.Contrib.KeyEvent.getKeyEvent
#endif
  ) where

------------------------------------------------------------------------------
import           Data.Char
import           GHCJS.DOM.EventM (EventM)
import           GHCJS.DOM.Types hiding (Event)
#ifdef ghcjs_HOST_OS
import           Control.Monad.Trans
import           GHCJS.DOM.EventM (event)
import           Reflex.Dom.Core
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Data structure with the details of key events.
data KeyEvent = KeyEvent
   { keKeyCode :: Int
   , keCtrl :: Bool
   , keShift :: Bool
   } deriving (Show, Read, Eq, Ord)


------------------------------------------------------------------------------
-- | Convenience constructor for KeyEvent with no modifiers pressed.
key :: Char -> KeyEvent
key k = KeyEvent
   { keKeyCode = ord k
   , keCtrl = False
   , keShift = False
   }


------------------------------------------------------------------------------
-- | Set the shift modifier of a KeyEvent.
shift :: KeyEvent -> KeyEvent
shift ke = ke { keShift = True }


------------------------------------------------------------------------------
-- | Set the ctrl modifier of a KeyEvent.
ctrlKey :: Char -> KeyEvent
ctrlKey k = (key $ toUpper k) { keCtrl = True }


------------------------------------------------------------------------------
#ifdef ghcjs_HOST_OS
getKeyEvent :: EventM e KeyboardEvent KeyEvent
getKeyEvent = do
  e <- event
  code <- Reflex.Dom.Core.getKeyEvent
  liftIO $ KeyEvent <$> pure (fromIntegral code)
                    <*> js_uiEventGetCtrlKey (unKeyboardEvent e)
                    <*> js_uiEventGetShiftKey (unKeyboardEvent e)

foreign import javascript unsafe "$1['ctrlKey']"
  js_uiEventGetCtrlKey :: JSVal -> IO Bool

foreign import javascript unsafe "$1['shiftKey']"
  js_uiEventGetShiftKey :: JSVal -> IO Bool
#elif GHCSTUBS
getKeyEvent :: EventM e KeyboardEvent KeyEvent
getKeyEvent = error "getKeyEvent: can only be used with GHCJS"
#endif

