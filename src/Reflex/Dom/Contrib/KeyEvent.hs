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
  , Reflex.Dom.Contrib.KeyEvent.getKeyEvent
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Char
import           GHCJS.DOM.EventM (event, EventM)
import           GHCJS.DOM.Types hiding (Event)
#ifdef ghcjs_HOST_OS
import           GHCJS.Types
#endif
import           Reflex.Dom
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
getKeyEvent :: EventM e KeyboardEvent KeyEvent
#ifdef ghcjs_HOST_OS
getKeyEvent = do
  e <- event
  code <- Reflex.Dom.getKeyEvent
  liftIO $ KeyEvent <$> pure code
                    <*> js_uiEventGetCtrlKey (unKeyboardEvent e)
                    <*> js_uiEventGetShiftKey (unKeyboardEvent e)

foreign import javascript unsafe "$1['ctrlKey']"
  js_uiEventGetCtrlKey :: JSVal -> IO Bool

foreign import javascript unsafe "$1['shiftKey']"
  js_uiEventGetShiftKey :: JSVal -> IO Bool
#else
getKeyEvent = error "getKeyEvent: can only be used with GHCJS"
#endif

