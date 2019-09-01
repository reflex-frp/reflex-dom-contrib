{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Reflex.Dom.Contrib.Widgets.Modal where

------------------------------------------------------------------------------
import           Data.Bifunctor
import           Data.Either
import           Data.Map (Map)
import           Data.Text (Text)
import           Reflex
import           Reflex.Contrib.Utils
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | The hiding strategies DisplayNone and VisibilityInvisible control
-- visibility by setting the style to @display:none@ or @visibility:invisible@
-- respectively.  @DisplayNone@ causes the elements to remain in the DOM but
-- be taken out of the document flow.  This means that widgets in the modal
-- will only be able to get things like height when the modal is visible.
-- Using @VisibilityInvisible@ makes height information available even when
-- modal is not visible.
data HidingStrategy = DisplayNone
                    | VisibilityInvisible
  deriving (Eq,Show,Ord,Enum,Bounded)


data ModalConfig = ModalConfig
    { modalAttributes     :: Map Text Text
    -- ^ Attributes to put on the modal's outermost div
    }


------------------------------------------------------------------------------
-- | Implements a modal that stays in the DOM but is hidden with either
-- visibility:hidden or display:none when not displayed.
hidingModal
  :: MonadWidget t m
  => HidingStrategy
  -> ModalConfig
  -> Event t Bool
  -- ^ Event to open and/or close the model
  -> m (a, Event t ())
  -- ^ Widget rendering the body of the modal.  Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m a
hidingModal strategy cfg showm body = do
    rec let visE = leftmost [showm, False <$ closem]
        (resE, closem) <- go =<< holdDyn False visE
    return resE
  where
    go vis = do
        let attrs = (\b -> modalAttributes cfg <> visibility b) <$> vis
        elDynAttr "div" attrs body

    visibility True = "style" =: "display:block;"
    visibility False =
      case strategy of
        VisibilityInvisible -> "style" =: "visibility:hidden; display:block;"
        DisplayNone -> "style" =: "display:none;"


------------------------------------------------------------------------------
-- | Implements a modal that is removed from the DOM when not displayed.  This
-- involves a widgetHold and therefore this widget uses a different signature
-- than hidingModal that makes the value inside the event available to the
-- function constructing the modal.
removingModal
  :: MonadWidget t m
  => ModalConfig
  -> Event t a
  -- ^ Event to open the model
  -> (a -> m (b, Event t ()))
  -- ^ Widget rendering the body of the modal.  Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m (Dynamic t (Maybe b))
removingModal cfg showm body = do
    rec let visE = leftmost [Just <$> showm, Nothing <$ closem]
        res <- widgetHoldHelper removeFromDOMWrapper Nothing visE
        let resE = fst <$> res
            closem = extractEvent snd res
    return resE
  where
    removeFromDOMWrapper Nothing = return (Nothing, never)
    removeFromDOMWrapper (Just a) =
      elAttr "div" (modalAttributes cfg) $
        first Just <$> body a


------------------------------------------------------------------------------
-- | Template for a modal with a header, body, and footer where the header has
-- a close icon and the footer has a cancel and save button.
mkModalBody
    :: MonadWidget t m
    => m (Event t ())
    -- ^ A header widget returning an event that closes the modal.
    -> (Dynamic t (Either e a) -> m (Event t (), Event t ()))
    -- ^ Footer widget that takes the current state of the body and returns
    -- a pair of a cancel event and an ok event.
    -> m (Dynamic t (Either e a))
    -> m (Event t (Either e a), Event t ())
mkModalBody header footer body = do
    divClass "modal-dialog" $ divClass "modal-content" $ do
      dismiss <- header
      bodyRes <- divClass "modal-body" body
      (cancel, ok) <- footer bodyRes
      let resE1 = tag (current bodyRes) ok
      let closem1 = leftmost
            [dismiss, cancel, () <$ ffilter isRight resE1]
      return (resE1, closem1)
