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
import           Data.Either
import           Data.Map (Map)
import           Data.Monoid
import           Reflex
import           Reflex.Contrib.Utils
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | When the hiding strategy is RemoveFromDOM, the widget adds and removes
-- the modal markup from the DOM when the modal is opened and closed.  For
-- @DisplayNone@ and @VisibilityInvisible@ the modal markup is always kept in
-- the DOM and visibility is controlled by setting the style to @display:none@
-- or @visibility:invisible@ respectively.  @DisplayNone@ causes the elements
-- to be completely taken out of the document flow.  This means that widgets
-- in the modal will only be able to get things like height when the modal is
-- visible.  Using @VisibilityInvisible@ gets around this limitation.
data HidingStrategy = DisplayNone
                    | VisibilityInvisible
                    | RemoveFromDOM
  deriving (Eq,Show,Ord,Enum,Bounded)


data ModalConfig = ModalConfig
    { modalHidingStrategy :: HidingStrategy
    , modalAttributes     :: Map String String
    -- ^ Attributes to put on the modal's outermost div
    }


------------------------------------------------------------------------------
modal
  :: MonadWidget t m
  => ModalConfig
  -> Event t Bool
  -- ^ Event to open and/or close the model
  -> m (Event t a, Event t ())
  -- ^ Widget rendering the body of the modal.  Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m (Event t a)
modal cfg showm body = do
    rec let visE = leftmost [showm, False <$ closem]
        (resE, closem) <- case modalHidingStrategy cfg of
          RemoveFromDOM -> do
            res <- widgetHoldHelper removeFromDOMWrapper False visE
            a <- extractEvent fst res
            b <- extractEvent snd res
            return (a,b)
          _ -> go =<< holdDyn False visE
    return resE
  where
    removeFromDOMWrapper False = return (never, never)
    removeFromDOMWrapper True = go $ constDyn True
    go vis = do
        attrs <- mapDyn (\b -> modalAttributes cfg <> visibility b) vis
        elDynAttr "div" attrs body

    visibility True = "style" =: "display:block;"
    visibility False =
      case modalHidingStrategy cfg of
        VisibilityInvisible -> "style" =: "visibility:hidden; display:block;"
        DisplayNone -> "style" =: "display:none;"
        RemoveFromDOM -> mempty


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
      let resE1 = tagDyn bodyRes ok
      let closem1 = leftmost
            [dismiss, cancel, () <$ ffilter isRight resE1]
      return (resE1, closem1)


