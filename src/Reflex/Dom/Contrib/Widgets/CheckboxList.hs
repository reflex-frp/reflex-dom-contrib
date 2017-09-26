{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Reflex.Dom.Contrib.Widgets.CheckboxList where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.Widgets.Common
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Takes a list of labels to make checkboxes for and returns the labels of
-- the boxes that are checked.
checkboxList
    :: forall t m a. (MonadWidget t m, Ord a)
    => (a -> Text)
    -- ^ Function to show each item
    -> (Text -> a -> Bool)
    -- ^ Function to filter each item
    -> Event t Bool
    -- ^ Blanket event to apply to all list items.  Allows you to have "select
    -- all" and "select none" buttons.  Fire True to select all and False to
    -- select none.
    -> Dynamic t Text
    -- ^ A search string for filtering the list of items.
    -> Set a
    -- ^ Set of items that should be initially checked
    -> [a]
    -- ^ List of items to show checkboxes for
    -> m (HtmlWidget t [a])
    -- ^ Dynamic list of checked items
checkboxList showFunc filterFunc blanketEvent searchString onItems items = do
    el "ul" $ do
      es <- forM items $ \item -> do
        let shown = showFunc item
            mkAttrs search =
              if filterFunc search item
                then mempty
                else "style" =: "display:none"
        attrs <- holdUniqDyn $ mkAttrs <$> searchString
        elDynAttr "li" attrs $ el "label" $ do
          cb <- htmlCheckbox $ WidgetConfig
                  (leftmost [blanketEvent])
                  (S.member item onItems)
                  (constDyn mempty)
          text shown
          return $ mapWidget (\b -> if b then [item] else []) cb
      return $ wconcat es


------------------------------------------------------------------------------
-- | Takes a list of labels to make checkboxes for and returns the labels of
-- the boxes that are checked.
checkboxListView
    :: forall t m a b. (MonadWidget t m, Ord a)
    => (a -> Text)
    -- ^ Function to show each item
    -> (Text -> a -> Bool)
    -- ^ Function to filter each item
    -> (a -> Bool -> b)
    -> Event t Bool
    -- ^ Blanket event to apply to all list items.  Allows you to have "select
    -- all" and "select none" buttons.  Fire True to select all and False to
    -- select none.
    -> Dynamic t Text
    -- ^ A search string for filtering the list of items.
    -> Set a
    -- ^ Set of items that should be initially checked
    -> [a]
    -- ^ List of items to show checkboxes for
    -> m (Event t b)
    -- ^ Events changing the selected set
checkboxListView showFunc filterFunc updateFunc blanketEvent searchString
                 onItems items = do
    el "ul" $ do
      es <- forM items $ \item -> do
        let shown = showFunc item
            mkAttrs search =
              if filterFunc search item
                then mempty
                else "style" =: "display:none"
        attrs <- holdUniqDyn $ mkAttrs <$> searchString
        elDynAttr "li" attrs $ el "label" $ do
          cb <- htmlCheckbox $ WidgetConfig
                  (leftmost [blanketEvent])
                  (S.member item onItems)
                  (constDyn mempty)
          text shown
          return $ updateFunc item <$> _hwidget_change cb
      return $ leftmost es

