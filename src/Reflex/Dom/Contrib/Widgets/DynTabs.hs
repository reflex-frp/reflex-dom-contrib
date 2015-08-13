{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-|

An API for constructing a tab bar where the list of tabs in the bar is
determined dynamically.

-}

module Reflex.Dom.Contrib.Widgets.DynTabs
  ( Tab(..)
  , tabBar
  , tabPane
  , activeHelper
  ) where

------------------------------------------------------------------------------
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Set as S
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
class (Eq tab, Ord tab) => Tab m tab where
    tabIndicator :: tab -> m ()


------------------------------------------------------------------------------
tabBar
    :: forall t m tab. (MonadWidget t m, Tab m tab)
    => String
    -> tab
    -- ^ Initial open tab
    -> [tab]
    -> Event t [tab]
    -- ^ Dynamic list of the displayed tabs
    -> Event t tab
    -- ^ Event updating the currently selected tab
    -> Dynamic t (S.Set tab)
    -- ^ Set containing tabs that are currently disabled
    -> m (Dynamic t tab)
tabBar tabClass initialSelected initialTabs tabs curTab disabledTabs = do
    divClass "dyn-tab-bar" $ do
      elAttr "ul" ("class" =: tabClass) $ do
        rec let tabFunc = mapM (mkTab currentTab disabledTabs)
            foo <- widgetHoldHelper tabFunc initialTabs tabs
            let bar :: Event t tab = switch $ fmap leftmost $ current foo
            currentTab <- holdDyn initialSelected $ leftmost [bar, curTab]
        return currentTab


------------------------------------------------------------------------------
mkTab
  :: (MonadWidget t m, Tab m tab)
  => Dynamic t tab
  -> Dynamic t (S.Set tab)
  -> tab
  -> m (Event t tab)
mkTab currentTab disabledTabs t = do
    isSelected <- mapDyn (==t) currentTab
    isDisabled <- mapDyn (S.member t) disabledTabs
    e <- activeHelper "li" (el "a" $ tabIndicator t) isSelected isDisabled
    return $ gate (not <$> current isDisabled) (t <$ e)


------------------------------------------------------------------------------
tabPane :: (MonadWidget t m, Eq tab) => Dynamic t tab -> tab -> m a -> m a
tabPane currentTab t child = do
    isShown <- mapDyn (\c ->
      if c == t then klass
                else klass <> "style" =: "display: none") currentTab
    elDynAttr "div" isShown child
  where
    klass = "class" =: "dyn-tab-pane"


------------------------------------------------------------------------------
-- | Sets the \"active\" class 
activeHelper
  :: MonadWidget t m
  => String
  -> m ()
  -> Dynamic t Bool
  -- ^ Is selected
  -> Dynamic t Bool
  -- ^ Is disabled
  -> m (Event t ())
activeHelper elName children isSelected isDisabled = do
    let mkAttrs selected disabled =
          if disabled
            then "class" =: "disabled"
            else if selected
                   then "class" =: "active"
                   else M.empty
    attrs <- combineDyn mkAttrs isSelected isDisabled
    (li, _) <- elDynAttr' elName attrs children
    return $ domEvent Click li

