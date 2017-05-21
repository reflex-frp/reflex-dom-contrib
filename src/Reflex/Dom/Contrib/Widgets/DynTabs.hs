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
  , TabBar(..)
  , TabBarConfig(..)
  , tabBar
  , tabPane
  , addDisplayNone
  , addActiveClass
  ) where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
class Eq tab => Tab t m tab where
    tabIndicator :: tab -> Dynamic t Bool -> m (Event t ())


------------------------------------------------------------------------------
-- | Tab bar widget inputs
data TabBarConfig t tab = TabBarConfig
    { _tabBarConfig_initialOpenTab :: tab
    , _tabBarConfig_initialTabs    :: [tab]
    , _tabBarConfig_tabListUpdates :: Event t [tab]
    , _tabBarConfig_changeCurTab   :: Event t tab
    }


instance (Reflex t, Bounded tab, Enum tab) => Default (TabBarConfig t tab) where
  def = TabBarConfig minBound [minBound..maxBound] never never


------------------------------------------------------------------------------
-- | Tab bar widget outputs
data TabBar t tab = TabBar
    { _tabBar_curTab    :: Dynamic t tab
    , _tabBar_tabClicks :: Event t tab
    }

------------------------------------------------------------------------------
-- | Renders a dynamic list of tabs comprising a tab bar.
tabBar :: forall t m tab. (MonadWidget t m, Tab t m tab)
    => TabBarConfig t tab
    -> m (TabBar t tab)
tabBar (TabBarConfig initialSelected initialTabs tabs curTab) = do
    rec let tabFunc = mapM (mkTab currentTab)
        foo <- widgetHoldHelper tabFunc initialTabs tabs
        let bar :: Event t tab = switch $ fmap leftmost $ current foo
        currentTab <- holdDyn initialSelected $ leftmost [bar, curTab]
    return $ TabBar currentTab bar


------------------------------------------------------------------------------
mkTab
    :: (MonadWidget t m, Tab t m tab)
    => Dynamic t tab
    -> tab
    -> m (Event t tab)
mkTab currentTab t = do
    e <- tabIndicator t ((==t) <$> currentTab)
    return (t <$ e)


------------------------------------------------------------------------------
-- | Convenience function for constructing a tab pane that uses display none
-- to hide the tab when it is not selected.
tabPane
    :: (MonadWidget t m, Eq tab)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m a
tabPane staticAttrs currentTab t child = do
    let attrs = addDisplayNone (constDyn staticAttrs) ((==t) <$> currentTab)
    elDynAttr "div" attrs child


------------------------------------------------------------------------------
-- | Helper function for hiding your tabs with display none.
addDisplayNone
    :: Reflex t
    => Dynamic t (Map Text Text)
    -> Dynamic t Bool
    -> Dynamic t (Map Text Text)
addDisplayNone attrs isActive = zipDynWith f isActive attrs
  where
    f True as = as
    f False as = M.insert "style" "display: none" as


------------------------------------------------------------------------------
-- | Helper function for adding an active class.  Useful for implemnting the
-- Tab type class.
addActiveClass
    :: Reflex t
    => Dynamic t Bool
    -> Dynamic t (Map Text Text)
    -> Dynamic t (Map Text Text)
addActiveClass isActive attrs = zipDynWith f isActive attrs
  where
    f True as = M.insertWith (\n o -> T.unwords [o,n]) "class" "active" as
    f False as = as

