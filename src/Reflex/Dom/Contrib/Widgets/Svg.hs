{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
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
{-# LANGUAGE UndecidableInstances      #-}

{-|

Convenience functions for generating SVG from reflex.

-}

module Reflex.Dom.Contrib.Widgets.Svg where

------------------------------------------------------------------------------
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Text  (Text)
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------

{-# INLINABLE svgDynAttr' #-}
svgDynAttr' :: forall t m a. MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

{-# INLINABLE svgDynAttr #-}
svgDynAttr :: forall t m a. MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgDynAttr elementTag attrs child = snd <$> svgDynAttr' elementTag attrs child

{-# INLINABLE svgAttr' #-}
svgAttr' :: forall t m a. MonadWidget t m => Text -> Map Text Text -> m a -> m (El t, a)
svgAttr' elementTag attrs child = svgDynAttr' elementTag (constDyn attrs) child

{-# INLINABLE svgAttr #-}
svgAttr :: forall t m a. MonadWidget t m => Text -> Map Text Text -> m a -> m a
svgAttr elementTag attrs child = svgDynAttr elementTag (constDyn attrs) child

{-# INLINABLE svg' #-}
svg' :: forall t m a. MonadWidget t m => Text -> m a -> m (El t, a)
svg' elementTag child = svgAttr' elementTag (Map.empty :: Map Text Text) child


{-# INLINABLE svg #-}
svg :: forall t m a. MonadWidget t m => Text -> m a -> m a
svg elementTag child = svgAttr elementTag Map.empty child

svgClass :: forall t m a. MonadWidget t m => Text -> Text -> m a -> m a
svgClass elementTag c child = svgAttr elementTag ("class" =: c) child

