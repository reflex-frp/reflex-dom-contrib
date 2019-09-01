{-|

This module probably an over-complicated and cumbersome solution to the problem
it is trying to solve. See the comment on boundedSelectList' for a description
of what it is trying to do. But on the whole you're probably better off not
using this module. It's being kept around mainly for legacy support. Suggestions
as to how this might be improved or redone better are most welcome.

-}


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

module Reflex.Dom.Contrib.Widgets.BoundedList
  ( boundedSelectList
  , boundedSelectList'
  , mkHiding
  , keyToMaybe
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Reflex.Contrib.Interfaces
import           Reflex.Contrib.Utils
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
findCurItem :: Ord k => Map k v -> k -> Maybe (k,v)
findCurItem m k = M.lookupLE k m <|> M.lookupGT k m


-- Limit on the number of items in the DOM.  Might make this more
-- sophisticated in the future.
type Limit = Maybe Int

-- An Int counter that we use in lieu of a timestamp for LRU calculations
type BornAt = Int


------------------------------------------------------------------------------
limitMap :: Ord k => Map k v -> Limit -> Map k v
limitMap m Nothing = m
limitMap m (Just lim) = M.fromList $ take lim $ M.toList m


------------------------------------------------------------------------------
boundedInsert
    :: Ord k
    => Limit
    -> (BornAt, (k,v))
    -> Map k (BornAt,v)
    -> Map k (BornAt,v)
boundedInsert Nothing (c, (k, v)) m = M.insert k (c,v) m
boundedInsert (Just lim) (c, (k, v)) m =
    if M.size m < lim then ins m else ins pruned
  where
    ins = M.insert k (c,v)
    pruned = M.fromList $ tail $ sortOn (fst . snd) $ M.toList m


------------------------------------------------------------------------------
-- | A widget with generalized handling for dynamically sized lists.  There
-- are many possible approaches to rendering lists that have one visible
-- current selection.  One way is to keep all the items in the DOM and manage
-- the selection by managing visibility through something like display:none or
-- visibility:hidden.  Another way is to only keep the currently selected item
-- in the DOM and swap it out every time the selection is changed.
--
-- The problem with keeping all items in the DOM is that this might use too
-- much memory either because there are many items or the items are large.
-- The problem with keeping only the currently selected item in the DOM is
-- that performance might be too slow if removing the old item's DOM elements
-- and building the new one takes too long.
--
-- This widget provides a middle ground.  It lets the user decide how many
-- elements are kept in the DOM at any one time and prunes the least recently
-- used items if that size is exceeded.
boundedSelectList'
    :: (MonadWidget t m, Ord k)
    => Limit
    -- ^ Maximum number of items to keep in the DOM at a time
    -> Dynamic t k
    -- ^ Currently selected item
    -> Event t (Map k v -> Map k v)
    -- ^ Event that updates individual item values
    -> ReflexMap t k v
    -- ^ Interface for updating the list
    -> (k -> Dynamic t v -> Dynamic t Bool -> m a)
    -- ^ Function to render a single item
    -> m (Dynamic t (Map k a))
boundedSelectList' itemLimit curSelected updateEvent
                  ReflexMap{..} renderSingle = do
    -- Map holding the full item list.
    items <- foldDyn ($) rmInitialItems $ leftmost
      [ M.union . M.fromList <$> rmInsertItems
      , rmDeleteFunc <$> rmDeleteItems
      , updateEvent
      ]

    counter <- count $ updated curSelected
    let curItem = zipDynWith findCurItem items curSelected
    let addCounter c (k,v) = (k, ((-c), (k, v)))
        taggedInitial = M.fromList $ zipWith addCounter [1..] $
                          M.toList rmInitialItems
    let initMap = limitMap taggedInitial itemLimit
    activeItems <- foldDyn ($) initMap $
      boundedInsert itemLimit <$>
      attachPromptlyDynWith (\c (k,v) -> (c, (k, (k,v))))
        counter (fmapMaybe id $ updated curItem)
    listWithKeyAndSelection curSelected activeItems wrapSingle
  where
    --wrapSingle :: k -> Dynamic t (BornAt, (k,v)) -> Dynamic t Bool -> m a
    wrapSingle k v b = do
        v' <- filterDyn (\x -> fst (snd x) == k) v
        renderSingle k (snd . snd <$> v') b


------------------------------------------------------------------------------
-- | Implements a common use of boundedSelectList' where only the currently
-- selected item from a list is displayed.  In this case a Dynamic
-- representing the current selection is used to drive insertions and they are
-- never deleted externally.  Instead of returning a Map of all the item
-- results, this function only returns the result for the item that is
-- currently selected.
boundedSelectList0
    :: (MonadWidget t m, Ord k)
    => Limit
    -- ^ Maximum number of items to keep in the DOM at a time
    -> Dynamic t a
    -- ^ Currently selected item.  New items are added to the list when the
    -- currently selected item changes and the new item is not already in the
    -- list.
    -> Event t (Map k v -> Map k v)
    -> (a -> k)
    -- ^ Gets the portion of a used as the key for the map of items
    -> (a -> Maybe a)
    -- ^ Decides whether to run expensiveGetNew in the case that the key is
    -- already in the cache.
    -> (Event t a -> m (Event t (k,v)))
    -- ^ Gets a new key/value pair.  This function is run when curSelected
    -- changes.
    -> (k -> Dynamic t v -> Dynamic t Bool -> m b)
    -- ^ Function to render a single item
    -> m (Dynamic t (Map k b))
boundedSelectList0 itemLimit curSelected updateEvent getKey shouldRunExpensive
                   expensiveGetNew renderSingle = do
    pb <- getPostBuild
    rec
      let insertEvent = leftmost
            [ fmapMaybe id $
                attachPromptlyDynWith isAlreadyPresent res (updated curSelected)
            , tagPromptlyDyn curSelected pb
            ]
      newVal <- expensiveGetNew insertEvent
      let rm = ReflexMap mempty ((:[]) <$> newVal) never
          curK = getKey <$> curSelected
      res :: Dynamic t (Map k b) <-
        boundedSelectList' itemLimit curK updateEvent rm renderSingle
    return res
  where
    isAlreadyPresent fieldListMap cur =
        case M.lookup (getKey cur) fieldListMap of
          Nothing -> Just cur
          Just _ -> shouldRunExpensive cur


------------------------------------------------------------------------------
-- | Implements a common use of boundedSelectList' where only the currently
-- selected item from a list is displayed.  In this case a Dynamic
-- representing the current selection is used to drive insertions and they are
-- never deleted externally.  Instead of returning a Map of all the item
-- results, this function only returns the result for the item that is
-- currently selected.
boundedSelectList
    :: (MonadWidget t m, Ord k)
    => Limit
    -- ^ Maximum number of items to keep in the DOM at a time
    -> Dynamic t a
    -- ^ Currently selected item.  New items are added to the list when the
    -- currently selected item changes and the new item is not already in the
    -- list.
    -> Event t (Map k v -> Map k v)
    -> (a -> k)
    -- ^ Gets the portion of a used as the key for the map of items
    -> (a -> Maybe a)
    -- ^ Decides whether to run expensiveGetNew in the case that the key is
    -- already in the cache.
    -> (Event t a -> m (Event t (k,v)))
    -- ^ Gets a new key/value pair.  This function is run when curSelected
    -- changes.
    -> b
    -- ^ Default value to return if nothing is in the list
    -> (k -> Dynamic t v -> Dynamic t Bool -> m b)
    -- ^ Function to render a single item
    -> m (Dynamic t b)
boundedSelectList itemLimit curSelected updateEvent getKey shouldRunExpensive
                  expensiveGetNew defaultVal renderSingle = do
    res <- boundedSelectList0 itemLimit curSelected updateEvent getKey
                              shouldRunExpensive expensiveGetNew renderSingle
    return $ zipDynWith getCurrent curSelected res
  where
    getCurrent cur listMap =
        case M.lookup (getKey cur) listMap of
          Nothing -> defaultVal
          Just v -> v


------------------------------------------------------------------------------
-- | Wraps a widget with a dynamically hidden div that uses display:none to
-- hide.
mkHiding
    :: (MonadWidget t m)
    => Map Text Text
    -> m a
    -> Dynamic t Bool
    -- ^ Function of a dynamic active flag
    -> m a
mkHiding staticAttrs w active = do
    let attrs = mkAttrs <$> active
    elDynAttr "div" attrs w
  where
    mkAttrs True = staticAttrs
    mkAttrs False = staticAttrs <> "style" =: "display:none"


------------------------------------------------------------------------------
-- | Small helper for a common pattern that comes up with the expensiveGetNew
-- parameter to boundedSelectList.
keyToMaybe
    :: MonadWidget t m
    => (Event t a -> m (Event t (b,c)))
    -> Event t (Maybe a)
    -> m (Event t (Maybe b, c))
keyToMaybe f = liftM (fmap $ first Just) . f . fmapMaybe id
