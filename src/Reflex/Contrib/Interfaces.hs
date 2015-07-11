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

module Reflex.Contrib.Interfaces
  ( ReflexMap(..)
  , ReflexList(..)
  , rlist2rmap
  , toReflexMap
  , rmDeleteFunc
  , selectViewListWithKey
  ) where

------------------------------------------------------------------------------
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A reflex interface to lists.  It has a pure list of initial items, an
-- event for inserting multiple items, and an event for deleting multiple
-- items.
data ReflexList t v = ReflexList
    { rlInitialItems :: [v]
    , rlInsertItems  :: Event t [v]
    , rlDeleteItems  :: Event t [v]
    }


------------------------------------------------------------------------------
-- | Converts a ReflexList to a ReflexMap with integer keys.
rlist2rmap
    :: (Reflex t, Eq v)
    => ReflexList t v
    -> Dynamic t (Map Int v)
    -> ReflexMap t Int v
rlist2rmap ReflexList{..} curMap = ReflexMap
    { rmInitialItems = M.fromList $ zip [0..] rlInitialItems
    , rmInsertItems = attachDynWith f curMap rlInsertItems
    , rmDeleteItems = attachDynWith g curMap rlDeleteItems
    }
  where
    f m is = zip [nextKey m..] is
    g m is = M.keysSet $ M.filter (`elem` is) m
    nextKey m = succ $ fst (M.findMax m)


------------------------------------------------------------------------------
-- | A reflex interface to maps that allows inserting and deleting multiple
-- items at a time.
data ReflexMap t k v = ReflexMap
    { rmInitialItems :: Map k v
    , rmInsertItems  :: Event t [(k,v)]
    -- ^ We use a list of key-value pairs here because order is important
    , rmDeleteItems  :: Event t (Set k)
    }


------------------------------------------------------------------------------
-- | Converts a ReflexList to a Dynamic list.
toReflexMap
    :: (MonadWidget t m, Eq a)
    => ReflexList t a
    -> (ReflexMap t Int a -> m (Dynamic t (Map Int a)))
    -> m (Dynamic t [a])
toReflexMap rlist f = do
    rec let rmap = rlist2rmap rlist m
        m <- f rmap
    mapDyn M.elems m


------------------------------------------------------------------------------
-- | Takes a set of keys and returns a function that deletes these keys from
-- a map.
rmDeleteFunc :: Ord k => Set k -> Map k v -> Map k v
rmDeleteFunc s m = foldr M.delete m (S.toList s)


------------------------------------------------------------------------------
-- | A generalized version of the one in reflex-dom.
selectViewListWithKey
    :: forall t m k v a. (MonadWidget t m, Ord k)
    => Dynamic t k
    -> Dynamic t (Map k v)
    -> (k -> Dynamic t v -> Dynamic t Bool -> m a)
    -> m (Dynamic t (Map k a))
selectViewListWithKey selection vals mkChild = do
  let selectionDemux = demux selection
  listWithKey vals $ \k v -> do
    selected <- getDemuxed selectionDemux k
    mkChild k v selected



