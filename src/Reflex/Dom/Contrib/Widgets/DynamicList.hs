{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecursiveDo      #-}
module Reflex.Dom.Contrib.Widgets.DynamicList where

------------------------------------------------------------------------------
import qualified Data.Map as M
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Dynamic list widget that creates a list that supports the dynamic
-- addition and removal of items.  This widget is completely general with zero
-- markup-specific choices.  It handles all the event plumbing and lets you
-- completely determine the markup.
dynamicList
    :: MonadWidget t m
    => (Int -> a -> Event t a -> m b)
    -- ^ Widget used to display each item
    -> (b -> Event t ())
    -- ^ Function that gets a remove event from the return value of each item
    -> (b -> Event t a)
    -- ^ Event that adds a new item to the list that is somehow based on an
    -- existing item.  If you don't want anything like this, use `const never`.
    -> Event t a
    -- ^ Event that adds a new item to the list that is not based on an
    -- existing item.
    -> [a]
    -- ^ Initial list of items
    -> m (Dynamic t [b])
dynamicList w removeEvent addFunc addEvent initList = do
    let initMap = M.fromList $ zip [0..] initList
    rec let vals = mergeWith (<>)
              [ attachWith addNew (current res) addEvent
              , addSpecific (current res)
              , remove (current res)
              ]
        res <- listWithKeyShallowDiff initMap vals w
    return $ M.elems <$> res
  where
    addSpecific res = switch (foo <$> res)
    foo m = leftmost $ map (fmap (addNew m) . addFunc) $ M.elems m
    addNew m a = M.singleton k (Just a)
      where
        k = if M.null m then 0 else fst (M.findMax m) + 1
    remove res = switch (mergeWith (<>) . map f . M.toList <$> res)
      where
        f (k,b) = M.singleton k Nothing <$ removeEvent b
