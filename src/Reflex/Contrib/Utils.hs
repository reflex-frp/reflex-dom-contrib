{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE RecursiveDo              #-}

module Reflex.Contrib.Utils where

------------------------------------------------------------------------------
import           Control.Monad
import           Reflex
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A small amount of convenience for return ().
end :: Monad m => m ()
end = return ()


------------------------------------------------------------------------------
-- | fmapMaybe twice
fmapMaybe2 :: Reflex t => Event t (Maybe (Maybe a)) -> Event t a
fmapMaybe2 = fmapMaybe id . fmapMaybe id


------------------------------------------------------------------------------
-- | Construct an event with a tuple of (current,updated).
attachDynSelf :: Reflex t => Dynamic t a -> Event t (a,a)
attachDynSelf d = attach (current d) (updated d)


------------------------------------------------------------------------------
-- | Partitions an event into a pair of events that fire when the predicate
-- function evaluates to True and False respectively.
partitionEvent
    :: Reflex t
    => (a -> Bool)
    -> Event t a
    -> (Event t a, Event t a)
partitionEvent f e = ( fmapMaybe (\(b, x) -> if b then Just x else Nothing) e'
                     , fmapMaybe (\(b, x) -> if b then Nothing else Just x) e'
                     )
  where e' = fmap (\x -> (f x, x)) e


------------------------------------------------------------------------------
-- | Sometimes you end up with a Dynamic t Foo where Foo contains an Event
-- field.  This function collapses the two levels of Dynamic Event into just
-- an Event.
extractEvent
    :: Reflex t
    => (a -> Event t b)
    -> Dynamic t a
    -> (Event t b)
extractEvent f = switch . current . fmap f


------------------------------------------------------------------------------
-- | Sometimes you end up with a Dynamic t Foo where Foo contains a Dynamic
-- field.  This function collapses the two levels of Dynamic Dynamic into a
-- single Dynamic.
extractDyn
    :: Reflex t
    => (a -> Dynamic t b)
    -> Dynamic t a
    -> Dynamic t b
extractDyn f = join . fmap f


------------------------------------------------------------------------------
-- | This function has the slight flaw that if (f initValue) == False, it will
-- still get through.
filterDyn
    :: (Reflex t, MonadHold t m)
    => (a -> Bool)
    -> Dynamic t a
    -> m (Dynamic t a)
filterDyn f d = fmap join $ holdDyn d $ fmap constDyn $
    ffilter f (updated d)


------------------------------------------------------------------------------
-- | Similar to filterDyn, but here the initial value problem is visible
-- because of the Maybe.
fmapMaybeDyn
    :: (MonadHold t m, Reflex t)
    => (a -> Maybe b)
    -> Dynamic t a
    -> m (Dynamic t (Maybe b))
fmapMaybeDyn f d = do
    fmap join $ holdDyn (f <$> d) $ fmap (constDyn . Just) $
      fmapMaybe f (updated d)
