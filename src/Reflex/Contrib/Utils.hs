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
extractEvent
    :: (Reflex t, MonadHold t m)
    => (a -> Event t b)
    -> Dynamic t a
    -> m (Event t b)
extractEvent f = liftM (switch . current) . mapDyn f


------------------------------------------------------------------------------
extractDyn
    :: (Reflex t, MonadHold t m)
    => (a -> Dynamic t b)
    -> Dynamic t a
    -> m (Dynamic t b)
extractDyn f = liftM joinDyn . mapDyn f

