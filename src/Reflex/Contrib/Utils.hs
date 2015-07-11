{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE RecursiveDo              #-}

module Reflex.Contrib.Utils where

------------------------------------------------------------------------------
import           Reflex
------------------------------------------------------------------------------


------------------------------------------------------------------------------
end :: Monad m => m ()
end = return ()


------------------------------------------------------------------------------
fmapMaybe2 :: Reflex t => Event t (Maybe (Maybe a)) -> Event t a
fmapMaybe2 = fmapMaybe id . fmapMaybe id


------------------------------------------------------------------------------
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


