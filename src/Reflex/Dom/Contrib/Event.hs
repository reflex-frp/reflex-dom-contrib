{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | Helper functions for dealing with events
module Reflex.Dom.Contrib.Event where

import           Control.Applicative      (empty)
import           Control.Monad            (void)
import           Control.Monad.IO.Class   (MonadIO)
import           Data.Time.Clock          (NominalDiffTime)
import           Reflex
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Dom.Old           as Dom
import qualified Reflex.Dom.Widget.Basic  as Dom

eventJoin :: (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)
eventJoin = switchHold never

-- | Block those nothing events and only let trough just a's
noNothing :: FunctorMaybe f => f (Maybe a) -> f a
noNothing = fmapMaybe id

-- | Do something monadic with an event val
--   Because of haskell lazyness the things inside a holdevent
--   don't get evaluated untill the event fires, which makes the first
--   time slow. However it is good for initialization as we don't
--   need to load things unused.
holdEvent ::
     (Dom.DomBuilder t m, MonadHold t m)
  => b
  -> Event t a
  -> (a -> m b)
  -> m (Dynamic t b)
holdEvent val evt fun = Dom.widgetHold (pure val) $ fun <$> evt

-- | Convenience holdEvent for the case where we don't care about the
--   value.
holdEvent_ ::
     (Dom.DomBuilder t m, MonadHold t m) => Event t a -> (a -> m b) -> m ()
holdEvent_ = fmap void . holdEvent undefined -- we throw away the value


-- | Get rid of a dynimc around a tupple of events,
--   common sense says we should be able to do this for any traversable,
--   but keeping the values of events hetrogenous is hard (I don't know how to)
switchTup ::
     (Reflex t) => Dynamic t (Event t b, Event t c) -> (Event t b, Event t c)
switchTup tup = (switchDyn $ fst <$> tup, switchDyn $ snd <$> tup)

-- | Do something monadic with an event val, and get the event which is
--  delayed for a moment.
--  Using this may indicate you're doing something weird.
--  Although I've found it handy in getting just something to work
holdAfter ::
     ( PostBuild t m
     , Dom.DomBuilder t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => b
  -> Event t a
  -> (a -> Event t a -> m b)
  -> m (Dynamic t b)
holdAfter val evt fun = delay 0 evt >>= holdEvent val evt . flip fun

-- | show something for 5 seconds after an event
flash ::
     (Monoid c, Dom.MonadWidget t m)
  => Event t b
  -> (b -> m c)
  -> m (Dynamic t c)
flash = flash' 5 mempty

flash' ::
     Dom.MonadWidget t m
  => NominalDiffTime
  -> c
  -> Event t b
  -> (b -> m c)
  -> m (Dynamic t c)
flash' time defVal event monadFunc = do
  delayed <- delay time event
  holdEvent defVal (leftmost [pure <$> event, empty <$ delayed]) $
    maybe (pure defVal) monadFunc

unqUpdated :: (Reflex t, Eq a) => UniqDynamic t a -> Event t a
unqUpdated = updated . fromUniqDynamic
