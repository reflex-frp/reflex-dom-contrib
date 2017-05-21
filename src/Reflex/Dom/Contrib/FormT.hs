{-# LANGUAGE DeriveFunctor #-}

module Reflex.Dom.Contrib.FormT where

import Control.Lens
import Data.Default
import Data.Functor.Compose
import Data.Monoid
import Reflex

-- The goal is to write forms something like this
--
--pairForm :: FormT (a,b) (a,b) t m ()
--pairForm = do
--    o1 <- zoomForm fst fstForm
--    o2 <- zoomForm snd sndForm
--    output $ (,) <$> o1 <*> o2
--  where
--    fstForm = undefined
--    sndForm = undefined

data FormInput t a = FormInput
    { _formInput_initialValue :: Maybe a
    , _formInput_setValue     :: Event t (Maybe a)
    }

instance Reflex t => Functor (FormInput t) where
    fmap f (FormInput iv sv) = FormInput (f <$> iv) (fmap f <$> sv)

data FormOutput t a = FormOutput
    { _formOutput_value  :: Dynamic t (Maybe a)
    , _formOutput_change :: Event t (Maybe a)
    }

instance Reflex t => Default (FormOutput t a) where
    def = FormOutput (constDyn Nothing) never

instance Reflex t => Functor (FormOutput t) where
    fmap f (FormOutput v c) = FormOutput (fmap f <$> v) (fmap f <$> c)

instance Reflex t => Applicative (FormOutput t) where
    pure a = FormOutput (constDyn $ Just a) never
    (FormOutput vf cf) <*> (FormOutput va ca) = FormOutput v
        (tagPromptlyDyn v $ mconcat [() <$ cf, () <$ ca])
      where
        v = getCompose $ Compose vf <*> Compose va

newtype FormT i o t m a = FormT { runFormT :: FormInput t i -> m (FormOutput t o, a) }

zoomForm :: (Reflex t, Monad m) => (b -> v) -> FormT v o t m a -> FormT b o t m (FormOutput t o)
zoomForm l f = FormT $ \fi -> do
    (o,_) <- runFormT f (l <$> fi)
    return (o,o)

output :: Monad m => FormOutput t o -> FormT i o t m ()
output o = FormT $ \_ -> return (o,())

