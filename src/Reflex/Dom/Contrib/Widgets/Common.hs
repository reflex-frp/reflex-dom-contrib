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

module Reflex.Dom.Contrib.Widgets.Common where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Readable
import           Data.String.Conv
import           Data.Time
import           GHCJS.DOM.HTMLInputElement
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Reflex.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Generic config structure common to most widgets.  The attributes field
-- may not be used for all widgets, but in that case it can just be ignored.
-- We may want to change this in the future, but it seems like a reasonable
-- start for now.
data WidgetConfig t a
    = WidgetConfig { _widgetConfig_setValue :: Event t a
                   , _widgetConfig_initialValue :: a
                   , _widgetConfig_attributes :: Dynamic t (Map String String)
                   }

makeLenses ''WidgetConfig

instance (Reflex t, Default a) => Default (WidgetConfig t a) where
  def = WidgetConfig { _widgetConfig_setValue = never
                     , _widgetConfig_initialValue = def
                     , _widgetConfig_attributes = constDyn mempty
                     }

instance HasAttributes (WidgetConfig t a) where
  type Attrs (WidgetConfig t a) = Dynamic t (Map String String)
  attributes = widgetConfig_attributes

instance HasSetValue (WidgetConfig t a) where
  type SetValue (WidgetConfig t a) = Event t a
  setValue = widgetConfig_setValue


------------------------------------------------------------------------------
-- | A general-purpose widget return value.
data HtmlWidget t a = HtmlWidget
    { _hwidget_value    :: Dynamic t a
      -- ^ The authoritative value for this widget.
    , _hwidget_change   :: Event t a
      -- ^ Event that fires when the widget changes internally (not via a
      -- setValue event).
    , _hwidget_keypress :: Event t Int
    , _hwidget_keydown  :: Event t Int
    , _hwidget_keyup    :: Event t Int
    , _hwidget_hasFocus :: Dynamic t Bool
    }

makeLenses ''HtmlWidget


instance HasValue (HtmlWidget t a) where
  type Value (HtmlWidget t a) = Dynamic t a
  value = _hwidget_value


------------------------------------------------------------------------------
-- | Generalized form of many widget functions.
type GWidget t m a = WidgetConfig t a -> m (HtmlWidget t a)


------------------------------------------------------------------------------
constWidget :: Reflex t => a -> HtmlWidget t a
constWidget a = HtmlWidget (constDyn a) never never never never (constDyn False)


------------------------------------------------------------------------------
-- | We can't make a Functor instance for HtmlWidget until Dynamic gets a
-- Functor instance.  So until then, this will have to do.
mapWidget
    :: MonadWidget t m
    => (a -> b)
    -> HtmlWidget t a
    -> m (HtmlWidget t b)
mapWidget f w = do
    newVal <- mapDyn f $ value w
    return $ HtmlWidget
      newVal
      (f <$> _hwidget_change w)
      (_hwidget_keypress w)
      (_hwidget_keydown w)
      (_hwidget_keyup w)
      (_hwidget_hasFocus w)


------------------------------------------------------------------------------
-- | Combine function does the expected thing for _value and _change and
-- applies leftmost to each of the widget events.
combineWidgets
    :: MonadWidget t m
    => (a -> b -> c)
    -> HtmlWidget t a
    -> HtmlWidget t b
    -> m (HtmlWidget t c)
combineWidgets f a b = do
    newVal <- combineDyn f (value a) (value b)
    let newChange = tag (current newVal) $ leftmost
          [() <$ _hwidget_change a, () <$ _hwidget_change b]
    newFocus <- combineDyn (||) (_hwidget_hasFocus a) (_hwidget_hasFocus b)
    return $ HtmlWidget
      newVal newChange
      (leftmost [_hwidget_keypress a, _hwidget_keypress b])
      (leftmost [_hwidget_keydown a, _hwidget_keydown b])
      (leftmost [_hwidget_keyup a, _hwidget_keyup b])
      newFocus


------------------------------------------------------------------------------
-- | Combines multiple widgets over a Monoid operation.
wconcat
    :: (MonadWidget t m, Foldable f, Monoid a)
    => f (HtmlWidget t a) -> m (HtmlWidget t a)
wconcat = foldM (combineWidgets (<>)) (constWidget mempty)


------------------------------------------------------------------------------
-- | Convenience for extracting HtmlWidget from a Dynamic.
extractWidget
    :: MonadWidget t m
    => Dynamic t (HtmlWidget t a)
    -> m (HtmlWidget t a)
extractWidget dynWidget = do
    v <- extractDyn value dynWidget
    c <- extractEvent _hwidget_change dynWidget
    kp <- extractEvent _hwidget_keypress dynWidget
    kd <- extractEvent _hwidget_keydown dynWidget
    ku <- extractEvent _hwidget_keyup dynWidget
    hf <- extractDyn _hwidget_hasFocus dynWidget
    return $ HtmlWidget v c kp kd ku hf


------------------------------------------------------------------------------
-- | Input widget for datetime values.
dateTimeWidget
    :: (MonadWidget t m)
    => GWidget t m (Maybe UTCTime)
dateTimeWidget cfg = do
    let wValue = _widgetConfig_setValue cfg
        setDate = maybe "" (formatTime defaultTimeLocale dfmt)
        setTime = maybe "" (formatTime defaultTimeLocale tfmt)
    el "div" $ do
        di <- htmlTextInput "date" $ def
          & setValue .~ (setDate <$> wValue)
          & attributes .~ _widgetConfig_attributes cfg
          & widgetConfig_initialValue .~ setDate
              (_widgetConfig_initialValue cfg)
        ti <- htmlTextInput "time" $ def
          & setValue .~ (setTime <$> wValue)
          & attributes .~ _widgetConfig_attributes cfg
          & widgetConfig_initialValue .~ setTime
              (_widgetConfig_initialValue cfg)
        combineWidgets (\d t -> parseTimeM True defaultTimeLocale "%F %X" $
                                  toS $ d ++ " " ++ t ++ ":00")
          di ti
  where
    dfmt = "%F"
    tfmt = "%X"


------------------------------------------------------------------------------
-- | Input widget for dates.
dateWidget
    :: (MonadWidget t m)
    => GWidget t m (Maybe Day)
dateWidget cfg = do
    let setVal = showD <$> _widgetConfig_setValue cfg
    di <- htmlTextInput "date" $ def
      & setValue .~ setVal
      & attributes .~ _widgetConfig_attributes cfg
      & widgetConfig_initialValue .~ showD
          (_widgetConfig_initialValue cfg)
    mapWidget (parseTimeM True defaultTimeLocale fmt) di
  where
    fmt = "%F"
    showD = maybe "" (formatTime defaultTimeLocale fmt)


------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's checkbox.
htmlCheckbox
    :: MonadWidget t m
    => GWidget t m Bool
htmlCheckbox cfg = do
    cb <- checkbox (_widgetConfig_initialValue cfg) $ def
      & setValue .~ _widgetConfig_setValue cfg
      & attributes .~ _widgetConfig_attributes cfg
    return $ HtmlWidget
      (_checkbox_value cb)
      (_checkbox_change cb)
      never never never
      (constDyn False)


------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's textInput.
htmlTextInput
    :: MonadWidget t m
    => String
    -> GWidget t m String
htmlTextInput inputType cfg = do
    (_,w) <- htmlTextInput' inputType cfg
    return w


------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's textInput that also returns the
-- HTMLInputElement.
htmlTextInput'
    :: MonadWidget t m
    => String
    -> WidgetConfig t String
    -> m (HTMLInputElement, HtmlWidget t String)
htmlTextInput' inputType cfg = do
    ti <- textInput $ def
      & setValue .~ _widgetConfig_setValue cfg
      & attributes .~ _widgetConfig_attributes cfg
      & textInputConfig_initialValue .~ _widgetConfig_initialValue cfg
      & textInputConfig_inputType .~ inputType
    let w = HtmlWidget
          (_textInput_value ti)
          (_textInput_input ti)
          (_textInput_keypress ti)
          (_textInput_keydown ti)
          (_textInput_keyup ti)
          (_textInput_hasFocus ti)
    return (_textInput_element ti, w)


------------------------------------------------------------------------------
-- | NOTE: You should probably not use this function with string types because
-- the Show instance will quote strings.
readableWidget
    :: (MonadWidget t m, Show a, Readable a)
    => GWidget t m (Maybe a)
readableWidget cfg = do
    let setVal = maybe "" show <$> _widgetConfig_setValue cfg
    w <- htmlTextInput "text" $ WidgetConfig setVal
      (maybe "" show (_widgetConfig_initialValue cfg))
      (_widgetConfig_attributes cfg)
    let parse = fromText . toS
    mapWidget parse w


------------------------------------------------------------------------------
-- | Widget that parses its input to a Double.
doubleWidget :: (MonadWidget t m) => GWidget t m (Maybe Double)
doubleWidget = readableWidget


------------------------------------------------------------------------------
-- | Widget that parses its input to an Integer.
integerWidget :: (MonadWidget t m) => GWidget t m (Maybe Integer)
integerWidget = readableWidget


------------------------------------------------------------------------------
-- | Widget that parses its input to an Int.
intWidget :: (MonadWidget t m) => GWidget t m (Maybe Int)
intWidget = readableWidget


------------------------------------------------------------------------------
-- | Returns an event that fires when the widget loses focus or enter is
-- pressed.
blurOrEnter
    :: Reflex t
    => HtmlWidget t a
    -> Event t a
blurOrEnter w = tagDyn (_hwidget_value w) fireEvent
  where
    fireEvent = leftmost [ () <$ (ffilter (==13) $ _hwidget_keypress w)
                         , () <$ (ffilter not $ updated $ _hwidget_hasFocus w)
                         ]


------------------------------------------------------------------------------
-- | Like readableWidget but only generates change events on blur or when
-- enter is pressed.
inputOnEnter
    :: MonadWidget t m
    => (WidgetConfig t a -> m (HtmlWidget t a))
    -> WidgetConfig t a
    -> m (Dynamic t a)
inputOnEnter wFunc cfg = do
    w <- wFunc cfg
    holdDyn (_widgetConfig_initialValue cfg) $ blurOrEnter w


------------------------------------------------------------------------------
-- | A list dropdown widget.
listDropdown :: (MonadWidget t m)
  => Dynamic t [a]
  -> (a -> String)
  -> Dynamic t (Map String String)
  -> String
  -> m (Dynamic t (Maybe a))
listDropdown xs f attrs defS = do
  m <- mapDyn (M.fromList . zip [(1::Int)..]) xs
  opts <- mapDyn ((M.insert 0 defS) . M.map f) m
  sel <- liftM _dropdown_value $ dropdown 0 opts $ def & attributes .~ attrs
  combineDyn M.lookup sel m

