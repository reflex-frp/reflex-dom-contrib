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

Infrastructure common to a wide variety of widgets.  WidgetConfig holds the
core inputs needed by most widgets, while HtmlWidget holds the core Dynamics
and Events returned by most widgets.  Encapsulating widget inputs and outputs
this way makes it easier to compose and transform widgets.

-}

module Reflex.Dom.Contrib.Widgets.Common where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Data.Default
import           Data.List
import           Data.Map (Map)
import           Data.Bimap (Bimap)
import qualified Data.Map as M
import qualified Data.Bimap as BM
import           Data.Maybe
import           Data.Readable
import           Data.String.Conv
import           Data.Text (Text)
import           Data.Time
import           GHCJS.DOM.HTMLInputElement hiding (setValue)
import           Reflex
import           Reflex.Dom.Core
import           Safe
------------------------------------------------------------------------------
import           Reflex.Contrib.Utils
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


class HasChange a where
  type Change a :: *
  change :: a -> Change a


------------------------------------------------------------------------------
-- | Generic config structure common to most widgets.  The attributes field
-- may not be used for all widgets, but in that case it can just be ignored.
-- We may want to change this in the future, but it seems like a reasonable
-- start for now.
data WidgetConfig t a
    = WidgetConfig { _widgetConfig_setValue :: Event t a
                   , _widgetConfig_initialValue :: a
                   , _widgetConfig_attributes :: Dynamic t (Map Text Text)
                   }

instance Reflex t => Functor (WidgetConfig t) where
    fmap f (WidgetConfig sv iv a) = WidgetConfig (f <$> sv) (f iv) a


makeLenses ''WidgetConfig

instance (Reflex t, Default a) => Default (WidgetConfig t a) where
  def = WidgetConfig { _widgetConfig_setValue = never
                     , _widgetConfig_initialValue = def
                     , _widgetConfig_attributes = constDyn mempty
                     }

instance HasAttributes (WidgetConfig t a) where
  type Attrs (WidgetConfig t a) = Dynamic t (Map Text Text)
  attributes = widgetConfig_attributes

instance HasSetValue (WidgetConfig t a) where
  type SetValue (WidgetConfig t a) = Event t a
  setValue = widgetConfig_setValue


class IsWidget w where
  ----------------------------------------------------------------------------
  -- | HtmlWidget with a constant value that never fires any events.
  constWidget :: Reflex t => a -> w t a

  ----------------------------------------------------------------------------
  -- | We can't make a Functor instance until Dynamic gets a Functor instance.
  mapWidget :: Reflex t => (a -> b) -> w t a -> w t b


  combineWidgets :: Reflex t => (a -> b -> c) -> w t a -> w t b -> w t c

  ----------------------------------------------------------------------------
  -- | Combines multiple widgets over a Monoid operation.
  wconcat :: (Reflex t, Foldable f, Monoid a) => f (w t a) -> w t a
  wconcat = foldl (combineWidgets (<>)) (constWidget mempty)

  ----------------------------------------------------------------------------
  -- | Since widgets contain Dynamics and Events inside them, we can pull
  -- Dynamic widgets out of the Dynamic.
  extractWidget :: Reflex t => Dynamic t (w t a) -> w t a


------------------------------------------------------------------------------
-- | A general-purpose widget return value.
data Widget0 t a = Widget0
    { _widget0_value    :: Dynamic t a
      -- ^ The authoritative value for this widget.
    , _widget0_change   :: Event t a
      -- ^ Event that fires when the widget changes internally (not via a
      -- setValue event).
    }

makeLenses ''Widget0

instance Reflex t => Functor (Widget0 t) where
  fmap f (Widget0 v c) = Widget0 (f <$> v) (f <$> c)

instance HasValue (Widget0 t a) where
  type Value (Widget0 t a) = Dynamic t a
  value = _widget0_value


instance HasChange (Widget0 t a) where
  type Change (Widget0 t a) = Event t a
  change = _widget0_change


instance IsWidget Widget0 where
  constWidget a = Widget0 (constDyn a) never
  mapWidget f w = Widget0 (f <$> value w) (f <$> _widget0_change w)
  combineWidgets f a b = Widget0 c cChange
    where
      c = zipDynWith f (value a) (value b)
      cChange = tagPromptlyDyn c $ leftmost
        [() <$ _widget0_change a, () <$ _widget0_change b]
  extractWidget dw = Widget0 v c
    where
      v = extractDyn value dw
      c = extractEvent _widget0_change dw


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


instance HasChange (HtmlWidget t a) where
  type Change (HtmlWidget t a) = Event t a
  change = _hwidget_change


htmlTo0 :: HtmlWidget t a -> Widget0 t a
htmlTo0 w = Widget0 (_hwidget_value w) (_hwidget_change w)


------------------------------------------------------------------------------
-- | Generalized form of many widget functions.
type GWidget t m a = WidgetConfig t a -> m (HtmlWidget t a)


instance IsWidget HtmlWidget where
  constWidget a = HtmlWidget (constDyn a) never never never never (constDyn False)
  mapWidget f w =
      HtmlWidget
        (f <$> value w)
        (f <$> _hwidget_change w)
        (_hwidget_keypress w)
        (_hwidget_keydown w)
        (_hwidget_keyup w)
        (_hwidget_hasFocus w)
  combineWidgets f a b =
      HtmlWidget
        newVal newChange
        (leftmost [_hwidget_keypress a, _hwidget_keypress b])
        (leftmost [_hwidget_keydown a, _hwidget_keydown b])
        (leftmost [_hwidget_keyup a, _hwidget_keyup b])
        newFocus
    where
      newVal = zipDynWith f (value a) (value b)
      newChange = tagPromptlyDyn newVal $ leftmost
        [() <$ _hwidget_change a, () <$ _hwidget_change b]
      newFocus = zipDynWith (||) (_hwidget_hasFocus a) (_hwidget_hasFocus b)
  extractWidget dynWidget = HtmlWidget v c kp kd ku hf
    where
      v = extractDyn value dynWidget
      c = extractEvent _hwidget_change dynWidget
      kp = extractEvent _hwidget_keypress dynWidget
      kd = extractEvent _hwidget_keydown dynWidget
      ku = extractEvent _hwidget_keyup dynWidget
      hf = extractDyn _hwidget_hasFocus dynWidget


------------------------------------------------------------------------------
-- | Sometimes the HTML time widget gives you 12:30 and sometimes 12:30:00, so
-- we need a lenient parser.
lenientTimeParser :: String -> Maybe UTCTime
lenientTimeParser s = foldl1' (<|>) $ map (($ s) . parse) fmts
  where
    parse = parseTimeM True defaultTimeLocale
    fmts = ["%F %R", "%F %X"]


------------------------------------------------------------------------------
-- | Input widget for datetime values.
dateTimeWidget
    :: (MonadWidget t m)
    => GWidget t m (Maybe UTCTime)
dateTimeWidget cfg = do
    let wValue = _widgetConfig_setValue cfg
        setDate = maybe mempty (toS . formatTime defaultTimeLocale dfmt)
        setTime = maybe mempty (toS . formatTime defaultTimeLocale tfmt)
    el "div" $ do
        di <- htmlTextInput "date" $ WidgetConfig
          (setDate <$> wValue)
          (setDate (_widgetConfig_initialValue cfg))
          (_widgetConfig_attributes cfg)
        ti <- htmlTextInput "time" $ WidgetConfig
          (setTime <$> wValue)
          (setTime (_widgetConfig_initialValue cfg))
          (_widgetConfig_attributes cfg)
        return $ combineWidgets
                   (\d t -> lenientTimeParser $ toS d ++ " " ++ toS t)
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
    di <- htmlTextInput "date" $ WidgetConfig
      setVal (showD (_widgetConfig_initialValue cfg))
      (_widgetConfig_attributes cfg)
    return $ mapWidget (parseTimeM True defaultTimeLocale fmt . toS) di
  where
    fmt = "%F"
    showD = maybe "" (toS . formatTime defaultTimeLocale fmt)


------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's checkbox.
htmlCheckbox
    :: MonadWidget t m
    => GWidget t m Bool
htmlCheckbox cfg = do
    cb <- checkbox (_widgetConfig_initialValue cfg) $ def
      & setValue .~ _widgetConfig_setValue cfg
      & attributes .~ _widgetConfig_attributes cfg
    return $ HtmlWidget (_checkbox_value cb) never -- FIXME Wrong
                        never never never (constDyn False)


------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's textInput.
htmlTextInput
    :: MonadWidget t m
    => Text
    -> GWidget t m Text
htmlTextInput inputType cfg = do
    (_,w) <- htmlTextInput' inputType cfg
    return w


------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's textInput that also returns the
-- HTMLInputElement.
htmlTextInput'
    :: MonadWidget t m
    => Text
    -> WidgetConfig t Text
    -> m (HTMLInputElement, HtmlWidget t Text)
htmlTextInput' inputType cfg = do
    ti <- textInput $ def
      & setValue .~ _widgetConfig_setValue cfg
      & attributes .~ _widgetConfig_attributes cfg
      & textInputConfig_initialValue .~ _widgetConfig_initialValue cfg
      & textInputConfig_inputType .~ inputType
    let w = HtmlWidget
          (_textInput_value ti)
          (_textInput_input ti)
          (fromIntegral <$> _textInput_keypress ti)
          (fromIntegral <$> _textInput_keydown ti)
          (fromIntegral <$> _textInput_keyup ti)
          (_textInput_hasFocus ti)
    return (_textInput_element ti, w)


------------------------------------------------------------------------------
-- | NOTE: You should probably not use this function with string types because
-- the Show instance will quote strings.
readableWidget
    :: (MonadWidget t m, Show a, Readable a)
    => GWidget t m (Maybe a)
readableWidget cfg = do
    let setVal = maybe "" tshow <$> _widgetConfig_setValue cfg
    w <- htmlTextInput "text" $ WidgetConfig setVal
      (maybe "" tshow (_widgetConfig_initialValue cfg))
      (_widgetConfig_attributes cfg)
    let parse = fromText . toS
    return $ mapWidget parse w


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
-- | Dropdown widget that takes a dynamic list of items and a function
-- generating a String representation of those items.
htmlDropdown
    :: (MonadWidget t m, Eq b)
    => Dynamic t [a]
    -> (a -> Text)
    -> (a -> b)
    -> WidgetConfig t b
    -> m (Widget0 t b)
htmlDropdown items f payload cfg = do
    let pairs = zip [(0::Int)..] <$> items
        m = M.fromList <$> pairs
        dynItems = M.map f <$> m
        findIt ps a = maybe 0 fst $ headMay (filter (\ (_,x) -> payload x == a) ps)
        setVal = attachPromptlyDynWith findIt pairs $ _widgetConfig_setValue cfg
    d <- dropdown 0 dynItems $
           DropdownConfig setVal (_widgetConfig_attributes cfg)
    let val = zipDynWith (\k x -> payload $ fromJust $ M.lookup k x) (_dropdown_value d) m
    return $ Widget0 val (tagPromptlyDyn val $ _dropdown_change d)


------------------------------------------------------------------------------
-- | Dropdown widget that takes a list of items and a function generating a
-- String representation of those items.
--
-- This widget doesn't require your data type to have Read and Show instances
-- like reflex-dom's dropdown function.  It does this by using Int indices
-- into your static list of items in the actual rendered dropdown element.
--
-- But this comes with a price--it has unexpected behavior under insertions,
-- deletions, and reorderings of the list of options.  Because of this, you
-- should probably only use this for static dropdowns where the list of
-- options never changes.
htmlDropdownStatic
    :: (MonadWidget t m, Eq b)
    => [a]
    -> (a -> Text)
    -> (a -> b)
    -> WidgetConfig t b
    -> m (Widget0 t b)
htmlDropdownStatic items f payload cfg = do
    let pairs = zip [(0::Int)..] items
        m = M.fromList pairs
        dynItems = M.map f m
    let findIt a = maybe 0 fst $ headMay (filter (\ (_,x) -> payload x == a) pairs)
    let setVal = findIt <$> _widgetConfig_setValue cfg
    d <- dropdown (findIt $ _widgetConfig_initialValue cfg) (constDyn dynItems) $
           DropdownConfig setVal (_widgetConfig_attributes cfg)
    let val = (\k -> payload $ fromJust $ M.lookup k m) <$> _dropdown_value d
    return $ Widget0 val (tagPromptlyDyn val $ _dropdown_change d)


------------------------------------------------------------------------------
-- | Returns an event that fires when the widget loses focus or enter is
-- pressed.
blurOrEnter
    :: Reflex t
    => HtmlWidget t a
    -> Event t a
blurOrEnter w = tagPromptlyDyn (_hwidget_value w) fireEvent
  where
    fireEvent = leftmost [ () <$ (ffilter (==13) $ _hwidget_keypress w)
                         , () <$ (ffilter not $ updated $ _hwidget_hasFocus w)
                         ]


------------------------------------------------------------------------------
-- | Returns a unit event that fires when the widget loses focus or enter is
-- pressed.  This function does not tagDyn the widget's value like
-- blurOrEnter.
blurOrEnterEvent :: Reflex t => HtmlWidget t a -> Event t ()
blurOrEnterEvent w = leftmost
    [ () <$ (ffilter (==13) $ _hwidget_keypress w)
    , () <$ (ffilter not $ updated $ _hwidget_hasFocus w)
    ]


------------------------------------------------------------------------------
-- | Allows you to restrict when a widget fires and only allow valid values to
-- appear.  If an invalid value is entered, it will revert to the last known
-- good value when the restrict event fires.
enforcingWidget
    :: MonadWidget t m
    => (HtmlWidget t (Maybe a) -> Event t ())
    -> GWidget t m (Maybe a)
    -> GWidget t m a
enforcingWidget restrictEvent wFunc cfg = do
    rec
      let iv = Just $ _widgetConfig_initialValue cfg
          newSetValue = leftmost [ Just <$> _widgetConfig_setValue cfg
                                 , Just <$> resetEvent
                                 ]
      w <- wFunc $ WidgetConfig newSetValue iv
                                (_widgetConfig_attributes cfg)
      let eMay = tag (current $ value w) $ restrictEvent w
          e = fmapMaybe id eMay
      v <- holdDyn (_widgetConfig_initialValue cfg) e
      let resetEvent = tag (current v) $ ffilter isNothing eMay
    return $ HtmlWidget { _hwidget_value = v
                        , _hwidget_change = e
                        , _hwidget_keypress = _hwidget_keypress w
                        , _hwidget_keydown = _hwidget_keydown w
                        , _hwidget_keyup = _hwidget_keyup w
                        , _hwidget_hasFocus = _hwidget_hasFocus w
                        }


------------------------------------------------------------------------------
-- | Allows you to restrict when a widget fires.  For instance,
-- @restrictWidget blurOrEnter@ restricts a widget so it only fires on blur
-- or when enter is pressed.
restrictWidget
    :: MonadWidget t m
    => (HtmlWidget t a -> Event t a)
    -> GWidget t m a
    -> GWidget t m a
restrictWidget restrictFunc wFunc cfg = do
    w <- wFunc cfg
    let e = restrictFunc w
    v <- holdDyn (_widgetConfig_initialValue cfg) e
    return $ w { _hwidget_value = v
               , _hwidget_change = e
               }


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
  -> (a -> Text)
  -> Dynamic t (Map Text Text)
  -> Text
  -> m (Dynamic t (Maybe a))
listDropdown xs f attrs defS = do
  let m = M.fromList . zip [(1::Int)..] <$> xs
      opts = (M.insert 0 defS) . M.map f <$> m
  sel <- liftM _dropdown_value $ dropdown 0 opts $ def & attributes .~ attrs
  return $ zipDynWith M.lookup sel m

------------------------------------------------------------------------------
-- | More efficient dropdown.
-- The dropdown function in reflex-dom takes its list of items as a `Dynamic t
-- (Map k Text)`, but internally, turns that Map into list of pairs using
-- toList, pairs the values and text with an Int index, and then calls
-- fromList to allocate both a Bimap and a Map of this indexed map. This
-- requires two lookups and calling T.readMaybe to get the haskell value out
-- of the selected item.  This function circumvents this by taking a `Dynamic
-- t (Bimap k Text)` instead of a `Dynamic t (Map k Text)` and using the Text
-- for the value attribute instead of an Int. Then, only one lookup is needed
-- and the behavior of the dropdown is more predictable.
dropdownBimap
  :: forall k t m.
     (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k)
  => k
  -> Dynamic t (Bimap k Text)
  -> DropdownConfig t k
  -> m (Dropdown t k)
dropdownBimap k0 options (DropdownConfig setK attrs) = do
    defaultKey <- holdDyn k0 setK
    modifyAttrs <- dynamicAttributesToModifyAttributes attrs
    let indexedOptions :: Dynamic t (Map k Text)
        indexedOptions = BM.toMap <$> options
    let cfg = def
          & selectElementConfig_elementConfig .
            elementConfig_modifyAttributes .~
            fmap mapKeysToAttributeName modifyAttrs
          & selectElementConfig_setValue .~
            attachPromptlyDynWithMaybe (flip BM.lookup) options setK
    (eRaw, _) <- selectElement cfg $ listWithKey indexedOptions $ \k v -> do
      let mkOptionAttrs dk v' = "value" =: v' <>
            if dk == k then "selected" =: "selected" else mempty
      elDynAttr "option" (mkOptionAttrs <$> defaultKey <*> v) $ dynText v
    let eChange :: Event t k
        eChange = attachPromptlyDynWithMaybe safeLookupR options
                    (_selectElement_change eRaw)
    dValue <- holdDyn k0 $ leftmost [eChange, setK]
    pure $ Dropdown dValue eChange
  where
    safeLookupR :: (Ord x, Ord y) => Bimap x y -> y -> Maybe x
    safeLookupR bi a = BM.lookupR a bi
