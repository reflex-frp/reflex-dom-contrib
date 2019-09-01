{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Contrib.Widgets.ButtonGroup (
  radioGroup,
  bootstrapButtonGroup,
  buttonGroup
) where

------------------------------------------------------------------------------
import           Control.Monad.Fail
import           Control.Monad.IO.Class     (liftIO)
import           Data.Bool                  (bool)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           GHCJS.DOM.Element          (Element (..))
import           GHCJS.DOM.HTMLInputElement (setChecked, HTMLInputElement (..))
import           GHCJS.DOM.Types            (MonadJSM, castTo)
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Reflex.Dom.Contrib.Widgets.Common
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A general container for collecting drawable buttons into a group with
--   one selection, under the HtmlWidget interface
buttonGroup
  :: forall t m a.(MonadWidget t m, Eq a)
  => Text
     -- ^ Html tag name for the container (normally @"div"@ or @"table"@)
  -> (Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t (), Dynamic t Bool))
     -- ^ WidgetMonadic action for rendering a single button, returning click events and focus state
  -> Dynamic t (Map.Map Int a)
     -- ^ Mapping from indices (used for layout order) to result elements
  -> GWidget t m (Maybe a)
     -- ^ Returning a GWidget (function from 'WidgetConfig' to 'HtmlWidget')
buttonGroup htmlTag drawDynBtn dynButtons (WidgetConfig wcSet wcInit wcAttrs) = do

  (parent, child) <- elDynAttr' htmlTag wcAttrs $ mdo

    pb <- getPostBuild

    let externSet   = attachWith revLookup (current dynButtons) wcSet
        initSet     = attachWith revLookup (current dynButtons) (wcInit <$ pb)
        internSet   = leftmost [initSet, clickSelEvents]
        internalV   = attachWith (\m k -> k >>= flip Map.lookup m)
                                 (current dynButtons)
                                 internSet

    dynK <- holdDyn Nothing $ leftmost [internSet, externSet]

    let dynButtons'  = Map.mapKeys Just <$> dynButtons

    (clickSelEvents, hasFocus) <- selectViewListWithKey_' dynK dynButtons' drawDynBtn

    let dynSelV = zipDynWith (\k m -> k >>= flip Map.lookup m) dynK dynButtons

    return (HtmlWidget dynSelV internalV never never never hasFocus)

  let keyp = Keypress `domEvent` parent
      keyu = Keyup    `domEvent` parent
      keyd = Keydown  `domEvent` parent
  return $ child { _hwidget_keypress = fromIntegral <$> keyp
                 , _hwidget_keyup    = fromIntegral <$> keyu
                 , _hwidget_keydown  = fromIntegral <$> keyd }


------------------------------------------------------------------------------
-- | Modified selectViewListWithKey from Reflex.Dom.Widget.Basic,
--   This one also passes back a 'Dynamic t Bool' indicating that at least
--   one child element is in focus
selectViewListWithKey_' :: forall t m k v a. (MonadWidget t m, Ord k)
                        => Dynamic t k
                        -> Dynamic t (Map.Map k v)
                        -> (k -> Dynamic t v
                              -> Dynamic t Bool
                              -> m (Event t a, Dynamic t Bool))
                        -> m (Event t k, Dynamic t Bool)
selectViewListWithKey_' selection vals mkChild = do
  let selectionDemux = demux selection -- For good performance, this value must be shared across all children

  selectChildAndFocus <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    (selectSelf, selfFocus) <- mkChild k v selected
    return (fmap (const k) selectSelf, selfFocus)

  let selectChild = Map.map fst <$> selectChildAndFocus
      selEvents = switchPromptlyDyn $ leftmost . Map.elems <$> selectChild
      focusMap = joinDynThroughMap $ Map.map snd <$> selectChildAndFocus
      dynFocused = any id <$> focusMap

  return (selEvents, dynFocused)


------------------------------------------------------------------------------
-- | Helper function finding a value's first key in a map
revLookup :: Eq a => Map.Map Int a -> Maybe a -> Maybe Int
revLookup _ Nothing  = Nothing
revLookup m (Just v) = listToMaybe . Map.keys $ Map.filter (== v) m


------------------------------------------------------------------------------
-- | Produce a bootstrap <http://v4-alpha.getbootstrap.com/components/button-group/ button group>
bootstrapButtonGroup :: forall t m a.(MonadWidget t m, Eq a)
                     => Dynamic t [(a,Text)]
                        -- ^ Selectable values and their string labels
                     -> GWidget t m (Maybe a)
                        -- ^ Button group in a 'GWidget' interface (function from 'WidgetConfig' to 'HtmlWidget' )
bootstrapButtonGroup dynEntryList cfg = do
  let btns :: Dynamic t (Map.Map Int a) = (\pairs ->
        Map.fromList (zip [1..] (Prelude.map fst pairs))) <$> dynEntryList

  let divAttrs = mkAttrs <$> _widgetConfig_attributes cfg

  buttonGroup "div" handleOne btns
    (WidgetConfig {_widgetConfig_attributes   = divAttrs
                  ,_widgetConfig_setValue     = _widgetConfig_setValue cfg
                  ,_widgetConfig_initialValue = _widgetConfig_initialValue cfg
                  })

  where

    mkAttrs attrs = attrs <> "class"      =: "btn-group"
                          <> "role"       =: "group"
                          <> "aria-label" =: "..."
    handleOne _ dynV dynChecked = do
      let txt = zipDynWith (\v m -> fromMaybe "" $ Prelude.lookup v m)
                        dynV dynEntryList
          btnAttrs = (\b ->
           "type"  =: "button" <>
           "class" =: ("btn btn-default" <> bool "" " active" b)) <$> dynChecked
      (b,_) <- elDynAttr' "button" btnAttrs $ dynText txt
      f     <- holdDyn False $ leftmost [ False <$ (Blur  `domEvent` b)
                                        , True  <$ (Focus `domEvent` b)]
      return (Click `domEvent` b, f)



------------------------------------------------------------------------------
-- | A group of radio buttons in a table layout
radioGroup :: forall t m a.(MonadWidget t m, Eq a, MonadJSM IO, MonadFail m)
           => Dynamic t Text
              -- ^ The name for the button group (different groups should be given different names)
           -> Dynamic t [(a,Text)]
              -- ^ Selectable values and their string labels
           -> GWidget t m (Maybe a)
              -- ^ Radio group in a 'GWidget' interface (function from 'WidgetConfig' to 'HtmlWidget' )
radioGroup dynName dynEntryList cfg = do
  let btns = (\pairs ->
        Map.fromList (zip [1..] (map fst pairs))) <$> dynEntryList

  buttonGroup "table" handleOne btns cfg

  where

    mkBtnAttrs nm chkd =
        "type" =: "radio"
     <> "name" =: nm
     <> bool mempty ("checked" =: "checked") chkd
    handleOne _ dynV dynChecked = do

      el "tr" $ el "td" $ el "label" $  do
        let txt = zipDynWith (\v m -> fromMaybe "" $ Prelude.lookup v m)
                             dynV dynEntryList

            btnAttrs = mkBtnAttrs <$> dynName <*> dynChecked
        (b,_) <- elDynAttr' "input" btnAttrs $ return ()
        f <- holdDyn False $ leftmost [ False <$ (Blur  `domEvent` b)
                                      , True  <$ (Focus `domEvent` b)]
        dynText txt
--        let e = castToHTMLInputElement $ _element_raw b
        Just e <- castTo HTMLInputElement $ _element_raw b
        _ <- performEvent $ (liftIO . setChecked e) <$> updated dynChecked
        return (Click `domEvent` b, f)
