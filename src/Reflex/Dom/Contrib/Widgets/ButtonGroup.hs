{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.Dom.Contrib.Widgets.ButtonGroup where

import Data.Foldable
import Data.Traversable
import Data.Bool
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common


------------------------------------------------------------------------------
buttonGroup
  :: forall t m a.(MonadWidget t m, Eq a, Show a)
  => (Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t (), Dynamic t Bool))
  -> Dynamic t (Map.Map Int a)
  -> GWidget t m (Maybe a)
buttonGroup drawDynBtn dynButtons (WidgetConfig wcSet wcInit wcAttrs) =

  elDynAttr "div" wcAttrs $ mdo

    pb <- getPostBuild

    let externSet   = attachWith revLookup (current dynButtons) wcSet
        initSet     = attachWith revLookup (current dynButtons) (wcInit <$ pb)
        internSet   = leftmost [initSet, clickSelEvents]
        internalV   = attachWith (\m k -> k >>= flip Map.lookup m)
                                 (current dynButtons)
                                 internSet

    dynDynFocusMap  <- forDyn dynButtons $ \btnMap -> for btnMap $ \btn -> do
      (_,isFocused) <- drawDynBtn
      -- flip traverse btnMap $ \(_,f) -> _

    dynFocusMap <- joinDynThroughMap dynDynFocusMap
    dynAnyFocus :: Dynamic t Bool <- forDyn dynFocusMap $ \focusMap -> any id focusMap

    dynK <- holdDyn Nothing $ leftmost [internSet, externSet]

    dynButtons'  <- mapDyn (Map.mapKeys Just) dynButtons

    clickSelEvents <- selectViewListWithKey_ dynK dynButtons'
                      (\k v b -> drawDynBtn k v b >>= \(e,_) -> return e)

    dynSelV <- combineDyn (\k m -> k >>= flip Map.lookup m) dynK dynButtons

    --      HTMLWidget _hwidget_value _hwidget_change _hwidget_keypress _hwidget_keydown _hwidget_keyup _hwidget_hasfocus
    return (HtmlWidget dynSelV internalV never never never dynAnyFocus)


-- selectViewListWithKey_' :: forall t m k v a. (MonadWidget t m, Ord k) => Dynamic t k -> Dynamic t (Map k v) -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -> m (Event t k, Dynamic t Bool)
-- selectViewListWithKey_' selection vals mkChild = do
--   let selectionDemux = demux selection -- For good performance, this value must be shared across all children
--   selectChild <- listWithKey vals $ \k v -> do
--     selected <- getDemuxed selectionDemux k
--     selectSelf <- mkChild k v selected
--     return $ fmap (const k) selectSelf
--   selEvents <- liftM switchPromptlyDyn $ mapDyn (leftmost . Map.elems) selectChild
--   return (selEvents, constDyn False)

------------------------------------------------------------------------------
revLookup :: Eq a => Map.Map Int a -> Maybe a -> Maybe Int
revLookup _ Nothing  = Nothing
revLookup m (Just v) = listToMaybe . Map.keys $ Map.filter (== v) m


------------------------------------------------------------------------------
bootstrapButtonGroup :: forall t m a.(MonadWidget t m, Eq a, Show a)
                     => Dynamic t [(a,String)]
                     -> GWidget t m (Maybe a)
bootstrapButtonGroup dynEntryList cfg = do
  btns :: Dynamic t (Map.Map Int a) <- forDyn dynEntryList $ \pairs ->
    Map.fromList (zip [1..] (Prelude.map fst pairs))

  divAttrs <- forDyn (_widgetConfig_attributes cfg) $ \attrs ->
    attrs <> "class"      =: "btn-group"
          <> "role"       =: "group"
          <> "aria-label" =: "..."

  buttonGroup handleOne btns
    (WidgetConfig {_widgetConfig_attributes   = divAttrs
                  ,_widgetConfig_setValue     = _widgetConfig_setValue cfg
                  ,_widgetConfig_initialValue = _widgetConfig_initialValue cfg
                  })

  where

    handleOne i dynV dynChecked = do
      txt <- combineDyn (\v m -> fromMaybe "" $ Prelude.lookup v m)
                        dynV dynEntryList
      btnAttrs <- forDyn dynChecked $ \b ->
           "type"  =: "button"
        <> "class" =: ("btn btn-default" <> bool "" " active" b)
      (b,_) <- elDynAttr' "button" btnAttrs $ dynText txt
      f     <- holdDyn False $ leftmost [ False <$ (Blur `domEvent`  b)
                                        , True  <$ (Focus `domEvent` b)]
      return (Click `domEvent` b, f)



-- ------------------------------------------------------------------------------
-- radioGroup :: forall t m a.MonadWidget t m
--            => Dynamic t [(a,String)]
--            -> GWidget t m (Maybe a)
-- radioGroup name dynEntryList cfg = do
--   btns <- forDyn dynEntryList $ \pairs ->
--     Map.fromList (zip [1..] (map fst pairs))

--   labeledPairGrp :: HtmlWidget t (Maybe (String, a)) <- buttonGroup handleOne btns cfg -- (cfg {_widgetConfig_attributes = attrs'})
--   return labeledPairGrp
--   mapWidget m labeledPairGrp

--   where

--     -- handleOne :: Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t ())
--     handleOne _ dynLabeledVal dynChecked = do
--       btnAttrs <- combineDyn btnAttrs dynLabeledVal dynChecked
--       (b,_) <- elDynAttr' "input" btnAttrs $ return ()
--       (dynText =<< mapDyn fst dynLabeledVal)
--       el "br" $ return ()
--       return (Click `domEvent` b)

--     btnAttrs (lbl,val) checked = "type" =: "radio"
--                               <> "name" =: name
--                               <> bool mempty ("checked" =: "checked") checked
--     --renderOne (nm,_) b = "type" =: "radio"
--     m Nothing      = Nothing
--     m (Just (x,y)) = Just y



-- demo :: IO ()
-- demo = mainWidget $ do
--   res <- buttonGroup (\_ dynV dynB -> do
--                            dynAttr <- forDyn dynB $ \b ->
--                              "style" =: ("background:" <> bool "blue" "green" b)
--                            (e,_) <- elDynAttr' "div" dynAttr $ display dynV
--                            return (Click `domEvent` e)
--                       ) (constDyn (Map.fromList [(1, "G"),(2, "H")] )) def
--   display (_hwidget_value res)
--   res' <- radioGroup' "g" (constDyn [("Test",100), ("Test2", 200), ("Hello", 300)]) def
--   display (_hwidget_value res')
--   res'' <- bootstrapButtonGroup (constDyn [(100,"Test"), (200,"Test2"), (300,"Hello")]) def
--   display (_hwidget_value res'')

