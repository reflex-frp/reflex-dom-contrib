{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.Dom.Contrib.Widgets.ButtonGroup where

import Data.Bool
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common


------------------------------------------------------------------------------
buttonGroup'
  :: forall t m a.(MonadWidget t m, Eq a, Show a)
  => (Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t ()))
  -> Dynamic t (Map Int a)
  -> GWidget t m (Maybe a)
buttonGroup' drawDynBtn dynButtons (WidgetConfig wcSet wcInit wcAttrs) =

  elDynAttr "div" wcAttrs $ mdo

    pb <- getPostBuild

    let externalSet :: Event t (Maybe Int)
        externalSet = attachWith revLookup (current dynButtons) wcSet

    let initSet :: Event t (Maybe Int)
        initSet = attachWith revLookup (current dynButtons) (traceEvent "INIT" $ wcInit <$ pb)

    let internalSet :: Event t (Maybe Int)
        internalSet = leftmost [initSet, clickSelEvents]

        internalV :: Event t (Maybe a)
        internalV = attachWith (\m k -> k >>= flip Map.lookup m)
                               (current dynButtons)
                               internalSet

    dynK :: Dynamic t (Maybe Int) <- holdDyn Nothing $ leftmost [internalSet, externalSet]

    dynButtons' :: Dynamic t (Map (Maybe Int) a) <- mapDyn
                                                    (mapKeys Just)
                                                    dynButtons

    clickSelEvents :: Event t (Maybe Int) <- selectViewListWithKey_
                                        dynK
                                        dynButtons'
                                        drawDynBtn


    dynSelV :: Dynamic t (Maybe a) <- combineDyn
                                      (\k m -> k >>= flip Map.lookup m)
                                      dynK dynButtons
    --      HTMLWidget _hwidget_value _hwidget_change _hwidget_keypress _hwidget_keydown _hwidget_keyup _hwidget_hasfocus
    return (HtmlWidget dynSelV (traceEvent "internalV: " internalV) never never never (constDyn False))


------------------------------------------------------------------------------
revLookup :: Eq a => Map Int a -> Maybe a -> Maybe Int
revLookup _ Nothing  = Nothing
revLookup m (Just v) = listToMaybe . Map.keys $ Map.filter (== v) m


------------------------------------------------------------------------------
revLookup' :: Eq a => [(String, a)] -> Maybe a -> Maybe (String, a)
revLookup' _ Nothing  = Nothing
revLookup' m (Just v) = listToMaybe $ Prelude.filter ((== v) . snd) m


------------------------------------------------------------------------------
bootstrapButtonGroup :: forall t m a.(MonadWidget t m, Eq a, Show a)
                     => Dynamic t [(a,String)]
                     -> GWidget t m (Maybe a)
bootstrapButtonGroup dynEntryList cfg = do
  btns :: Dynamic t (Map.Map Int a) <- forDyn dynEntryList $ \pairs ->
    Map.fromList (zip [1..] (Prelude.map fst pairs))

  -- let externalSets :: Event t (Maybe (String, a))
  --     externalSets = attachWith (\m mayV -> revLookup' m mayV) (current dynEntryList) (_widgetConfig_setValue cfg)

  divAttrs <- forDyn (_widgetConfig_attributes cfg) $ \attrs ->
    attrs <> "class"      =: "btn-group"
          <> "role"       =: "group"
          <> "aria-label" =: "..."

  buttonGroup' handleOne btns
    (WidgetConfig {_widgetConfig_attributes = divAttrs
                  ,_widgetConfig_setValue   = _widgetConfig_setValue cfg
                  ,_widgetConfig_initialValue = _widgetConfig_initialValue cfg
                  })

  return groupWidget

  where
    handleOne i dynV dynChecked = do
      txt :: Dynamic t (Maybe String) <- combineDyn (\v m -> Prelude.lookup v m) dynV dynEntryList
      txt' :: Dynamic t String <- mapDyn (fromMaybe "") txt
      btnAttrs <- forDyn dynChecked $ \b ->
           "type" =: "button"
        <> "class" =: ("btn btn-default" <> bool "" " active" checked)
      (b,_) <- elDynAttr' "button" btnAttrs $ (dynText txt')
      return (Click `domEvent` b)



------------------------------------------------------------------------------
-- radioGroup :: forall t m a.MonadWidget t m
--            => Dynamic t [(String,a)]
--            -> GWidget t m (Maybe (a))
radioGroup' name dynEntryList cfg = do
  btns <- forDyn dynEntryList $ \pairs ->
    Map.fromList (zip [1..] pairs)

  -- attrs' <- forDyn (_widgetConfig_attributes cfg) $ \attrs -> attrs <> "type" =: "radio"
  labeledPairGrp :: HtmlWidget t (Maybe (String, a)) <- buttonGroup' handleOne btns cfg -- (cfg {_widgetConfig_attributes = attrs'})
  return labeledPairGrp
  mapWidget m labeledPairGrp

  where

    -- handleOne :: Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t ())
    handleOne _ dynLabeledVal dynChecked = do
      btnAttrs <- combineDyn btnAttrs dynLabeledVal dynChecked
      (b,_) <- elDynAttr' "input" btnAttrs $ return ()
      (dynText =<< mapDyn fst dynLabeledVal)
      el "br" $ return ()
      return (Click `domEvent` b)

    btnAttrs (lbl,val) checked = "type" =: "radio"
                              <> "name" =: name
                              <> bool mempty ("checked" =: "checked") checked
    --renderOne (nm,_) b = "type" =: "radio"
    m Nothing      = Nothing
    m (Just (x,y)) = Just y



demo :: IO ()
demo = mainWidget $ do
  res <- buttonGroup' (\_ dynV dynB -> do
                           dynAttr <- forDyn dynB $ \b ->
                             "style" =: ("background:" <> bool "blue" "green" b)
                           (e,_) <- elDynAttr' "div" dynAttr $ display dynV
                           return (Click `domEvent` e)
                      ) (constDyn (Map.fromList [(1, "G"),(2, "H")] )) def
  display (_hwidget_value res)
  res' <- radioGroup' "g" (constDyn [("Test",100), ("Test2", 200), ("Hello", 300)]) def
  display (_hwidget_value res')
  res'' <- bootstrapButtonGroup (constDyn [(100,"Test"), (200,"Test2"), (300,"Hello")]) def
  display (_hwidget_value res'')

