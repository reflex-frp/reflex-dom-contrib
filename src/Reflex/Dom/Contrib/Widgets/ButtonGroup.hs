{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.Dom.Contrib.Widgets.ButtonGroup where

import Data.Map
import qualified Data.Map as Map
import Data.Monoid
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common


------------------------------------------------------------------------------
buttonGroup'
  :: forall t m a.MonadWidget t m
  => (Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t ()))
  -> Dynamic t (Map Int a)
  -> GWidget t m (Maybe a)
buttonGroup' drawDynBtn dynButtons cfg =

  elDynAttr "div" (_widgetConfig_attributes cfg) $ mdo

    dynK :: Dynamic t (Maybe Int) <- holdDyn Nothing $ selEvents

    dynButtons' :: Dynamic t (Map (Maybe Int) a) <- mapDyn
                                                    (mapKeys Just)
                                                    dynButtons

    selEvents :: Event t (Maybe Int) <- selectViewListWithKey_
                                        dynK
                                        dynButtons'
                                        drawDynBtn


    dynSelV :: Dynamic t (Maybe a) <- combineDyn
                                      (\k m -> k >>= flip Map.lookup m)
                                      dynK dynButtons
    return (HtmlWidget dynSelV undefined never never never (constDyn False))





------------------------------------------------------------------------------
bootstrapButtonGroup :: MonadWidget t m => Dynamic t [(String,a)] -> GWidget t m (Maybe a)
bootstrapButtonGroup dynEntryList cfg = do
  btns <- forDyn dynEntryList $ \pairs ->
    Map.fromList (zip [1..] pairs)

  divAttrs <- forDyn (_widgetConfig_attributes cfg) $ \attrs ->
    attrs <> "class" =: "btn-group"
          <> "role" =: "group"
          <> "aria-label" =: "..."
  groupWidget <- buttonGroup' handleOne btns (def {_widgetConfig_attributes = divAttrs})
  mapWidget m groupWidget

  where
    handleOne _ dynLabeledVal dynChecked = do
      btnAttrs <- combineDyn btnAttrs dynLabeledVal dynChecked
      (b,_) <- elDynAttr' "button" btnAttrs $ (dynText =<< mapDyn fst dynLabeledVal)
      return (Click `domEvent` b)

    btnAttrs (lbl, val) checked = "type" =: "button"
                               <> "class" =: ("btn btn-default" <> bool "" " active" checked)

    m Nothing      = Nothing
    m (Just (k,v)) = Just v

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
  res'' <- bootstrapButtonGroup (constDyn [("Test",100), ("Test2", 200), ("Hello", 300)]) def
  display (_hwidget_value res'')

