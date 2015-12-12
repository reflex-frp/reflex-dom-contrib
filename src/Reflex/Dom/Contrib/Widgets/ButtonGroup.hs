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
-- radioGroup :: forall t m a.MonadWidget t m
--            => Dynamic t [(String,a)]
--            -> GWidget t m (Maybe (a))
radioGroup dynEntryList cfg = do
  btns <- forDyn dynEntryList $ \pairs ->
    Map.fromList (zip [1..] pairs)

  attrs' <- forDyn (_widgetConfig_attributes cfg) $ \attrs -> attrs <> "type" =: "radio"
  labeledPairGrp :: HtmlWidget t (Maybe (String, a)) <- buttonGroup' handleOne btns (cfg {_widgetConfig_attributes = attrs'})
  return labeledPairGrp
  mapWidget m labeledPairGrp

  where

    -- handleOne :: Maybe Int -> Dynamic t a -> Dynamic t Bool -> m (Event t ())
    handleOne _ dynLabeledVal dynChecked = do
      --attrs <- combineDyn btnAttrs dynLabeledVal dynChecked
      (b,_) <- elAttr' "input" ("type" =: "radio") $
        (dynText =<< mapDyn fst dynLabeledVal)
      return (Click `domEvent` b)

    --renderOne (nm,_) b = "type" =: "radio"
    m Nothing      = Nothing
    m (Just (x,y)) = Just y
