{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Tooltips with balloon css
module Reflex.Bulmex.Tag.Tooltip
  ( tooltipText
  , tooltipText'
  , ToolDirection(..)
  , tipToAttr
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import           Reflex
import           Reflex.Bulmex.Attr
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Tags              as T

data ToolDirection
  = Top -- top is graphic bugged
  | Lft
  | Rght
  | Down

-- | balloon css, for example: https://cdnjs.cloudflare.com/ajax/libs/balloon-css/0.5.0/balloon.min.css
--   uses a span underneath
tooltipText ::
     (PostBuild t m, Dom.DomBuilder t m) => Dynamic t Text.Text -> m a -> m a
tooltipText = tooltipText' Rght

-- | Allows setting of initial direction
tooltipText' ::
     (PostBuild t m, Dom.DomBuilder t m)
  => ToolDirection
  -> Dynamic t Text.Text
  -> m a
  -> m a
tooltipText' dir tipDyn monad = T.spanDynAttr (tipToAttr dir <$> tipDyn) $ monad

tipToAttr :: ToolDirection -> Text.Text -> AttrMap
tipToAttr dir "" = tipToAttr dir "-"
tipToAttr dir tip =
  Map.fromList [("data-balloon", tip), ("data-balloon-pos", direction dir)]

direction :: ToolDirection -> Text.Text
direction Top  = "top"
direction Lft  = "left"
direction Rght = "right"
direction Down = "down"
