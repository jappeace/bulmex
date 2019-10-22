{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Icons with material design
module Reflex.Bulmex.Tag.Icon
  ( icon
  , iconClass
  , iconDyn
   -- * Flask
  , flask
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Tag.Odd
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Dom.Widget        as Dom
import qualified Reflex.Tags              as T

icon :: (PostBuild t m, Dom.DomBuilder t m) => Text.Text -> m ()
icon = iconClass ""

-- | second is name
iconClass ::
     (PostBuild t m, Dom.DomBuilder t m) => Text.Text -> Text.Text -> m ()
iconClass wrapClass =
  iconDyn (constDyn (classAttr wrapClass)) . constDyn . classAttr

iconDyn ::
     (PostBuild t m, Dom.DomBuilder t m)
  => Dynamic t AttrMap
  -> Dynamic t AttrMap
  -> m ()
iconDyn wrapClass mdiClass = do
  textSpace
  T.spanDynAttr (attrUnion (classAttr "icon") <$> wrapClass) $
    T.iDynAttr (Map.unionWith (<>) (classAttr "mdi mdi-") <$> mdiClass) $
    Dom.blank
  textSpace

flask :: (PostBuild t m, Dom.DomBuilder t m) => m ()
flask = icon "flask"
