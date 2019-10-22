{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Bulma styled tags: https://bulma.io/
module Reflex.Bulmex.Tag.Bulma
  ( container
  , containerClass
  , hero
  , content
  , sect
  , section
  , sectionDyn
  , columns
  , column
  , columnsClass
  , columnClass
  -- * Tiles
  , tile
  , tileChild
  , tileChildClass
  , tileParentClass
  , tileParent
  , tileAncestor
  -- * Form
  , control
  , controlClass
  , controlDyn
  , buttons
  , field
  , fieldClass
  , fieldGrouped
  , labeled'
  , labelEl
  , labelClass
  -- * Title
  , title
  , titleClazz
  , subtitle
  , subtitleClass
  -- * Box
  , box
  ) where

import qualified Data.Text                  as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Tag.Abstract
import qualified Reflex.Dom.Builder.Class   as Dom
import qualified Reflex.Tags                as T

-- | Container: https://bulma.io/documentation/layout/container/
container :: Dom.DomBuilder t m => m a -> m a
container = containerClass mempty

containerClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
containerClass = partialDiv "container"

buttons :: Dom.DomBuilder t m => m a -> m a
buttons = T.divClass "buttons"

-- | bulma hero sturcture https://bulma.io/documentation/layout/hero/
--
-- > <section class="hero">
-- >  <div class="hero-body">
-- >    <div class="container">
hero :: Dom.DomBuilder t m => Text.Text -> m a -> m a
hero styles =
  txtEl T.sectionClass "hero" styles . T.divClass "hero-body" . container

content :: Dom.DomBuilder t m => m a -> m a
content = T.divClass "content"

sect :: (Dom.DomBuilder t m) => m a -> m a
sect = T.divClass "section"

-- | https://bulma.io/documentation/layout/section/
section :: (PostBuild t m, Dom.DomBuilder t m) => m a -> m a
section = sectionDyn $ constDyn mempty

sectionDyn ::
     (PostBuild t m, Dom.DomBuilder t m) => Dynamic t AttrMap -> m a -> m a
sectionDyn =
  dynAttrEl (\a b -> T.divDynAttr a $ container b) $ classAttr "section"

-- | https://bulma.io/documentation/columns/
columns :: Dom.DomBuilder t m => m a -> m a
columns = columnsClass mempty

column :: Dom.DomBuilder t m => m a -> m a
column = columnClass mempty

columnsClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
columnsClass = partialDiv "columns"

columnClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
columnClass = partialDiv "column"

control :: Dom.DomBuilder t m => m a -> m a
control = controlClass mempty

controlClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
controlClass = partialDiv "control"

controlDyn ::
     (PostBuild t m, Dom.DomBuilder t m) => Dynamic t AttrMap -> m a -> m a
controlDyn = dynAttrEl T.divDynAttr $ classAttr "control"

-- | https://bulma.io/documentation/layout/tiles/
tile :: Dom.DomBuilder t m => Text.Text -> m a -> m a
tile = partialDiv "tile"

tileChild :: Dom.DomBuilder t m => m a -> m a
tileChild = tileChildClass mempty

tileChildClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
tileChildClass = txtEl tile "is-child"

tileParentClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
tileParentClass = txtEl tile "is-parent"

tileParent :: Dom.DomBuilder t m => m a -> m a
tileParent = tileParentClass mempty

tileAncestor :: Dom.DomBuilder t m => m a -> m a
tileAncestor = tile "is-ancestor"

field :: Dom.DomBuilder t m => m a -> m a
field = fieldClass mempty

fieldClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
fieldClass = partialDiv "field"

fieldGrouped :: Dom.DomBuilder t m => m a -> m a
fieldGrouped = fieldClass "is-grouped"

title :: Dom.DomBuilder t m => m a -> m a
title = titleClazz "is-1"

titleClazz :: Dom.DomBuilder t m => Text.Text -> m a -> m a
titleClazz = txtEl T.h1Class "title"

subtitle :: Dom.DomBuilder t m => m a -> m a
subtitle = subtitleClass "is-3"

subtitleClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
subtitleClass = txtEl T.h2Class "subtitle"

box :: Dom.DomBuilder t m => m a -> m a
box = T.divClass "box"

labeled' :: Dom.DomBuilder t m => m () -> m a -> m a
labeled' labelM inputM = do
  labelEl labelM
  control inputM

-- | named labelEl cause didn't want to fix name clashes
labelEl :: Dom.DomBuilder t m => m a -> m a
labelEl = labelClass mempty

labelClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
labelClass = txtEl T.labelClass "label"
