{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | bulma divs
module Reflex.Bulmex.Tag where

import           Control.Applicative
import           Data.Bool
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Space
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Dom.Widget        as Dom
import qualified Reflex.Tags              as T

container :: Dom.DomBuilder t m => m a -> m a
container = containerClass mempty

containerClass  :: Dom.DomBuilder t m => Text.Text -> m a -> m a
containerClass = partialDiv "container"

partialDiv :: Dom.DomBuilder t m => Text.Text -> Text.Text -> m a -> m a
partialDiv = txtEl T.divClass

txtEl :: (Text.Text -> m a -> m a) -> Text.Text -> Text.Text -> m a -> m a
txtEl = defaultEl spaceJoin

-- | allows us to set a default value for tags by defining a join function
--   not a monoid because often it does it wrong, text needs a space
--    for example in case of classes, and the default map monoid is broken
defaultEl :: (arg -> arg -> arg) -> (arg -> m a -> m a) -> arg -> arg -> m a -> m a
defaultEl monoidF elF a b = elF $ monoidF a b

buttons :: Dom.DomBuilder t m => m a -> m a
buttons = T.divClass "buttons"

-- | kindoff hard to set an image tag in reflex
image :: Dom.DomBuilder t m => Text.Text -> m()
image url = T.imgAttr (Map.singleton "src" url) Dom.blank

-- | first class second src
imageClass :: Dom.DomBuilder t m => Text.Text -> Text.Text -> m()
imageClass clazz url = T.imgAttr (Map.fromList [("src",url), ("class", clazz)]) Dom.blank

-- | bulma hero sturcture
-- | <section class="hero">
-- |  <div class="hero-body">
-- |     <div class="container">
hero :: Dom.DomBuilder t m => Text.Text -> m a -> m a
hero styles =
  txtEl T.sectionClass "hero" styles . T.divClass "hero-body" . container

content :: Dom.DomBuilder t m => m a -> m a
content = T.divClass "content"

sect :: (Dom.DomBuilder t m) => m a -> m a
sect = T.divClass "section"

section :: (PostBuild t m, Dom.DomBuilder t m) => m a -> m a
section = sectionDyn $ constDyn mempty

sectionDyn ::
     (PostBuild t m, Dom.DomBuilder t m) => Dynamic t AttrMap -> m a -> m a
sectionDyn =
  dynAttrEl (\a b -> T.divDynAttr a $ container b) $ classAttr "section"

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

dynAttrEl ::
     Reflex t
  => (Dynamic t AttrMap -> m a -> m a)
  -> AttrMap
  -> Dynamic t AttrMap
  -> m a
  -> m a
dynAttrEl f = defaultEl ((<*>) . (<$>) attrUnion) f . constDyn

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

evtText :: (Dom.DomBuilder t m, PostBuild t m, MonadHold t m) => Event t Text.Text -> m ()
evtText evt = Dom.dynText =<< holdDyn "" evt


labeled' :: Dom.DomBuilder t m => m () -> m a -> m a
labeled' labelM inputM = do
  labelEl labelM
  control inputM

-- | named labelEl cause didn't want to fix name clashes
labelEl :: Dom.DomBuilder t m => m a -> m a
labelEl = labelClass mempty

labelClass :: Dom.DomBuilder t m => Text.Text -> m a -> m a
labelClass = txtEl T.labelClass "label"

data ToolDirection
  = Top -- top is graphic bugged
  | Lft
  | Rght
  | Down

-- | balloon css, for example: https://cdnjs.cloudflare.com/ajax/libs/balloon-css/0.5.0/balloon.min.css
tooltipText ::
     (PostBuild t m, Dom.DomBuilder t m) => Dynamic t Text.Text -> m a -> m a
tooltipText = tooltipText' Rght

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

-- | sometimes you just need 2 pieces of text to seperate with a space
textSpace :: Dom.DomBuilder t m => m ()
textSpace = Dom.text space

flask :: (PostBuild t m, Dom.DomBuilder t m) => m ()
flask = icon "flask"

-- | a html tag that accepts any text into it's href value
ahref :: (Dom.DomBuilder t m, PostBuild t m) => Text.Text -> m a -> m a
ahref = ahref' mempty

ahref' :: (Dom.DomBuilder t m, PostBuild t m) => AttrMap -> Text.Text -> m a -> m a
ahref' uno = ahrefDyn (constDyn uno) . constDyn

ahrefDyn :: (Dom.DomBuilder t m, PostBuild t m) => Dynamic t AttrMap -> Dynamic t Text.Text -> m a -> m a
ahrefDyn uno txt = T.aDynAttr $ (attrUnion <$> uno) <*> (Map.singleton "href" <$> txt)

switchDiv ::
     (PostBuild t m, Dom.DomBuilder t m)
  => Dynamic t Bool
  -> m ()
  -> m a
  -> m a
switchDiv attrDyn true false = do
  hideDiv_ (not <$> attrDyn) true
  hideDiv_ attrDyn false

-- | when dynamic is true ishidden will be added, else the attrmap is used
hideDiv ::
     (PostBuild t m, Dom.DomBuilder t m)
  => Dynamic t AttrMap
  -> Dynamic t Bool
  -> m a
  -> m a
hideDiv attrDyn hide =
  T.divDynAttr $ bool <$> attrDyn <*> constDyn isHidden <*> hide

hideDiv_ :: (PostBuild t m, Dom.DomBuilder t m) => Dynamic t Bool -> m a -> m a
hideDiv_ = hideDiv $ constDyn Map.empty

loadSpinner :: (Dom.DomBuilder t m) => m ()
loadSpinner = imageClass "loadSpinner" "spinner.svg"

hideEmptyDiv ::
     (Eq (f b), Alternative f, PostBuild t m, Dom.DomBuilder t m)
  => Dynamic t (f b)
  -> m a
  -> m a
hideEmptyDiv = hideEmptyDyn $ constDyn Map.empty

hideEmptyDyn ::
     (Eq (f b), Alternative f, PostBuild t m, Dom.DomBuilder t m)
  => Dynamic t AttrMap
  -> Dynamic t (f b)
  -> m a
  -> m a
hideEmptyDyn dyn = hideDiv dyn . fmap ((==) empty)
