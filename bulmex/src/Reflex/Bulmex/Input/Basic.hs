{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Basic inputs with bulma styling
module Reflex.Bulmex.Input.Basic
  ( abutton
  , abuttonClass
  , abuttonLarge
  , abuttonDynAttr
  , buttonClassAttr
  -- * Text inputs
  , txtArea
  , txtInput
  ) where

import           Control.Lens
import qualified Data.Text                       as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Input.Polymorphic
import qualified Reflex.Dom.Builder.Class        as Dom
import qualified Reflex.Dom.Widget               as Dom
import qualified Reflex.Tags                     as T

txtArea ::
     (Dom.DomBuilder t m, PostBuild t m)
  => Dom.TextAreaConfig t
  -> m (TextArea t)
txtArea conf =
  textArea $
  conf &
  Dom.textAreaConfig_attributes %~
  ((<*>) $ constDyn $ attrUnion $ classAttr "textarea")

-- | Unlabeled text input with input class applied
txtInput ::
     (PostBuild t m, Dom.DomBuilder t m)
  => Dom.TextInputConfig t
  -> m (TextInput t)
txtInput config =
  textInput
    (config &
     Dom.textInputConfig_attributes %~ \x -> attrUnion <$> inputAttr <*> x)

buttonClassAttr :: AttrMap
buttonClassAttr = classAttr "button"

abuttonLarge :: (PostBuild t m, Dom.DomBuilder t m) => m a -> m (Event t ())
abuttonLarge = fmap fst . abuttonClass "is-large"

abuttonClass ::
     (PostBuild t m, Dom.DomBuilder t m)
  => Text.Text
  -> m a
  -> m (Event t (), a)
abuttonClass = abutton . classAttr

-- | A button around an arbitrary dom element:
--
--   > <button> m a </button>
--
--   It's also styled nicely.
abutton ::
     (PostBuild t m, Dom.DomBuilder t m) => AttrMap -> m a -> m (Event t (), a)
abutton = abuttonDynAttr . constDyn

abuttonDynAttr ::
     (PostBuild t m, Dom.DomBuilder t m)
  => Dynamic t AttrMap
  -> m a
  -> m (Event t (), a)
abuttonDynAttr attrs monadF = do
  over (mapped . _1) (Dom.domEvent Dom.Click) $
    T.aDynAttr' (attrUnion buttonClassAttr <$> attrs) monadF
