{-# LANGUAGE OverloadedStrings #-}

-- | Deal with attribute maps, which revolves mostly around assigning
--   a class so we get appropriate styling
module Reflex.Bulmex.Attr
  ( AttrMap
  , attrUnion
  , isSelectedAttr
  , whenAttr
  , inputAttr
  , classAttr
  , disabled
  , isHidden
  , switchAttr
  , colspan
  ) where

import           Data.Bool
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as Text
import           Reflex
import           Reflex.Bulmex.Space

-- | This type occures to often to not alias
type AttrMap = Map.Map Text.Text Text.Text

isHidden :: AttrMap
isHidden = classAttr "is-hidden"

-- | Unifies all keys by concatinating the values with a whitespace
attrUnion :: AttrMap -> AttrMap -> AttrMap
attrUnion = Map.unionWith spaceJoin

-- | If bool true adds isSelected
isSelectedAttr :: Bool -> AttrMap
isSelectedAttr = whenAttr "is-selected"

whenAttr :: Text.Text -> Bool -> AttrMap
whenAttr = switchAttr mempty

switchAttr :: Text.Text -> Text.Text -> Bool -> AttrMap
switchAttr lies truth = bool (classAttr lies) (classAttr truth)

inputAttr :: Reflex t => Dynamic t AttrMap
inputAttr = constDyn $ classAttr "input"

-- | class attr is what is needed most of the time
classAttr :: Text.Text -> AttrMap
classAttr = Map.singleton "class"

disabled :: AttrMap
disabled = Map.singleton "disabled" "disabled"

colspan :: Int -> AttrMap
colspan = Map.singleton "colspan" . Text.pack . show
