{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Abstract tags, most tags have very similiar overload mechanisms
--   this provides an abstraction over that
module Reflex.Bulmex.Tag.Abstract
  ( defaultEl
  , txtEl
  , dynAttrEl
  , partialDiv
  ) where

import qualified Data.Text                as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Space
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Tags              as T

partialDiv :: Dom.DomBuilder t m => Text.Text -> Text.Text -> m a -> m a
partialDiv = txtEl T.divClass

txtEl :: (Text.Text -> m a -> m a) -> Text.Text -> Text.Text -> m a -> m a
txtEl = defaultEl spaceJoin

-- | allows us to set a default value for tags by defining a join function
--   not a monoid because often it does it wrong, text needs a space
--    for example in case of classes, and the default map monoid is broken
defaultEl ::
     (arg -> arg -> arg) -> (arg -> m a -> m a) -> arg -> arg -> m a -> m a
defaultEl monoidF elF a b = elF $ monoidF a b

dynAttrEl ::
     Reflex t
  => (Dynamic t AttrMap -> m a -> m a)
  -> AttrMap
  -> Dynamic t AttrMap
  -> m a
  -> m a
dynAttrEl f = defaultEl ((<*>) . (<$>) attrUnion) f . constDyn
