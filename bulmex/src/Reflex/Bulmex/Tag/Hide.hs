{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | A div that can hide with bulma is-hidden class
module Reflex.Bulmex.Tag.Hide where

import           Control.Applicative
import           Data.Bool
import qualified Data.Map.Strict          as Map
import           Reflex
import           Reflex.Bulmex.Attr
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Tags              as T

-- | When the dynamic is true the first monad is shown, otherwise the second.
switchDiv ::
     (PostBuild t m, Dom.DomBuilder t m) => Dynamic t Bool -> m () -> m a -> m a
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
