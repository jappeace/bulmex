{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Odd tag helpers, these have uncommen html attributes
module Reflex.Bulmex.Tag.Odd
  (
  -- * Image
    image
  , imageClass
  -- * Link
  , ahref
  , ahref'
  , ahrefDyn
  -- * Space
  , textSpace
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Space
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Dom.Widget        as Dom
import qualified Reflex.Tags              as T

-- | kindoff hard to set an image tag in reflex
image :: Dom.DomBuilder t m => Text.Text -> m()
image url = T.imgAttr (Map.singleton "src" url) Dom.blank

-- | first class second src
imageClass :: Dom.DomBuilder t m => Text.Text -> Text.Text -> m()
imageClass clazz url = T.imgAttr (Map.fromList [("src",url), ("class", clazz)]) Dom.blank

-- | sometimes you just need 2 pieces of text to seperate with a space
textSpace :: Dom.DomBuilder t m => m ()
textSpace = Dom.text space

-- | a html tag that accepts any text into it's href value
ahref :: (Dom.DomBuilder t m, PostBuild t m) => Text.Text -> m a -> m a
ahref = ahref' mempty

ahref' :: (Dom.DomBuilder t m, PostBuild t m) => AttrMap -> Text.Text -> m a -> m a
ahref' uno = ahrefDyn (constDyn uno) . constDyn

ahrefDyn :: (Dom.DomBuilder t m, PostBuild t m) => Dynamic t AttrMap -> Dynamic t Text.Text -> m a -> m a
ahrefDyn uno txt = T.aDynAttr $ (attrUnion <$> uno) <*> (Map.singleton "href" <$> txt)
