{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Odd tag helpers, these have uncommen html attributes
--   They don't fit in the 'normal' pattern of Abstract
module Reflex.Bulmex.Tag.Odd
  ( image
  , imageClass
  -- * Link
  , ahref
  , ahref'
  , ahrefDyn
  -- * Space
  , textSpace
  -- * Prevent browser default
  , elDynAttrPrevDef
  , elDynAttrModConf
  ) where

import           Control.Lens             ((%~), (.~))
import           Data.Map                 (Map)
import qualified Data.Map.Strict          as Map
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Space
import qualified Reflex.Dom.Builder.Class as Dom
import           Reflex.Dom.Core
import qualified Reflex.Dom.Widget        as Dom
import qualified Reflex.Tags              as T

-- | kindoff hard to set an image tag in reflex
image :: Dom.DomBuilder t m => Text.Text -> m ()
image url = T.imgAttr (Map.singleton "src" url) Dom.blank

-- | first class second src
imageClass :: Dom.DomBuilder t m => Text.Text -> Text.Text -> m ()
imageClass clazz url =
  T.imgAttr (Map.fromList [("src", url), ("class", clazz)]) Dom.blank

-- | sometimes you just need 2 pieces of text to seperate with a space
textSpace :: Dom.DomBuilder t m => m ()
textSpace = Dom.text space

-- | a html tag that accepts any text into it's href value
ahref :: (Dom.DomBuilder t m, PostBuild t m) => Text.Text -> m a -> m a
ahref = ahref' mempty

ahref' ::
     (Dom.DomBuilder t m, PostBuild t m) => AttrMap -> Text.Text -> m a -> m a
ahref' uno = ahrefDyn (constDyn uno) . constDyn

ahrefDyn ::
     (Dom.DomBuilder t m, PostBuild t m)
  => Dynamic t AttrMap
  -> Dynamic t Text.Text
  -> m a
  -> m a
ahrefDyn uno txt =
  T.aDynAttr $ (attrUnion <$> uno) <*> (Map.singleton "href" <$> txt)

-- | From https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7
--   Like 'elDynAttr'' but configures "prevent default" on the given event.
--   Blocks for example a context menu from poping up on right mouse click.
--   This should be used with caution, as it may be unexpected for end user.
elDynAttrPrevDef ::
     forall a en m t. (DomBuilder t m, PostBuild t m)
  => EventName en -- ^ Event on the element to configure with 'preventDefault'
  -> Text -- ^ Element tag
  -> Dynamic t (Map Text Text) -- ^ Element attributes
  -> m a -- ^ Child of element
  -> m (Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrPrevDef ev =
  elDynAttrModConf
    (\elCfg ->
       elCfg &
       elementConfig_eventSpec %~
       addEventSpecFlags
         (Proxy :: Proxy (DomBuilderSpace m))
         ev
         (const preventDefault))

-- | Like 'elDynAttr'' but allows you to modify the element configuration.
--
-- Special thanks to @luigy: https://gist.github.com/luigy/b49ce04de8462e594c9c2b5b455ae5a5#file-foo-hs
elDynAttrModConf ::
     (DomBuilder t m, PostBuild t m)
  => (ElementConfig EventResult t (DomBuilderSpace m) -> ElementConfig EventResult t (DomBuilderSpace m))
  -> Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrModConf f elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg =
        def & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- element elementTag (f cfg) child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result
