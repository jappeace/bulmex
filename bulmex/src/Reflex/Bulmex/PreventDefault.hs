{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ripped from https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7
module Reflex.Bulmex.PreventDefault where

import           Control.Lens    ((%~), (.~))
import           Data.Map        (Map)
import           Data.Proxy      (Proxy (..))
import           Data.Text       (Text)
import           Reflex.Dom.Core

-- | Like 'elDynAttr'' but configures "prevent default" on the given event.
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
