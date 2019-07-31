{-# LANGUAGE OverloadedStrings #-}

-- | Load functions
module Reflex.Bulmex.Load where

import           Control.Monad                        (join)
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Map                             as Map
import           Data.Maybe
import qualified Data.Text                            as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy                       as LText
import           JSDOM
import           JSDOM.Generated.Element
import           JSDOM.Generated.NonElementParentNode
import           Reflex
import qualified Reflex.Dom.Builder.Class             as Dom
import qualified Reflex.Dom.Prerender                 as Dom
import qualified Reflex.Dom.Widget.Basic              as Dom

-- | Insert an encodable in the document body,
--   in case of the server side rendering we encode it as script tag with jsonval,
--   in case of ghcjsdom we read the value from that script tag
--   first arg is the idname to connect the two up (has to be uniq for a doc)
preloadState :: (FromJSON a, ToJSON a, Dom.DomBuilder t m, Dom.Prerender js t m) =>
  Text.Text -> a -> m (Dynamic t a)
preloadState comelid serverState =
  Dom.prerender (do
    Dom.elAttr "script" (Map.fromList [
                            ("type", "application/json"),
                            ("id", comelid)
                            ]) $
      Dom.text $ LText.toStrict $ encodeToLazyText serverState
    pure serverState
    ) $ do
        mayDoc <- currentDocument
        mayEl' <- sequence $ (getElementById <$> mayDoc) <*> pure comelid
        mayInner  <- sequence $ getInnerHTML <$> join mayEl'
        let result = (join $ decode . LBS.fromStrict .  encodeUtf8 <$> mayInner)
        pure $ fromMaybe serverState  -- TODO don't fail silently
            result
