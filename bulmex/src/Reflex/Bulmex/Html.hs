{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | A root for an app,
--   usefull for server side html rendering.
--   has a neat api for the head tag, use this if that api suits your needs.
--   For more info see blogpost: https://jappieklooster.nl/reflex-server-side-html-rendering.html
module Reflex.Bulmex.Html
  ( htmlWidget
  -- * Head tag stuff
  , HeadSettings(..)
  , head_js
  , head_css
  , head_title
  , HeadScript(..)
  , script_uri
  , script_is_async
  -- * Defaults
  , defScript
  , defSettings
  -- * Load
  , writeReadDom
  ) where

import           Control.Lens
import           Control.Monad                        (void)
import           Control.Monad                        (join)
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Foldable                        (traverse_)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe
import qualified Data.Text                            as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy                       as LText
import           GHC.Generics                         (Generic)
import           JSDOM
import           JSDOM.Generated.Element
import           JSDOM.Generated.NonElementParentNode
import           Network.URI
import           Reflex
import qualified Reflex.Dom.Builder.Class             as Dom
import qualified Reflex.Dom.Prerender                 as Dom
import qualified Reflex.Dom.Widget                    as Dom

-- | Adds the core html tags.
--   we already know most of the head.
--
-- >  <html>
-- >    <head>
-- >     'HeadSettings' <!-- provided settings --!>
-- >    </head>
-- >    <body>
-- >    'm a' <!-- provided monad --!>
-- >    </body>
-- >  </html>
htmlWidget :: (Dom.DomBuilder t m) => HeadSettings -> m a -> m a
htmlWidget settings content =
  Dom.el "html" $ do
    void $ Dom.el "head" $ do headWidget settings
    Dom.el "body" $ content

defSettings :: HeadSettings
defSettings =
  HeadSettings {_head_js = mempty, _head_css = mempty, _head_title = mempty}

data HeadSettings = HeadSettings
  { _head_js    :: [HeadScript]
  , _head_css   :: [URI]
  , _head_title :: Text.Text
  } deriving (Generic, Show)

head_js :: Lens' HeadSettings [HeadScript]
head_js = lens _head_js $ \x b -> x {_head_js = b}

head_css :: Lens' HeadSettings [URI]
head_css = lens _head_css $ \x b -> x {_head_css = b}

head_title :: Lens' HeadSettings Text.Text
head_title = lens _head_title $ \x b -> x {_head_title = b}

data HeadScript = HeadScript
  { _script_is_async :: Bool
  , _script_uri      :: URI
  } deriving (Generic, Show)

defScript :: HeadScript
defScript = HeadScript {_script_is_async = True, _script_uri = nullURI}

script_uri :: Lens' HeadScript URI
script_uri = lens _script_uri $ \x b -> x {_script_uri = b}

script_is_async :: Lens' HeadScript Bool
script_is_async = lens _script_is_async $ \x b -> x {_script_is_async = b}

isasync :: (Text.Text, Text.Text)
isasync = ("async", "true")

scriptToMap :: HeadScript -> Map.Map Text.Text Text.Text
scriptToMap script =
  Map.fromList $
  if (script ^. script_is_async)
    then [isasync, src]
    else [src]
  where
    src = ("src", script ^. script_uri . to (Text.pack . show))

-- | Try to keep the head as small as possible.
--   Only things that are required initially should be placed in the head.
--   so the pattern is that we require a bunch of different components
--   initially but we put them in different files.
--   for example we needed the bulma css file for most styling
--   and balloon css for just tooltips.
headWidget :: Dom.DomBuilder t m => HeadSettings -> m ()
headWidget settings = do
  traverse_ (\attrlist -> Dom.elAttr "script" attrlist Dom.blank) $ settings ^..
    head_js .
    traversed .
    to scriptToMap
  -- google complaints about viewport, but it breaks the table
  -- metaAttr (Map.fromList [("name", "viewport"), ("content", "device-width, initial-scale=1")]) Dom.blank
  void $ Dom.el "title" $ Dom.text $ settings ^. head_title
  traverse_
    (\href ->
       Dom.elAttr
         "link"
         (Map.fromList -- bulmo
            [("rel", "stylesheet"), ("href", href)])
         Dom.blank) $
    settings ^..
    head_css .
    traversed .
    to (Text.pack . show)

-- TODO Move to Load
-- | Insert an encodable in the document body,
--   in case of the server side rendering we encode it as script tag with jsonval,
--   in case of ghcjsdom we read the value from that script tag
--   first arg is the idname to connect the two up (has to be uniq for a doc)
writeReadDom ::
     (FromJSON a, ToJSON a, Dom.DomBuilder t m, Dom.Prerender js t m)
  => Text.Text
  -> a
  -> m (Dynamic t a)
writeReadDom comelid serverState =
  Dom.prerender
    (do Dom.elAttr
          "script"
          (Map.fromList [("type", "application/json"), ("id", comelid)]) $
          Dom.text $
          LText.toStrict $
          encodeToLazyText serverState
        pure serverState) $ do
    mayDoc <- currentDocument
    mayEl' <- sequence $ (getElementById <$> mayDoc) <*> pure comelid
    mayInner <- sequence $ getInnerHTML <$> join mayEl'
    let result = (join $ decode . LBS.fromStrict . encodeUtf8 <$> mayInner)
    pure $
      fromMaybe
        serverState -- TODO don't fail silently
        result
