{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | A root for an app,
--   usefull for server side html rendering.
--   has a neat api for the head tag, use this if that api your needs.
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
  ) where

import           Control.Lens
import           Control.Monad                (void)
import           Data.Foldable                (traverse_)
import           Data.Generics.Product.Fields
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as Text
import           GHC.Generics                 (Generic)
import           Network.URI
import qualified Reflex.Dom.Builder.Class     as Dom
import qualified Reflex.Dom.Widget            as Dom

-- | Adds the core html tags.
--   we already know most of the head.
htmlWidget :: (Dom.DomBuilder t m) => HeadSettings -> m a -> m a
htmlWidget settings content =
  Dom.el "html" $ do
    void $ Dom.el "head" $ do
      headWidget settings
    Dom.el "body" $ content

defSettings :: HeadSettings
defSettings = HeadSettings{
    _head_js    = mempty
  , _head_css   = mempty
  , _head_title = mempty
  }

data HeadSettings = HeadSettings
  { _head_js    :: [HeadScript]
  , _head_css   :: [URI]
  , _head_title :: Text.Text
  } deriving (Generic, Show)

head_js :: Lens' HeadSettings [HeadScript]
head_js = field @"_head_js"

head_css :: Lens' HeadSettings [URI]
head_css = field @"_head_css"

head_title :: Lens' HeadSettings Text.Text
head_title = field @"_head_title"

data HeadScript = HeadScript
  { _script_is_async :: Bool
  , _script_uri      :: URI
  } deriving (Generic, Show)

defScript :: HeadScript
defScript = HeadScript {
    _script_is_async = True
  , _script_uri = nullURI
  }

script_uri :: Lens' HeadScript URI
script_uri = field @"_script_uri"

script_is_async :: Lens' HeadScript Bool
script_is_async = field @"_script_is_async"

isasync :: (Text.Text, Text.Text)
isasync = ("async","true")

scriptToMap :: HeadScript -> Map.Map Text.Text Text.Text
scriptToMap script = Map.fromList $
    if (script ^. script_is_async) then [isasync, src] else [src]
  where
    src = ("src", script ^. script_uri . to (Text.pack . show))

-- | Try to keep the head as small as possible.
--   Only things that are required initially should be placed in the head.
--   so the pattern is that we require a bunch of different components
--   initially but we put them in different files.
--   for example we needed the bulma css file for most styling
--   and balloon css for just tooltips.
headWidget :: Dom.DomBuilder t m => HeadSettings ->  m ()
headWidget settings = do
  traverse_ (\attrlist -> Dom.elAttr "script" attrlist Dom.blank) $ settings ^.. head_js .  traversed . to scriptToMap
  -- google complaints about viewport, but it breaks the table
  -- metaAttr (Map.fromList [("name", "viewport"), ("content", "device-width, initial-scale=1")]) Dom.blank
  void $ Dom.el "title" $ Dom.text $ settings ^. head_title
  traverse_
    (\href ->
       Dom.elAttr "link"
         (Map.fromList -- bulmo
            [("rel", "stylesheet"), ("href", href)])
         Dom.blank) $ settings ^.. head_css . traversed . to (Text.pack . show)
