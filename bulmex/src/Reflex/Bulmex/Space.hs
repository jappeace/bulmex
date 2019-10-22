{-# LANGUAGE OverloadedStrings #-}

-- | Sometimes you just need some space in your life
module Reflex.Bulmex.Space
  ( space
  , spaceJoin
  ) where

import qualified Data.Text as Text

space :: Text.Text
space = " "

spaceJoin :: Text.Text -> Text.Text -> Text.Text
spaceJoin a b = a <> space <> b
