{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This is just a copy of the upstream code except polymorphic:
--   Get rid of m ~ Ghcjsdomspace to allow these widgets to be prerendered
module Reflex.Bulmex.Input.Polymorphic
  ( textInput
  , TextInput(..)
  , textArea
  , TextArea(..)
  , textArea_value
  , textArea_keypress
  , textInput_value
  , textInput_keyup
  , textInput_keypress
  , textInput_keydown
  , textInput_input
  , textInput_hasFocus
  ) where

import           Control.Lens
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import           Reflex
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Widget.Basic
import qualified Reflex.Dom.Widget.Input  as Inp

textInput ::
     (DomBuilder t m, PostBuild t m) => Inp.TextInputConfig t -> m (TextInput t)
textInput (Inp.TextInputConfig inputType initial eSetValue dAttrs) = do
  modifyAttrs <-
    dynamicAttributesToModifyAttributes $
    fmap (Map.insert "type" inputType) dAttrs
  i <-
    inputElement $ Inp.def & inputElementConfig_initialValue .~ initial &
    inputElementConfig_setValue .~
    eSetValue &
    inputElementConfig_elementConfig .
    elementConfig_modifyAttributes .~
    fmap mapKeysToAttributeName modifyAttrs
  return $
    TextInput
      { _textInput_value = _inputElement_value i
      , _textInput_input = _inputElement_input i
      , _textInput_keypress = domEvent Keypress i
      , _textInput_keydown = domEvent Keydown i
      , _textInput_keyup = domEvent Keyup i
      , _textInput_hasFocus = _inputElement_hasFocus i
      }

data TextInput t = TextInput
  { _textInput_value    :: Dynamic t Text.Text
  , _textInput_input    :: Event t Text.Text
  , _textInput_keypress :: Event t Word
  , _textInput_keydown  :: Event t Word
  , _textInput_keyup    :: Event t Word
  , _textInput_hasFocus :: Dynamic t Bool
  }

instance Inp.HasValue (TextInput t) where
  type Value (TextInput t) = Dynamic t Text.Text
  value = _textInput_value

textArea ::
     (DomBuilder t m, PostBuild t m) => Inp.TextAreaConfig t -> m (TextArea t)
textArea (Inp.TextAreaConfig initial eSet attrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  i <-
    textAreaElement $ Inp.def & textAreaElementConfig_initialValue .~ initial &
    textAreaElementConfig_setValue .~
    eSet &
    textAreaElementConfig_elementConfig .
    elementConfig_modifyAttributes .~
    fmap mapKeysToAttributeName modifyAttrs
  return $
    TextArea
      { _textArea_value = _textAreaElement_value i
      , _textArea_input = _textAreaElement_input i
      , _textArea_keypress = domEvent Keypress i
      , _textArea_hasFocus = _textAreaElement_hasFocus i
      }

data TextArea t = TextArea
  { _textArea_value    :: Dynamic t Text.Text
  , _textArea_input    :: Event t Text.Text
  , _textArea_hasFocus :: Dynamic t Bool
  , _textArea_keypress :: Event t Word
  }

instance Inp.HasValue (TextArea t) where
  type Value (TextArea t) = Dynamic t Text.Text
  value = _textArea_value

textArea_keypress :: Lens' (TextArea t) (Event t Word)
textArea_keypress f (TextArea x1 x2 x3 x4) =
  (\y -> TextArea x1 x2 x3 y) <$> f x4

textArea_value :: Lens' (TextArea t) (Dynamic t Text.Text)
textArea_value f (TextArea x1 x2 x3 x4) = (\y -> TextArea y x2 x3 x4) <$> f x1

textInput_hasFocus :: Lens' (TextInput t) (Dynamic t Bool)
textInput_hasFocus f (TextInput x1 x2 x3 x4 x5 x6) =
  (\y -> TextInput x1 x2 x3 x4 x5 y) <$> f x6

textInput_input :: Lens' (TextInput t) (Event t Text.Text)
textInput_input f (TextInput x1 x2 x3 x4 x5 x6) =
  (\y -> TextInput x1 y x3 x4 x5 x6) <$> f x2

textInput_keydown :: Lens' (TextInput t) (Event t Word)
textInput_keydown f (TextInput x1 x2 x3 x4 x5 x6) =
  (\y -> TextInput x1 x2 x3 y x5 x6) <$> f x4

textInput_keypress :: Lens' (TextInput t) (Event t Word)
textInput_keypress f (TextInput x1 x2 x3 x4 x5 x6) =
  (\y -> TextInput x1 x2 y x4 x5 x6) <$> f x3

textInput_keyup :: Lens' (TextInput t) (Event t Word)
textInput_keyup f (TextInput x1 x2 x3 x4 x5 x6) =
  (\y -> TextInput x1 x2 x3 x4 y x6) <$> f x5

textInput_value :: Lens' (TextInput t) (Dynamic t Text.Text)
textInput_value f (TextInput x1 x2 x3 x4 x5 x6) =
  (\y -> TextInput y x2 x3 x4 x5 x6) <$> f x1
