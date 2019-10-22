{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

-- | Types for Form.
module Reflex.Bulmex.Form.FormTypes(
  FormAction(..), SpinState(..)
-- * Prisms
  , _PostDefault
  , _Loading
  , _FormRest
                                   ) where

import           Control.Lens
import           Data.Generics.Sum
import           GHC.Generics      (Generic)

data FormAction
  = PostDefault -- ^ do the post
  | Loading -- ^ trigger spin state without doing default the post
  | FormRest -- ^ stop the spin state
  deriving Generic

_PostDefault :: Prism' FormAction ()
_PostDefault = _Ctor @"PostDefault"

_Loading :: Prism' FormAction ()
_Loading = _Ctor @"Loading"

_FormRest :: Prism' FormAction ()
_FormRest = _Ctor @"FormRest"

data SpinState
  = SpinRest
  | Spinning
  deriving (Show)

instance Semigroup SpinState where
  (<>) SpinRest SpinRest = SpinRest
  (<>) _ _               = Spinning

instance Monoid SpinState where
  mempty = SpinRest
