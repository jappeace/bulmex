{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

-- | Types for Form.
module Reflex.Bulmex.Form.FormTypes
  ( FormAction(..)
  , FormState(..)
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
  deriving (Generic)

_PostDefault :: Prism' FormAction ()
_PostDefault = _Ctor @"PostDefault"

_Loading :: Prism' FormAction ()
_Loading = _Ctor @"Loading"

_FormRest :: Prism' FormAction ()
_FormRest = _Ctor @"FormRest"

data FormState
  = FormStateRest
  | FormStateSpinning
  deriving (Show)

instance Semigroup FormState where
  (<>) FormStateRest FormStateRest = FormStateRest
  (<>) _ _                         = FormStateSpinning

instance Monoid FormState where
  mempty = FormStateRest
