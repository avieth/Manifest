{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Manifest.FType (

    FType(..)
  , FTypeMeet

  ) where

import Data.Typeable

-- | Description of a function: either injective or not injective.
data FType = FInjective | FNotInjective
  deriving (Eq, Typeable)

-- | Expresses the fact that the composition f . g is injective iff both
--   f and g are injective.
type family FTypeMeet (m1 :: FType) (m2 :: FType) :: FType where
  FTypeMeet FInjective FInjective = FInjective
  FTypeMeet FNotInjective FInjective = FNotInjective
  FTypeMeet FInjective FNotInjective = FNotInjective
  FTypeMeet FNotInjective FNotInjective = FNotInjective
