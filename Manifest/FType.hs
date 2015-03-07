{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Manifest.FType (

    FType(..)
  , FTypeMeet

  ) where

data FType = FInjective | FNotInjective

type family FTypeMeet (m1 :: FType) (m2 :: FType) :: FType where
  FTypeMeet FInjective FInjective = FInjective
  FTypeMeet FNotInjective FInjective = FNotInjective
  FTypeMeet FInjective FNotInjective = FNotInjective
  FTypeMeet FNotInjective FNotInjective = FNotInjective
