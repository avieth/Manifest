{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Manifest.Manifest (

    Manifest(..)
  , ManifestRead(..)
  , ManifestWrite(..)
  , ManifestInjective(..)

  , Access(..)
  , AccessConstraint

  ) where

import GHC.Exts (Constraint)
import Data.Typeable
import Control.Exception
import Control.Monad.Trans.Except
import Manifest.ManifestException
import Manifest.Resource
import Manifest.FType

-- | Description of Manifest accessibility. Some Manifests are not
--   writeable.
data Access = ReadOnly | ReadWrite

-- | Association of a constraint to each Manifest, Access constructor pair:
--   ReadWrite demands that the Manifest is ManifestWrite.
type family AccessConstraint m (a :: Access) :: Constraint where
  AccessConstraint m ReadOnly = ()
  AccessConstraint m ReadWrite = ManifestWrite m

class Manifest (a :: FType -> Access -> * -> * -> *) where
  type ManifestResourceDescriptor a :: *
  resourceDescriptor :: a mtype access domain range -> ManifestResourceDescriptor a
  -- The actual "low-level" domain and range types can depend upon
  -- the "high-level" domain and range.
  type ManifestDomainType a domain range :: *
  type ManifestRangeType a domain range :: *
  type ManifestDomainConstraint a domain range :: Constraint
  type ManifestRangeConstraint a domain range :: Constraint
  mdomainDump
    :: ManifestDomainConstraint a domain range
    => a mtype access domain range
    -> domain
    -> ManifestDomainType a domain range
  mrangePull
    :: ManifestRangeConstraint a domain range
    => a mtype access domain range
    -> ManifestRangeType a domain range
    -> Maybe range

class Manifest a => ManifestRead a where
  mget
    :: (
       )
    => a mtype access domain range
    -> ResourceType (ManifestResourceDescriptor a)
    -> ManifestDomainType a domain range
    -> ExceptT SomeException IO (Maybe (ManifestRangeType a domain range))

class Manifest a => ManifestWrite a where
  mrangeDump
    :: ManifestRangeConstraint a domain range
    => a mtype access domain range
    -> range
    -> ManifestRangeType a domain range
  mset
    :: (
       )
    => a mtype ReadWrite domain range
    -> ResourceType (ManifestResourceDescriptor a)
    -> ManifestDomainType a domain range
    -> Maybe (ManifestRangeType a domain range)
    -> ExceptT SomeException IO ()

class Manifest a => ManifestInjective a where
  minvert 
    :: ( mtype ~ FInjective
       )
    => a mtype access domain range
    -> a mtype access range domain
