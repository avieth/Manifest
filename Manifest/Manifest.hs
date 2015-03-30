{-|
Module      : Manifest.Manifest
Description : Definition of Manifest typeclasses
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

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

  , Access(..)
  , AccessConstraint

  ) where

import GHC.Exts (Constraint)
import Data.Typeable
import Control.Exception
import Control.Monad.Trans.Except
import Manifest.ManifestException
import Manifest.Resource

-- | Description of Manifest accessibility. Some Manifests are not
--   writeable.
data Access = ReadOnly | ReadWrite
  deriving (Eq, Typeable)

-- | Association of a constraint to each Manifest, Access constructor pair:
--   ReadWrite demands that the Manifest is ManifestWrite.
type family AccessConstraint m (a :: Access) :: Constraint where
  AccessConstraint m ReadOnly = ()
  AccessConstraint m ReadWrite = ManifestWrite m

class Manifest (a :: Access -> * -> * -> *) where
  type ManifestResourceDescriptor a (access :: Access) domain range :: *
  resourceDescriptor :: a access domain range -> ManifestResourceDescriptor a access domain range
  type ManifestDomainType a domain range :: *
  -- ^ The low-level domain type.
  type ManifestRangeType a domain range :: *
  -- ^ The low-level range type.
  type ManifestDomainConstraint a domain range :: Constraint
  type ManifestRangeConstraint a domain range :: Constraint
  type ManifestFunctor a domain range :: * -> *
  mdomainDump
    :: ManifestDomainConstraint a domain range
    => a access domain range
    -> domain
    -> ManifestDomainType a domain range
  mrangePull
    :: ManifestRangeConstraint a domain range
    => a access domain range
    -> ManifestRangeType a domain range
    -> (ManifestFunctor a domain range) range

class Manifest a => ManifestRead a where
  mget
    :: (
       )
    => a access domain range
    -> ResourceType (ManifestResourceDescriptor a access domain range)
    -> ManifestDomainType a domain range
    -> IO ((ManifestFunctor a domain range) (ManifestRangeType a domain range))

class Manifest a => ManifestWrite a where
  mrangeDump
    :: ManifestRangeConstraint a domain range
    => a access domain range
    -> range
    -> ManifestRangeType a domain range
  mset
    :: (
       )
    => a ReadWrite domain range
    -> ResourceType (ManifestResourceDescriptor a ReadWrite domain range)
    -> ManifestDomainType a domain range
    -> (ManifestFunctor a domain range) (ManifestRangeType a domain range)
    -> IO ()
