{-|
Module      : Manifest.Pure
Description : An immutable in-memory Manifest instance.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Manifest.Pure (

    PureManifest
  , pureManifest

  ) where

import Data.Typeable
import Data.Functor.Identity
import Manifest.Manifest
import Manifest.Resource

-- | This resource descriptor contains no information, because a PureManifest
--   doesn't need any information: it's immutable, so committing and rolling
--   back is trivial.
data PureDescriptor = PD
  deriving (Eq, Typeable)

instance ResourceDescriptor PureDescriptor where
  type ResourceType PureDescriptor = ()
  acquireResource PD = return $ Resource () commit rollback release
    where
      commit _ = return ()
      rollback _ = return ()
      release _ = return ()

-- | A pure partial function manifest.
data PureManifest :: Access -> * -> * -> * where
  PureManifest :: (a -> b) -> PureManifest ReadOnly a b

pureManifest :: (a -> b) -> PureManifest ReadOnly a b
pureManifest = PureManifest

instance Manifest PureManifest where
  type ManifestResourceDescriptor PureManifest access domain range = PureDescriptor
  resourceDescriptor _ = PD
  type ManifestDomainType PureManifest domain range = domain
  type ManifestRangeType PureManifest domain range = range
  type ManifestDomainConstraint PureManifest domain range = ()
  type ManifestRangeConstraint PureManifest domain range = ()
  type ManifestFunctor PureManifest domain range = Identity
  mdomainDump = const id
  mrangePull = const Identity

instance ManifestRead PureManifest where
  mget pm () x = case pm of
    PureManifest f -> return . Identity . f $ x
