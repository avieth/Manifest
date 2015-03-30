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

-- | A pure Manifest, parameterized by an arbitrary monad.
data PureManifest :: (* -> *) -> Access -> * -> * -> * where
  PureManifest :: (a -> m b) -> PureManifest m ReadOnly a b

pureManifest :: (a -> m b) -> PureManifest m ReadOnly a b
pureManifest = PureManifest

instance Monad m => Manifest (PureManifest m) where
  type ManifestResourceDescriptor (PureManifest m) access domain range = PureDescriptor
  resourceDescriptor _ = PD
  type ManifestDomainType (PureManifest m) domain range = domain
  type ManifestRangeType (PureManifest m) domain range = range
  type ManifestDomainConstraint (PureManifest m) domain range = ()
  type ManifestRangeConstraint (PureManifest m) domain range = ()
  type ManifestFunctor (PureManifest m) domain range = m
  mdomainDump = const id
  mrangePull = const return

instance Monad m => ManifestRead (PureManifest m) where
  mget pm () x = case pm of
    PureManifest f -> return . f $ x
