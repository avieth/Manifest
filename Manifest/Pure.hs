{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Manifest.Pure (

    PureManifest
  , pureFunction
  , pureInjection

  ) where

import Data.Typeable
import Manifest.Manifest
import Manifest.Resource
import Manifest.FType

data PureDescriptor = PD
  deriving (Eq, Ord, Typeable)

instance ResourceDescriptor PureDescriptor where
  type ResourceType PureDescriptor = ()
  acquireResource PD = return $ Resource () commit rollback release
    where
      commit _ = return ()
      rollback _ = return ()
      release _ = return ()

data PureManifest mtype access a b where
  PureManifestN :: (a -> Maybe b) -> PureManifest FNotInjective ReadOnly a b
  PureManifestI :: (a -> Maybe b) -> (b -> Maybe a) -> PureManifest FInjective ReadOnly a b

pureFunction :: (a -> Maybe b) -> PureManifest FNotInjective ReadOnly a b
pureFunction = PureManifestN

pureInjection :: (a -> Maybe b) -> (b -> Maybe a) -> PureManifest FInjective ReadOnly a b
pureInjection = PureManifestI

instance Manifest PureManifest where
  type ManifestResourceDescriptor PureManifest = PureDescriptor
  resourceDescriptor _ = PD
  type ManifestDomainType PureManifest domain range = domain
  type ManifestRangeType PureManifest domain range = range
  type ManifestDomainConstraint PureManifest domain range = ()
  type ManifestRangeConstraint PureManifest domain range = ()
  mdomainDump = const id
  mrangePull = const Just

instance ManifestRead PureManifest where
  mget pm () x = case pm of
    PureManifestN f -> return $ f x
    PureManifestI f _ -> return $ f x

instance ManifestInjective PureManifest where
  minvert pm = case pm of
    PureManifestI f g -> PureManifestI g f
