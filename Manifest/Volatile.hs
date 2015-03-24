{-|
Module      : Manifest.Volatile
Description : A mutable in-memory Manifest instance.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Manifest.Volatile (

    VolatileManifest
  , volatileManifest

  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Typeable
import Data.IORef
import qualified Data.Map as M
import Manifest.Manifest
import Manifest.Resource
import Manifest.FType

data VolatileDescriptor = VD
  deriving (Eq, Ord, Typeable)

instance ResourceDescriptor VolatileDescriptor where
  type ResourceType VolatileDescriptor = ()
  acquireResource VD = return $ Resource () commit rollback release
    where
      commit _ = return ()
      rollback _ = return ()
      release _ = return ()

data VolatileManifest ftype access domain range where
  VolatileManifest :: Ord domain => IORef (M.Map domain range) -> VolatileManifest FNotInjective ReadWrite domain range

volatileManifest :: Ord domain => IO (VolatileManifest FNotInjective ReadWrite domain range)
volatileManifest = do
    ioref <- newIORef M.empty
    return $ VolatileManifest ioref

instance Manifest VolatileManifest where
  type ManifestResourceDescriptor VolatileManifest = VolatileDescriptor
  resourceDescriptor _ = VD
  type ManifestDomainType VolatileManifest domain range = domain
  type ManifestRangeType VolatileManifest domain range = range
  type ManifestDomainConstraint VolatileManifest domain range = Ord domain
  type ManifestRangeConstraint VolatileManifest domain range = ()
  mdomainDump _ = id
  mrangePull _ = Just

instance ManifestRead VolatileManifest where
  mget (VolatileManifest ioref) _ x = do
      map <- liftIO $ readIORef ioref
      return $ M.lookup x map

instance ManifestWrite VolatileManifest where
  mrangeDump _ = id
  mset (VolatileManifest ioref) _ x y = liftIO $ atomicModifyIORef' ioref (\map ->
        (M.alter (const y) x map, ())
      )
