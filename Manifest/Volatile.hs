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
  , volatileManifestInjective

  ) where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable
import Data.IORef
import qualified Data.Map as M
import Manifest.Manifest
import Manifest.ManifestException
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

-- | TODO I don't believe this is thread safe. Perhaps throw an MVar in here
--   and take it as acquireResource? Yeah, VolatileManifest can be its
--   own descriptor, since each one has its own resource! Makes perfect sense.
--   Aha but we need the Ord instace :( 
--   For non-volatile instances like an RDBMS our resource will certainly
--   be Ord/Eq but maybe that's too strict... I suppose we could shim something
--   in here: each VolatileManifest carries a UUID. However, I'm thinking
--   perhaps we should focus on doing away with the Ord constraint.
--   It's there only because we want to be able to dump these things into a
--   search tree. We could use the Typeable for Ord and at each element in
--   that search tree, place a homogeneous list where the type is Eq-able.
data VolatileManifest :: FType -> Access -> * -> * -> * where
  VolatileManifestN
    :: ( Ord domain
       )
    => IORef (M.Map domain range)
    -> VolatileManifest FNotInjective access domain range
  VolatileManifestI
    :: ( Ord domain
       , Ord range
       )
    => IORef (M.Map domain range)
    -> IORef (M.Map range domain)
    -> VolatileManifest FInjective access domain range

volatileManifest :: Ord domain => IO (VolatileManifest FNotInjective access domain range)
volatileManifest = do
    ioref <- newIORef M.empty
    return $ VolatileManifestN ioref

volatileManifestInjective
  :: ( Ord domain
     , Ord range
     )
  => IO (VolatileManifest FInjective access domain range)
volatileManifestInjective = do
    iorefForward <- newIORef M.empty
    iorefBackward <- newIORef M.empty
    return $ VolatileManifestI iorefForward iorefBackward

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
  mget (VolatileManifestN ioref) _ x = do
      map <- liftIO $ readIORef ioref
      return $ M.lookup x map
  mget (VolatileManifestI iorefForward _) _ x = do
      map <- liftIO $ readIORef iorefForward
      return $ M.lookup x map

instance ManifestWrite VolatileManifest where
  mrangeDump _ = id
  mset (VolatileManifestN ioref) _ x y = liftIO $ atomicModifyIORef' ioref (\map ->
        (M.alter (const y) x map, ())
      )
  mset (VolatileManifestI iorefForward iorefBackward) _ x y = do
      mapF <- liftIO $ readIORef iorefForward
      mapB <- liftIO $ readIORef iorefBackward
      let oldImage = M.lookup x mapF
      let newMapF = M.alter (const y) x mapF
      let mapB' = case oldImage of
                    Just oldImage' -> M.delete oldImage' mapB
                    Nothing -> mapB
      newMapB <- case y of
                   Just y' -> case M.lookup y' mapB of
                                Just x' -> if x' == x then return mapB' else mthrow ManifestInjectivityViolation
                                Nothing -> return $ M.alter (const (Just x)) y' mapB'
                   Nothing -> return mapB'
      liftIO $ atomicModifyIORef' iorefForward (\_ -> (newMapF, ()))
      liftIO $ atomicModifyIORef' iorefBackward (\_ -> (newMapB, ()))

instance ManifestInjective VolatileManifest where
  minvert (VolatileManifestI iorefForward iorefBackward) = VolatileManifestI iorefBackward iorefForward
