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
{-# LANGUAGE StandaloneDeriving #-}

module Manifest.Volatile (

    VolatileManifest
  , volatileManifest

  ) where

--import Control.Monad.Trans.Except (throwE)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import Data.IORef
import Data.Typeable
import Manifest.Manifest
import Manifest.ManifestException
import Manifest.Resource
import Manifest.FType

data VolatileManifestResource domain range where
  VMR
    :: IORef (M.Map domain range)
    -- ^ The map at acquisition time.
    -> IORef (M.Map domain range)
    -- ^ staged changes. No TVar or MVar necessary. The f-term has a definite
    --   ordering of writes; the strategy must guarantee they are done
    --   in-order.
    -> VolatileManifestResource domain range

vmrWrite :: Ord domain => VolatileManifestResource domain range -> domain -> Maybe range -> IO ()
vmrWrite (VMR _ staged) x y = modifyIORef staged (M.alter (const y) x)

vmrRead :: Ord domain => VolatileManifestResource domain range -> domain -> IO (Maybe range)
vmrRead (VMR original staged) x = do
    originalEntry <- readIORef original >>= return . M.lookup x
    cachedWrite <- readIORef staged >>= return . M.lookup x
    return $ cachedWrite <|> originalEntry

data VolatileManifest :: FType -> Access -> * -> * -> * where
  VolatileManifestN
    :: ( Ord domain
       )
    => TVar (M.Map domain range)
    -> VolatileManifest FNotInjective access domain range

deriving instance Typeable VolatileManifest
deriving instance Eq (VolatileManifest ftype access domain range)

volatileManifest :: Ord domain => IO (VolatileManifest FNotInjective access domain range)
volatileManifest = newTVarIO M.empty >>= return . VolatileManifestN

-- | VolatileManifest is its own resource descriptor. Makes sense, since two
--   VolatileManifests can share a resource only if they are the same
--   VolatileManifest (same TVar).
instance
    ( Ord domain
    , Eq range
    , Typeable domain
    , Typeable range
    , Typeable ftype
    , Typeable access
    )
    => ResourceDescriptor (VolatileManifest ftype access domain range) where
  type ResourceType (VolatileManifest ftype access domain range) = VolatileManifestResource domain range
  acquireResource (VolatileManifestN tvar) = do
      original <- readTVarIO tvar
      stage <- liftIO $ newIORef M.empty
      let commitEffect = \(VMR stage) -> do
                staged <- readIORef stage
                atomically $ modifyTVar tvar (\map -> M.union staged map)
                -- ^ Here we union with whatever is in the TVar. This
                --   may be no good, as other threads may have updated the
                --   contents of the TVar in the time since we acquired the
                --   resource. But then, overwriting it with the original map
                --   at acquisition time, unioned with staged changes, seems
                --   wrong as well, as this would destroy changes made in
                --   other threads!
                --   STM can help us, but only if we do everything from
                --   acquisition to commit in STM! Is this feasible? Having
                --   a different monad for each Manifest? Don't think so, as
                --   we would have to escape to a common monad at some point:
                --   we mget from the VolatileManifest and try to use its
                --   value in some other manifest; if the other manifest
                --   uses IO, then we're out of luck.
                --
                --   We could ditch STM and just do manual locking: only one
                --   thread can use a VolatileManifest at any given time. Or
                --   we could manually roll out optimistic locking.
                --
                --   Whatever we choose, the goal should be clearly remembered:
                --   the user of Manifest does not have to worry about
                --   concurrency problems. He always gets
                --     Atomicity
                --     Isolation
                --   consistency and durability are only applicable to
                --   database-backed manifests. The volatile manifest can
                --   by definition (nature of Haskell) never enter an
                --   inconsistent state, and since it's volatile, durability
                --   doesn't make any sense. 
                --
      let rollbackEffect = \_ -> return ()
      let releaseEffect = \_ -> return ()
      return $ Resource (VMR original stage) commitEffect rollbackEffect releaseEffect

instance Manifest VolatileManifest where
  type ManifestResourceDescriptor VolatileManifest ftype access domain range = VolatileManifest ftype access domain range
  resourceDescriptor = id
  type ManifestDomainType VolatileManifest domain range = domain
  type ManifestRangeType VolatileManifest domain range = range
  type ManifestDomainConstraint VolatileManifest domain range = Ord domain
  type ManifestRangeConstraint VolatileManifest domain range = ()
  mdomainDump _ = id
  mrangePull _ = Just

instance ManifestRead VolatileManifest where
  mget (VolatileManifestN tvar) vr x = do
      cachedWrite <- vmrRead vr x of
      return $ cachedWrite <|> readFromTVar
      map <- liftIO $ readIORef ioref
      return $ M.lookup x map

{-
instance ManifestWrite VolatileManifest where
  mrangeDump _ = id
  mset (VolatileManifestN tvar) vr x = do
      
  mset (VolatileManifestN ioref) _ x y = liftIO $ atomicModifyIORef' ioref (\map ->
        (M.alter (const y) x map, ())
      )
-}

--instance ManifestInjective VolatileManifest where
--  minvert (VolatileManifestI iorefForward iorefBackward) = VolatileManifestI iorefBackward iorefForward
