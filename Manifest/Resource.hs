{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Manifest.Resource (

    Resource(..)
  , ResourceDescriptor(..)

  , ResourceAcquisitionException(..)

  , resource
  , commit
  , rollback
  , release

  , releaseAll
  , rollbackAll
  , commitAll

  , RollbackEffect
  , CommitEffect
  , ReleaseEffect

  , DResourceMap
  , DResourceKey

  ) where

import qualified Data.DependentMap as DM
import Control.Monad.Trans.Except
import Control.Exception
import Data.Functor.Identity
import Data.Typeable
import Manifest.ManifestException

type RollbackEffect r = r -> IO ()
type CommitEffect r = r -> IO ()
type ReleaseEffect r = r -> IO ()

-- | TBD Should we demand commit and rollback for every resource? Some
--   resources are read-only, in which case we wouldn't need these, no?
data Resource r where
  Resource
    :: r
    -> CommitEffect r
    -> RollbackEffect r
    -> ReleaseEffect r
    -> Resource r

resource :: Resource r -> r
resource (Resource r _ _ _) = r

commit :: Resource r -> IO ()
commit (Resource r c _ _) = c r

rollback :: Resource r -> IO ()
rollback (Resource r _ rb _) = rb r

release :: Resource r -> IO ()
release (Resource r _ _ rel) = rel r

data ResourceAcquisitionException where
  ResourceAcquisitionException :: ResourceAcquisitionException

deriving instance Typeable ResourceAcquisitionException
deriving instance Show ResourceAcquisitionException
instance Exception ResourceAcquisitionException where
  toException = manifestExceptionToException
  fromException = manifestExceptionFromException

-- | A ResourceDescriptor determines a kind of resource, and describes how
--   to produce one.
--   We demand Eq because we want the descriptor to uniquely determine a
--   resource. Two descriptors must be equal if and only if the resources
--   they would produce (via acquireResource) would be observationally
--   identical!
--   Typeable is needed so that we can use it in a dependent map.
class (Eq rd, Typeable rd) => ResourceDescriptor rd where
  type ResourceType rd :: *
  acquireResource :: rd -> ExceptT ResourceAcquisitionException IO (Resource (ResourceType rd))

type DResourceKey = Identity

data DResourceMap
type instance DM.DependentMapFunction DResourceMap a = ResourceType a

releaseAll :: DM.DependentMap DResourceMap DResourceKey Resource -> IO ()
releaseAll dmap = DM.foldWithKey releaseOne (return ()) dmap
  where
    releaseOne _ res io = io >> release res

commitAll :: DM.DependentMap DResourceMap DResourceKey Resource -> IO ()
commitAll dmap = DM.foldWithKey commitOne (return ()) dmap
  where
    commitOne _ res io = io >> commit res

rollbackAll :: DM.DependentMap DResourceMap DResourceKey Resource -> IO ()
rollbackAll dmap = DM.foldWithKey rollbackOne (return ()) dmap
  where
    rollbackOne _ res io = io >> rollback res
