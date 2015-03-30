{-|
Module      : Manifest.SmartStrategy
Description : A good PFStrategy.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Manifest.SmartStrategy (

    SmartStrategy
  , runSmartStrategy

  ) where

import Control.Concurrent.STM (retry)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Exception
import Data.Functor.Identity
import Data.Typeable
import qualified Data.DependentMap as DM
import qualified Control.Concurrent.SafeConcurrentState as SCS
import Manifest.Manifest
import Manifest.ManifestException
import Manifest.Resource
import Manifest.Function

-- TODO this strategy has everything to do with the guarantees that Manifest
-- should provide. We ought to ditch the PFStrategy class and just demand that
-- every M term has this functor as its f parameter.

newtype SmartStrategy a = SmartStrategy {
    unSmartStrategy :: SCS.SafeConcurrentState StrategyState a
  }

deriving instance Functor SmartStrategy
deriving instance Applicative SmartStrategy
deriving instance Monad SmartStrategy

runSmartStrategy :: SmartStrategy a -> IO (Either SomeException a)
runSmartStrategy (SmartStrategy m) =
    SCS.runBracket DM.empty m onException onNoException cleanup
  where
    onException (ex, resources) = rollbackAll resources >> return (Left ex, resources)
    onNoException (value, resources) = commitAll resources >> return (Right value, resources)
    cleanup (outcome, resources) = releaseAll resources >> return outcome

instance PFStrategy SmartStrategy where

  runGet m x = SmartStrategy $ do
      resrc <- assureResource m
      let r = resource resrc
      let x_ = mdomainDump m x
      let getAction = mget m r x_
      y <- SCS.embedIO getAction
      return (y >>= mrangePull m)

  runSet m x y = SmartStrategy $ do
      resrc <- assureResource m
      let r = resource resrc
      let x_ = mdomainDump m x
      let y_ = mrangeDump m <$> y
      let setAction = mset m r x_ y_
      SCS.embedIO setAction

assureResource
  :: ( Manifest m
     , Typeable (ManifestResourceDescriptor m access domain range)
     , Eq (ManifestResourceDescriptor m access domain range)
     , ResourceDescriptor (ManifestResourceDescriptor m access domain range)
     )
  => m access domain range
  -> SCS.SafeConcurrentState StrategyState (Resource (ResourceType (ManifestResourceDescriptor m access domain range)))
assureResource m = do
    -- What we really want:
    --   let rd = resourceDescriptor m
    --   let key = Identity rd
    --   map <- SCS.get
    --   resrc <- getOrInsert rd (acquireResource rd) map
    --   SCS.set map
    -- As you can see, the SCS state is irrelevant, since the real mutable
    -- action happens inside getOrInsert. SafeConcurrentState becomes useless
    -- here; it's a SafeConcurrenetDependentMap that we need!
    -- Yes, a thread- and exception-safe mutable dependent map would be ideal,
    -- and we could just take it from a reader. If the computation blows up,
    -- the map given to the reader is still there for inspection; accomplishes
    -- the same thing that SafeConcurrentState does
    let rd = resourceDescriptor m
    let key = Identity rd
    -- ^ Keys in the resource map are Identity functor values!

    -- We have atomic access to the resource map, but during an STM transaction
    -- with that map, we are incapable of acquiring a resource because that's
    -- IO! Nevertheless, we must somehow ensure that we never attempt to
    -- acquire a resource for the same descriptor twice. To this end, we
    -- atomically modify the map with a return value:
    --   - if nothing is there, place a Pending, then later we'll acquire the
    --     resource a put in a Present value.
    --   - if Pending, retry. We expect some other thread to replace it with
    --     Present soon.
    --   - if Present, take the resource and use it.
    resourceState <- SCS.modifySTM $ \map -> do
        case DM.lookup key map of
          Just (Present resrc) -> return (Present resrc, map)
          Just Pending -> retry
          -- TBD seems like a wart, exposing modifySTM and demanding users
          -- actually work with STM. Is there a better way?
          Nothing -> return (Pending, DM.insert key Pending map)

    case resourceState of
      Present resrc -> return resrc
      Pending -> do
          resrc <- SCS.embedIO $ acquireResource rd
          -- TODO should catch exceptions in acquireResource rd so we
          -- can modify the map to remove the Pending, but it doesn't really
          -- matter since in practice we'll abort the entire SCS computation
          -- anyway.
          -- Indeed, we wouldn't want to remove resource descriptor from the
          -- map, else some other thread may try to acquire the resource again,
          -- and again, and again, and so on, and nothing will ever be
          -- accomplished.
          SCS.modify (\map -> ((), DM.insert key (Present resrc) map))
          return resrc

data ConcurrentResource a = Pending | Present (Resource a)

type StrategyState = DM.DependentMap DResourceMap DResourceKey ConcurrentResource

type DResourceKey = Identity

data DResourceMap
type instance DM.DependentMapFunction DResourceMap a = ResourceType a

releaseAll :: DM.DependentMap DResourceMap DResourceKey ConcurrentResource -> IO ()
releaseAll dmap = DM.foldWithKey releaseOne (return ()) dmap
  where
    releaseOne _ res io = case res of
        Pending -> io
        -- ^ It is safe to assume here that the resource could not be acquired
        --   (acquireResource threw an exception) and that it will never be
        --   acquired, so we can just ignore the Pending.
        --   See assureResource; if a Pending is inserted, it is updated later
        --   UNLESS acquireResource blows up.
        Present res -> io >> release res

commitAll :: DM.DependentMap DResourceMap DResourceKey ConcurrentResource -> IO ()
commitAll dmap = DM.foldWithKey commitOne (return ()) dmap
  where
    commitOne _ res io = case res of
        Pending -> io
        Present res -> io >> commit res

rollbackAll :: DM.DependentMap DResourceMap DResourceKey ConcurrentResource -> IO ()
rollbackAll dmap = DM.foldWithKey rollbackOne (return ()) dmap
  where
    rollbackOne _ res io = case res of
        Pending -> io
        Present res -> io >> rollback res
