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
import Manifest.PartialFunction

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
     , Typeable (ManifestResourceDescriptor m ftype access domain range)
     , Eq (ManifestResourceDescriptor m ftype access domain range)
     , ResourceDescriptor (ManifestResourceDescriptor m ftype access domain range)
     )
  => m ftype access domain range
  -> SCS.SafeConcurrentState StrategyState (Resource (ResourceType (ManifestResourceDescriptor m ftype access domain range)))
assureResource m = do
    map <- SCS.get
    let rd = resourceDescriptor m
    let key = Identity rd
    -- ^ Keys in the resource map are Identity functor values!
    case DM.lookup key map of
      Just resrc -> return resrc
      Nothing -> do
        resrc <- SCS.embedIO $ acquireResource rd
        -- DANGER! Somebody else could try to acquire the resource
        -- concurrently. Will this be a problem!?
        SCS.modify (updateMap key resrc)
        -- DANGER! This may overwrite something! Will this be a problem?
        -- Yes it will !!! The old resource will never be released. So, we
        -- have a problem that STM cannot solve.
        -- Maybe we could put an "acquiring..." into the map
        -- atomically if it's not there. After acquisition, we replace it
        -- with the resource. But what do we do if we find "acquiring..."
        -- in there? Just wait? We'd want an MVar in any case. The MVar
        -- signals that "Someone is trying to acquire a resource for this
        -- descriptor, please wait".
        -- Hm, could we build that on top of the dependent map?
        -- ConcurrentDependentMap?
        return resrc
  where
    updateMap key resrc map = ((), DM.insert key resrc map)

type StrategyState = DM.DependentMap DResourceMap DResourceKey Resource

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
