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
import Control.Concurrent.Async
import Control.Exception
import Data.Functor.Identity
import Data.Typeable
import qualified Data.DependentMap as DM
import Manifest.Manifest
import Manifest.ManifestException
import Manifest.Resource
import Manifest.PartialFunction

-- TODO this strategy has everything to do with the guarantees that Manifest
-- should provide. We ought to ditch the PFStrategy class and just demand that
-- every M term has this functor as its f parameter.

newtype SmartStrategy a = SmartStrategy {
    unSmartStrategy :: ExceptT SomeException (StateT StrategyState Concurrently) a
  }

deriving instance Functor SmartStrategy
deriving instance Applicative SmartStrategy
deriving instance Monad SmartStrategy

runSmartStrategy :: SmartStrategy a -> IO (Either SomeException a)
runSmartStrategy (SmartStrategy m) = do
    let stateM = runExceptT m
    let concM = runStateT stateM DM.empty
    (outcome, resources) <- runConcurrently concM
    val <- case outcome of
      Left problem -> rollbackAll resources >> return (Left problem)
      Right val -> commitAll resources >> return (Right val)
    releaseAll resources
    return val

instance PFStrategy SmartStrategy where

  runGet m x = SmartStrategy $ do
      resrc <- assureResource m
      let r = resource resrc
      let x_ = mdomainDump m x
      let getAction = mget m r x_
      outcome <- lift . lift . Concurrently $ runExceptT getAction
      case outcome of
        Left problem -> throwE problem
        Right Nothing -> return Nothing
        Right (Just value) -> return $ mrangePull m value

  runSet m x y = SmartStrategy $ do
      resrc <- assureResource m
      let r = resource resrc
      let x_ = mdomainDump m x
      let y_ = mrangeDump m <$> y
      let setAction = mset m r x_ y_
      outcome <- lift . lift . Concurrently $ runExceptT setAction
      case outcome of
        Left problem -> throwE problem
        Right () -> return ()

assureResource
  :: ( Manifest m
     , Typeable (ManifestResourceDescriptor m ftype access domain range)
     , Eq (ManifestResourceDescriptor m ftype access domain range)
     , ResourceDescriptor (ManifestResourceDescriptor m ftype access domain range)
     )
  => m ftype access domain range
  -> ExceptT SomeException (StateT StrategyState Concurrently) (Resource (ResourceType (ManifestResourceDescriptor m ftype access domain range)))
assureResource m = do
    map <- lift get
    let rd = resourceDescriptor m
    let key = Identity rd
    -- ^ Keys in the resource map are Identity functor values!
    case DM.lookup key map of
      Just resrc -> return resrc
      Nothing -> do
        outcome <- lift . lift . Concurrently $ runExceptT (acquireResource rd)
        case outcome of
          Left problem -> mthrow problem
          Right resrc -> lift (put (DM.insert key resrc map)) >> return resrc

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
