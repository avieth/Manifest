{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Manifest.PartialFunction (

    PartialFunction

  , function
  , injection
  , compose
  , (~>)
  , invert

  , PFStrategy(..)

  , runAt
  , runAssign

  ) where

import Control.Applicative
import Control.Monad
import Manifest.Manifest
import Manifest.FType
import Manifest.Resource

data PartialFunctionN :: Access -> * -> * -> * where
  PFN
    :: ( ManifestRead m
       , ResourceDescriptor (ManifestResourceDescriptor m)
       , ManifestDomainConstraint m domain range
       , ManifestRangeConstraint m domain range
       , AccessConstraint m access
       )
    => m mtype access domain range
    -> PartialFunctionN access domain range
  CPFN
    :: PartialFunctionN access1 domain range1
    -> PartialFunctionN access2 range1 range
    -> PartialFunctionN ReadOnly domain range
    -- ^ Always ReadOnly; you can only update an individual manifest, not
    --   a composition.

data PartialFunctionI :: Access -> * -> * -> * where
  PFI
    :: ( ManifestRead m
       , ResourceDescriptor (ManifestResourceDescriptor m)
       , ManifestDomainConstraint m domain range
       , ManifestRangeConstraint m domain range
       , ManifestDomainConstraint m range domain
       , ManifestRangeConstraint m range domain
       -- ^ We need the range and domain constraints on both sides, since we
       --   may invert!
       , AccessConstraint m access
       , ManifestInjective m
       )
    => m FInjective access domain range
    -> PartialFunctionI access domain range
  CPFI
    :: PartialFunctionI access1 domain range1
    -> PartialFunctionI access2 range1 range
    -> PartialFunctionI ReadOnly domain range
  IPFI
    :: PartialFunctionI access domain range
    -> PartialFunctionI access range domain

data PartialFunction :: FType -> Access -> * -> * -> * where
  Normal :: PartialFunctionN access a b -> PartialFunction FNotInjective access a b
  Injective :: PartialFunctionI access a b -> PartialFunction FInjective access a b

makeN :: PartialFunctionI access domain range -> PartialFunctionN access domain range
makeN pfi = case pfi of
  PFI manifest -> PFN manifest
  CPFI pfiA pfiB -> CPFN (makeN pfiA) (makeN pfiB)
  IPFI pfi' -> makeN (pfInvert pfi')

function
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m)
     , ManifestDomainConstraint m domain range
     , ManifestRangeConstraint m domain range
     , AccessConstraint m access
     )
  => m mtype access domain range
  -> PartialFunction FNotInjective access domain range
function = Normal . PFN

injection
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m)
     , ManifestDomainConstraint m domain range
     , ManifestRangeConstraint m domain range
     , ManifestDomainConstraint m range domain
     , ManifestRangeConstraint m range domain
     -- ^ We need the range and domain constraints on both sides, since we
     --   may invert!
     , AccessConstraint m access
     , ManifestInjective m
     )
  => m FInjective access domain range
  -> PartialFunction FInjective access domain range
injection = Injective . PFI

compose
  :: PartialFunction mtype1 access1 domain range1
  -> PartialFunction mtype2 access2 range1 range
  -> PartialFunction (FTypeMeet mtype1 mtype2) ReadOnly domain range
compose pfA pfB = case pfA of
  Normal pfnA -> case pfB of
    Normal pfnB -> Normal $ CPFN pfnA pfnB
    Injective pfiB -> Normal $ CPFN pfnA (makeN pfiB)
  Injective pfiA -> case pfB of
    Normal pfnB -> Normal $ CPFN (makeN pfiA) pfnB
    Injective pfiB -> Injective $ CPFI pfiA pfiB

(~>) = compose

invert
  :: PartialFunction FInjective access domain range
  -> PartialFunction FInjective access range domain
invert pf = case pf of
  Injective pfi -> Injective $ pfInvert pfi

pfInvert :: PartialFunctionI access domain range -> PartialFunctionI access range domain
pfInvert pf = case pf of
  PFI manifest -> PFI $ minvert manifest
  CPFI pfA pfB -> CPFI (pfInvert pfB) (pfInvert pfA)
  IPFI pf' -> pf'

-- Notice how the function signatures look almost exactly like the signatures
-- of the consturctors MFAt and MFAssign.
-- We don't unroll the f around the arguments (like Maybe domain in runAt)
-- because that would demand using a bind in the M2At clause of runM2, but
-- I suspect that may be too strict. Leaving them there, we offer more
-- flexibility to the f definition: maybe it wants to fork and get the
-- domain value in another thread, for instance.
class (Functor f, Applicative f, Monad f) => PFStrategy f where

  runGet
    :: ( ManifestRead manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       )
    => manifest mtype access domain range
    -> f (Maybe domain)
    -> f (Maybe range)

  runSet
    :: ( ManifestWrite manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       )
    => manifest mtype ReadWrite domain range
    -> f (Maybe domain)
    -> f (Maybe range)
    -> f ()

runAt
  :: ( PFStrategy f
     )
  => PartialFunction mtype access domain range
  -> f (Maybe domain)
  -> f (Maybe range)
runAt pf x = case pf of
  Normal (PFN manifest) -> runGet manifest x
  Injective (PFI manifest) -> runGet manifest x
  Normal (CPFN pfA pfB) -> do
    y <- runAt (Normal pfA) x
    case y of
      Nothing -> return Nothing
      Just y' -> runAt (Normal pfB) (return $ Just y')
  Injective (CPFI pfA pfB) -> do
    y <- runAt (Injective pfA) x
    case y of
      Nothing -> return Nothing
      Just y' -> runAt (Injective pfB) (return $ Just y')
  Injective (IPFI pfA) -> runAt (Injective $ pfInvert pfA) x

runAssign
  :: ( PFStrategy f
     )
  => PartialFunction mtype ReadWrite domain range
  -> f (Maybe domain)
  -> f (Maybe range)
  -> f ()
runAssign pf x y = case pf of
  Normal (PFN manifest) -> runSet manifest x y
  Injective (PFI manifest) -> runSet manifest x y
  Injective (IPFI pf') -> runAssign (Injective $ pfInvert pf') x y
  -- Other cases ruled out by Access type.


{-
runGet
  :: ( ManifestRead manifest
     , ResourceDescriptor (ManifestResourceDescriptor manifest)
     , ManifestDomainConstraint manifest domain range
     , ManifestRangeConstraint manifest domain range
     )
  => manifest mtype access domain range
  -> domain
  -> StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO (Maybe range)
runGet manifest x = do
    dmap <- get
    rsrc <- case DM.lookup (Identity $ resourceDescriptor manifest) dmap of
      Nothing -> do
          r <- liftIO $ acquireResource (resourceDescriptor manifest)
          put $ DM.insert (Identity $ resourceDescriptor manifest) r dmap
          return r
      Just r -> return r
    y <- liftIO $ mget manifest (resource rsrc) (mdomainDump manifest x)
    return $ case y of
      Nothing -> Nothing
      Just y' -> mrangePull manifest y'

runSet
  :: ( ManifestWrite manifest
     , ResourceDescriptor (ManifestResourceDescriptor manifest)
     , ManifestDomainConstraint manifest domain range
     , ManifestRangeConstraint manifest domain range
     )
  => manifest mtype ReadWrite domain range
  -> domain
  -> Maybe range
  -> StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO ()
runSet manifest x y = do
    dmap <- get
    rsrc <- case DM.lookup (Identity $ resourceDescriptor manifest) dmap of
      Nothing -> do
          r <- liftIO $ acquireResource (resourceDescriptor manifest)
          put $ DM.insert (Identity $ resourceDescriptor manifest) r dmap
          return r
      Just r -> return r
    let y' = mrangeDump manifest <$> y
    liftIO $ mset manifest (resource rsrc) (mdomainDump manifest x) y'

-}

