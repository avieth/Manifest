{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Manifest.PartialFunction (

    PartialFunction

  , fmapPF
  , contramapPF

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
import Manifest.Pure

data PartialFunctionN :: Access -> * -> * -> * where
  PFN
    :: ( ManifestRead m
       , ResourceDescriptor (ManifestResourceDescriptor m ftype access domain range)
       , ManifestDomainConstraint m domain range
       , ManifestRangeConstraint m domain range
       , AccessConstraint m access
       )
    => m ftype access domain range
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
       , ResourceDescriptor (ManifestResourceDescriptor m FInjective access domain range)
       , ResourceDescriptor (ManifestResourceDescriptor m FInjective access range domain)
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

-- | Functor-like but not quite a functor because the Access parameter may
--   change.
fmapPF
  :: (range -> range')
  -> PartialFunction ftype access domain range
  -> PartialFunction (FTypeMeet ftype FNotInjective) ReadOnly domain range'
fmapPF f pf = pf ~> (function (pureFunction (fmap Just f)))

-- | Contravariant functor-like but not quite because the Access parameter may
--   change.
contramapPF
  :: (domain' -> domain)
  -> PartialFunction ftype access domain range
  -> PartialFunction (FTypeMeet FNotInjective ftype) ReadOnly domain' range
contramapPF f pf = (function (pureFunction (fmap Just f))) ~> pf

function
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m ftype access domain range)
     , ManifestDomainConstraint m domain range
     , ManifestRangeConstraint m domain range
     , AccessConstraint m access
     )
  => m ftype access domain range
  -> PartialFunction FNotInjective access domain range
function = Normal . PFN

injection
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m FInjective access domain range)
     , ResourceDescriptor (ManifestResourceDescriptor m FInjective access range domain)
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
  :: PartialFunction ftype1 access1 domain range1
  -> PartialFunction ftype2 access2 range1 range
  -> PartialFunction (FTypeMeet ftype1 ftype2) ReadOnly domain range
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

class (Functor f, Applicative f, Monad f) => PFStrategy f where

  runGet
    :: ( ManifestRead manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest ftype access domain range)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       )
    => manifest ftype access domain range
    -> domain
    -> f (Maybe range)

  runSet
    :: ( ManifestWrite manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest ftype ReadWrite domain range)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       )
    => manifest ftype ReadWrite domain range
    -> domain
    -> Maybe range
    -> f ()

runAt
  :: ( PFStrategy f
     )
  => PartialFunction ftype access domain range
  -> domain
  -> f (Maybe range)
runAt pf x = case pf of
    Normal (PFN manifest) -> runGet manifest x
    Injective (PFI manifest) -> runGet manifest x
    Normal (CPFN pfA pfB) -> do
      y <- runAt (Normal pfA) x
      case y of
        Nothing -> return Nothing
        Just y' -> runAt (Normal pfB) y'
    Injective (CPFI pfA pfB) -> do
      y <- runAt (Injective pfA) x
      case y of
        Nothing -> return Nothing
        Just y' -> runAt (Injective pfB) y'
    Injective (IPFI pfA) -> runAt (Injective $ pfInvert pfA) x

runAssign
  :: ( PFStrategy f
     )
  => PartialFunction ftype ReadWrite domain range
  -> domain
  -> Maybe range
  -> f ()
runAssign pf x y = case pf of
    Normal (PFN manifest) -> runSet manifest x y
    Injective (PFI manifest) -> runSet manifest x y
    Injective (IPFI pf') -> runAssign (Injective $ pfInvert pf') x y
    -- Other cases ruled out by Access type.
