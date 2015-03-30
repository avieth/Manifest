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

  , fmapPF
  , contramapPF


  , PFStrategy(..)

  , runAt
  , runAssign

  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Manifest.CommuteL
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
       , Monad f
       )
    => m ftype access domain range
    -> PartialFunctionN access domain (f range)
  CPFN
    :: Monad m
    => PartialFunctionN access1 domain (m range1)
    -> PartialFunctionN access2 range1 (m range)
    -> PartialFunctionN ReadOnly domain (m range)
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
       , Monad f
       )
    => m FInjective access domain range
    -> PartialFunctionI access domain (f range)
  CPFI
    :: Monad m
    => PartialFunctionI access1 domain (m range1)
    -> PartialFunctionI access2 range1 (m range)
    -> PartialFunctionI ReadOnly domain (m range)
  IPFI
    :: PartialFunctionI access domain (Identity range)
    -> PartialFunctionI access range (Identity domain)

data PartialFunction :: FType -> Access -> * -> * -> * where
  Normal :: PartialFunctionN access a b -> PartialFunction FNotInjective access a b
  Injective :: PartialFunctionI access a b -> PartialFunction FInjective access a b

-- | We can convert an injective partial function to a noninjective one (not
--   changing the definition, just forgetting the injectivity).
makeN :: PartialFunctionI access domain range -> PartialFunctionN access domain range
makeN pfi = case pfi of
  PFI manifest -> PFN manifest
  CPFI pfiA pfiB -> CPFN (makeN pfiA) (makeN pfiB)
  IPFI pfi' -> makeN (pfInvert pfi')

-- | Inversion makes sense only for injective partial functions whose range
--   lies in the Identity monad.
pfInvert
  :: PartialFunctionI access domain (Identity range)
  -> PartialFunctionI access range (Identity domain)
pfInvert pf = case pf of
  PFI manifest -> PFI $ minvert manifest
  CPFI pfA pfB -> CPFI (pfInvert pfB) (pfInvert pfA)
  IPFI pf' -> pf'

function
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m ftype access domain range)
     , ManifestDomainConstraint m domain range
     , ManifestRangeConstraint m domain range
     , AccessConstraint m access
     , Monad f
     )
  => m ftype access domain range
  -> PartialFunction FNotInjective access domain (f range)
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
     , Monad f
     )
  => m FInjective access domain range
  -> PartialFunction FInjective access domain (f range)
injection = Injective . PFI

compose
  :: Monad m
  => PartialFunction ftype1 access1 domain (m range1)
  -> PartialFunction ftype2 access2 range1 (m range)
  -> PartialFunction (FTypeMeet ftype1 ftype2) ReadOnly domain (m range)
compose pfA pfB = case pfA of
  Normal pfnA -> case pfB of
    Normal pfnB -> Normal $ CPFN pfnA pfnB
    Injective pfiB -> Normal $ CPFN pfnA (makeN pfiB)
  Injective pfiA -> case pfB of
    Normal pfnB -> Normal $ CPFN (makeN pfiA) pfnB
    Injective pfiB -> Injective $ CPFI pfiA pfiB

(~>)
  :: Monad m
  => PartialFunction ftype1 access1 domain (m range1)
  -> PartialFunction ftype2 access2 range1 (m range)
  -> PartialFunction (FTypeMeet ftype1 ftype2) ReadOnly domain (m range)
(~>) = compose

invert
  :: PartialFunction FInjective access domain (Identity range)
  -> PartialFunction FInjective access range (Identity domain)
invert pf = case pf of
  Injective pfi -> Injective $ pfInvert pfi

-- | Functor-like but not quite a functor because the Access parameter may
--   change.
fmapPF
  :: Monad m
  => (range -> range')
  -> PartialFunction ftype access domain (m range)
  -> PartialFunction (FTypeMeet ftype FNotInjective) ReadOnly domain (m range')
fmapPF f pf = pf ~> (function (pureFunction f))

-- | Contravariant functor-like but not quite because the Access parameter may
--   change.
contramapPF
  :: Monad m
  => (domain' -> domain)
  -> PartialFunction ftype access domain (m range)
  -> PartialFunction (FTypeMeet FNotInjective ftype) ReadOnly domain' (m range)
contramapPF f pf = (function (pureFunction f)) ~> pf

class (Functor f, Applicative f, Monad f) => PFStrategy f where

  runGet
    :: ( ManifestRead manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest ftype access domain range)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       )
    => manifest ftype access domain range
    -> domain
    -> f range

  runSet
    :: ( ManifestWrite manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest ftype ReadWrite domain range)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       )
    => manifest ftype ReadWrite domain range
    -> domain
    -> range
    -> f ()

runAt
  :: ( PFStrategy f
     , Monad m
     , CommuteL m f
     )
  => PartialFunction ftype access domain (m range)
  -> domain
  -> f (m range)
runAt pf x = case pf of
    Normal (PFN manifest) -> return <$> runGet manifest x
    Injective (PFI manifest) -> return <$> runGet manifest x
    Injective (IPFI pfA) -> runAt (Injective $ pfInvert pfA) x
    Normal (CPFN pfA pfB) -> do
      -- Is this even possible? I don't think so. We cannot exit
      -- m unless f and m commute, so that we can go from
      --   (m (f (m range)))
      -- to
      --   f (m range)
      -- in a reasonable way.
      --
      -- Yes, we have an (m range1) and we can recurse to get an
      -- (m (f (m range2))) but what we actually need is an f (m range2) so
      -- we must commute the f out front and then fmap a join into the term.
      -- I think we get this for free if m is traversable; it's just
      -- sequence.
      y <- runAt (Normal pfA) x
      let getNext = return . runAt (Normal pfB)
      let next = y >>= getNext
      join <$> commuteL next
    Injective (CPFI pfA pfB) -> do
      y <- runAt (Injective pfA) x
      let getNext = return . runAt (Injective pfB)
      let next = y >>= getNext
      join <$> commuteL next

-- What if we added kleisli-arrow style things to our language?
-- So we'd have PFCompose, but also PFBind.
--
--   PFBind
--     :: Monad m
--     => PartialFunction ftype access domain (m range)
--     -> PartialFunction ftype' access' range (m range')
--     -> PartialFunction FNotInjective ReadOnly domain (m range')
--
-- this would be real cool. Does it demand a new term, though? Can we
-- not do it via existing terms? I don't think so; it needs to be
-- interpreted differently by runAt.
-- Hm no this may not be possible, because we'll have to do a bind inside
-- the interpreter, and we cannot in general exit the monad. I believe
-- traversable really may be the way to go here. 
--
-- Ah, you know what we could do? Always run with a monadic range! Then we
-- recover normal composition via the Identity monad.
--   Identity x -> (x -> Identity y) -> Identity y
--
--

runAssign
  :: ( PFStrategy f
     )
  => PartialFunction ftype ReadWrite domain (Identity range)
  -> domain
  -> range
  -> f ()
runAssign pf x y = case pf of
    Normal (PFN manifest) -> runSet manifest x y
    Injective (PFI manifest) -> runSet manifest x y
    Injective (IPFI pf') -> runAssign (Injective $ pfInvert pf') x y
    -- Other cases ruled out by Access type.
