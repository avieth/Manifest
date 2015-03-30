{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Manifest.Function (

    Function
  , abstraction
  , concretion
  , function
  , compose
  , (~>)
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
import Manifest.Resource
import Manifest.Pure

data Function :: Access -> * -> * -> * where
  FN
    :: ( ManifestRead m
       , ResourceDescriptor (ManifestResourceDescriptor m access domain range)
       , ManifestDomainConstraint m domain range
       , ManifestRangeConstraint m domain range
       , AccessConstraint m access
       , f ~ ManifestFunctor m domain range
       , Functor f
       )
    => m access domain range
    -> Function access domain (f range)
  CFN
    :: Monad m
    => Function access1 domain (m range1)
    -> Function access2 range1 (m range)
    -> Function ReadOnly domain (m range)
    -- ^ Always ReadOnly; you can only update an individual manifest, not
    --   a composition.
  FL :: (range -> m range) -> Function access domain range -> Function ReadOnly domain (m range)
  FD :: (m range -> range) -> Function access domain (m range) -> Function ReadOnly domain range

abstraction :: (range -> m range) -> Function access domain range -> Function ReadOnly domain (m range)
abstraction = FL

concretion :: (m range -> range) -> Function access domain (m range) -> Function ReadOnly domain range
concretion = FD

function
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m access domain range)
     , ManifestDomainConstraint m domain range
     , ManifestRangeConstraint m domain range
     , AccessConstraint m access
     , f ~ ManifestFunctor m domain range
     , Functor f
     )
  => m access domain range
  -> Function access domain (f range)
function = FN

compose
  :: Monad m
  => Function access1 domain (m range1)
  -> Function access2 range1 (m range)
  -> Function ReadOnly domain (m range)
compose = CFN

infixr 1 ~>

(~>)
  :: Monad m
  => Function access1 domain (m range1)
  -> Function access2 range1 (m range)
  -> Function ReadOnly domain (m range)
(~>) = compose

pureFunction :: (a -> b) -> Function ReadOnly a b
pureFunction = concretion runIdentity . function . pureManifest

-- | Functor-like but not quite a functor because the Access parameter may
--   change.
fmapPF
  :: Monad m
  => (range -> range')
  -> Function access domain (m range)
  -> Function ReadOnly domain (m range')
fmapPF f pf = pf ~> pureFunction (return . f)

-- | Contravariant functor-like but not quite because the Access parameter may
--   change.
contramapPF
  :: Monad m
  => (domain' -> domain)
  -> Function access domain (m range)
  -> Function ReadOnly domain' (m range)
contramapPF f pf = pureFunction (return . f) ~> pf

class (Functor f, Applicative f, Monad f) => PFStrategy f where

  runGet
    :: ( ManifestRead manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest access domain range)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       , g ~ ManifestFunctor manifest domain range
       , Monad g
       )
    => manifest access domain range
    -> domain
    -> f (g range)

  runSet
    :: ( ManifestWrite manifest
       , ResourceDescriptor (ManifestResourceDescriptor manifest ReadWrite domain range)
       , ManifestDomainConstraint manifest domain range
       , ManifestRangeConstraint manifest domain range
       , g ~ ManifestFunctor manifest domain range
       , Functor g
       )
    => manifest ReadWrite domain range
    -> domain
    -> g range
    -> f ()

runAt
  :: ( PFStrategy f
     , Monad m
     , CommuteL m f
     )
  => Function access domain (m range)
  -> domain
  -> f (m range)
runAt pf x = case pf of
    FN manifest -> runGet manifest x
    CFN pfA pfB -> do
      -- we have an (m range1) and we can recurse to get an
      -- (m (f (m range2))) but what we actually need is an f (m range2) so
      -- we must commute the f out front and then fmap a join into the term.
      -- I think we get this for free if m is traversable; it's just
      -- sequence.
      y <- runAt pfA x
      let getNext = return . runAt pfB
      let next = y >>= getNext
      join <$> commuteL next

runAssign
  :: ( PFStrategy f
     )
  => Function ReadWrite domain range
  -> domain
  -> range
  -> f ()
runAssign pf x y = case pf of
    FN manifest -> runSet manifest x y
    -- Other case ruled out by Access type.
