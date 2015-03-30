{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Manifest.Function (

    Function
  , shift
  , function
  , compose
  , (~>)
  , fmapF
  , contramapF
  , pureFunction

  , PFStrategy(..)
  , runAt
  , runAssign

  ) where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Data.Functor.Identity
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
       , Traversable f
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
  FS
    :: ( Traversable f
       , Traversable g
       , Monad f
       , Monad g
       )
    => (f range -> g range)
    -> Function access domain (f range)
    -> Function ReadOnly domain (g range)

shift
  :: ( Traversable f
     , Traversable g
     , Monad f
     , Monad g
     )
  => (f range -> g range)
  -> Function access domain (f range)
  -> Function ReadOnly domain (g range)
shift = FS

function
  :: ( ManifestRead m
     , ResourceDescriptor (ManifestResourceDescriptor m access domain range)
     , ManifestDomainConstraint m domain range
     , ManifestRangeConstraint m domain range
     , AccessConstraint m access
     , f ~ ManifestFunctor m domain range
     , Functor f
     , Traversable f
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

pureFunction :: (Monad m, Traversable m) => (a -> m b) -> Function ReadOnly a (m b)
pureFunction f = function (pureManifest f)

pureFunction' :: (Monad m, Traversable m) => (a -> b) -> Function ReadOnly a (m b)
pureFunction' f = function (pureManifest (return . f))

identityToAny :: Monad m => Identity a -> m a
identityToAny = return . runIdentity

-- | Covariant functor-like.
fmapF
  :: ( Monad m
     , Traversable m
     )
  => (range -> range')
  -> Function access domain (m range)
  -> Function ReadOnly domain (m range')
fmapF f pf = pf ~> shift identityToAny (pureFunction' f)

-- | Contravariant functor-like.
contramapF
  :: ( Monad m
     , Traversable m
     )
  => (domain' -> domain)
  -> Function access domain (m range)
  -> Function ReadOnly domain' (m range)
contramapF f pf = shift identityToAny (pureFunction' f) ~> pf

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
     , Traversable m
     )
  => Function access domain (m range)
  -> domain
  -> f (m range)
runAt fn x = case fn of
    FN manifest -> runGet manifest x
    CFN fnA fnB -> do
      -- we have an (m range1) and we can recurse to get an
      -- (m (f (m range2))) but what we actually need is an f (m range2) so
      -- we must commute the f out front and then fmap a join into the term.
      -- I think we get this for free if m is traversable; it's just
      -- sequence.
      y <- runAt fnA x
      let getNext = return . runAt fnB
      let next = y >>= getNext
      join <$> sequenceA next
    FS shifter g -> shifter <$> runAt g x

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
