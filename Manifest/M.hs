{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Manifest.M (

    M
  , MF
  , runM
  , at
  , at_
  , assign
  , (.:=)
  , inspect
  , inspect_

  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Proxy
import Data.Functor.Identity
import Manifest.Function
import Manifest.Manifest
import Manifest.CommuteL

-- | A functor to describe a DSL which we call M, parameterized by another
--   functor @f.
--   The free monad over MF f gives the DSL.
data MF f t where
  MAt
    :: ( Monad m
       , CommuteL m f
       )
    => Function access domain (m range)
    -> f domain
    -> (f (m range) -> t)
    -> MF f t
  MAssign
    :: Function ReadWrite domain range
    -> f domain
    -> f range
    -> t
    -> MF f t
  MInspect
    :: f a
    -> (f a -> t)
    -> MF f t
  -- ^ This constructor allows us to treat arbitrary f-terms as part of
  --   our DSL.

instance Functor f => Functor (MF f) where
  fmap f m' = case m' of
    MAt manifest x g -> MAt manifest x (fmap f g)
    MAssign manifest x y next -> MAssign manifest x y (f next)
    MInspect fx next -> MInspect fx (fmap f next)

type M f = Free (MF f)

-- | Convenient for feeding results of `at`s to other `at`s; no need to
--   pattern match on the Maybe; we do it for you.
at
  :: ( Functor f
     , Monad m
     , CommuteL m f
     )
  => Function access domain (m range)
  -> f domain
  -> M f (f (m range))
at pf x = liftF (MAt pf x id)

at_
  :: ( Applicative f
     , Monad m
     , CommuteL m f
     )
  => Function access domain (m range)
  -> domain
  -> M f (f (m range))
at_ pf x = at pf (pure x)

assign
  :: ( Functor f
     , Applicative f
     )
  => Function ReadWrite domain range
  -> f domain
  -> f range
  -> M f (f ())
assign pf x y = liftF (MAssign pf x y (pure ()))

infixr 1 .:=

(.:=)
  :: ( Applicative f
     , Functor f
     )
  => (Function ReadWrite domain range, domain)
  -> range
  -> M f (f ())
(.:=) (pf, x) y = assign pf (pure x) (pure y)

-- | Sometimes you want to switch the M computation based on the value inside
--   an f. Do that via this.
inspect
  :: ( PFStrategy f
     )
  => f a
  -> (a -> M f (f b))
  -> M f (f b)
inspect term k = liftF $ MInspect term (\fx -> fx >>= runM . k)

-- | With inspect, we just lift pure computation in through the f, pushing out
--   another f-wrapped value, so that the DSL user still has to work with
--   f-terms until the very end.
inspect_
  :: ( Functor f
     , Monad f
     )
  => f a
  -> (a -> b)
  -> M f (f b)
inspect_ term k = liftF (MInspect term (\fx -> fx >>= return . k))

runMF
  :: forall f a .
     ( PFStrategy f
     )
  => MF f (f a)
  -> f a
runMF term = case term of
    MAt pf mx next -> next (mx >>= runAt pf)
    MAssign pf mx my next -> assignment pf mx my *> next
    MInspect fx next -> next fx
  where
    assignment pf mx my = do
      x <- mx
      y <- my
      runAssign pf x y

-- | iterM won't suit our needs, because it puts a return in the Pure case:
--     iterM _ (Pure x) = return x
--   but here we assume the monadic term is already there (in fact, we don't
--   even demand that it's a Monad nor an Applicative, as it's no longer
--   necessary).
iterM' :: (Functor f) => (f (m a) -> m a) -> Free f (m a) -> m a
iterM' _   (Pure x) = x
iterM' phi (Free f) = phi $ fmap (iterM' phi) f

runM
  :: ( PFStrategy f
     )
  => M f (f a)
  -> f a
runM = iterM' runMF
