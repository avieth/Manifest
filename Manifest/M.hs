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
import Manifest.PartialFunction
import Manifest.Manifest

-- | A functor to describe a DSL which we call M, parameterized by another
--   functor @f.
--   The free monad over M f gives the DSL.
data MF f t where
  MAt
    :: (
       )
    => PartialFunction mtype access domain range
    -> f (Maybe domain)
    -- ^ Yes, it has to be Maybe domain.
    --   Might be more natural to absorb the maybe into the f.
    --   MonadPlus / Alternative constraints would then be useful, rather
    --   than just demanding Maybe.
    -> (f (Maybe range) -> t)
    -> MF f t
  MAssign
    :: (
       )
    => PartialFunction mtype ReadWrite domain range
    -> f (Maybe domain)
    -- ^ See note in MAt
    -> f (Maybe range)
    -> t
    -> MF f t
  MInspect
    :: (
       )
    => f a
    -> (f a -> t)
    -> MF f t

instance Functor f => Functor (MF f) where
  fmap f m' = case m' of
    MAt manifest x g -> MAt manifest x (fmap f g)
    MAssign manifest x y next -> MAssign manifest x y (f next)
    MInspect fx next -> MInspect fx (fmap f next)

type M f = Free (MF f)

-- | Convenient for feeding results of `at`s to other `at`s; no need to
--   pattern match on the Maybe; we do it for you.
at
  :: Functor f
  => PartialFunction mtype access domain range
  -> f (Maybe domain)
  -> M f (f (Maybe range))
at pf x = liftF (MAt pf x id)

at_
  :: Applicative f
  => PartialFunction mtype access domain range
  -> domain
  -> M f (f (Maybe range))
at_ pf x = at pf (pure $ Just x)

assign
  :: Functor f
  => PartialFunction mtype ReadWrite domain range
  -> f (Maybe domain)
  -> f (Maybe range)
  -> M f ()
assign pf x y = liftF (MAssign pf x y ())

infixr 1 .:=

(.:=)
  :: Functor f
  => PartialFunction mtype ReadWrite domain range
  -> f (Maybe domain)
  -> f (Maybe range)
  -> M f ()
(.:=) = assign

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

-- | Sometimes you want to switch the M computation based on the value inside
--   an f. Do that via this.
inspect
  :: ( PFStrategy f
     )
  => f a
  -> (a -> M f (f b))
  -> M f (f b)
inspect term k = liftF $ MInspect term (\fx -> fx >>= runM . k)

ex1 :: (PFStrategy f) => M f (f (Maybe ()))
ex1 = do
  fx <- at_ undefined True
  fy <- at_ undefined False
  inspect fx (\x -> return $ pure x)
  inspect_ fx (\x -> if True then x else Nothing)

  -- Note that if we replace the last line with this, then both fx and fy
  -- will factor into the final term; otherwise, only fx does! fy is never
  -- run!
  --fx' <- inspect fx (\x -> if True then x else Nothing)
  --return $ const <$> fx' <*> fy

-- I believe this type will have to be specialized, as we cannot evaluate
-- partial functions in an arbitrary monad.
runMF
  :: forall f a .
     ( PFStrategy f
     )
  => MF f (f a)
  -> f a
runMF term = case term of
    MAt pf domain next -> next (runAt (Proxy :: Proxy f) pf domain)
    MAssign pf domain range next -> runAssign (Proxy :: Proxy f) pf domain range *> next
    -- Place next on the left so that we do all assignments at the end?!
    -- No, this won't be necessary. It's the job of the f monad to sort that
    -- out.
    --MAssign pf domain range next -> next <* runAssign pf domain range
    MInspect fx next -> next fx

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
