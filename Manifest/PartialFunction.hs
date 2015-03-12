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

  , M
  , runM
  , at
  , at_
  , assign
  , (.:=)

  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Free
import Data.Functor.Identity
import Control.Concurrent.Async
import qualified Data.DependentMap as DM
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

runPFGet
  :: (
     )
  => PartialFunction mtype access domain range
  -> domain
  -> StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO (Maybe range)
runPFGet pf x = case pf of
  Normal (PFN manifest) -> runGet manifest x
  Injective (PFI manifest) -> runGet manifest x
  Normal (CPFN pfA pfB) -> do
    y <- runPFGet (Normal pfA) x
    case y of
      Nothing -> return Nothing
      Just y' -> runPFGet (Normal pfB) y'
  Injective (CPFI pfA pfB) -> do
    y <- runPFGet (Injective pfA) x
    case y of
      Nothing -> return Nothing
      Just y' -> runPFGet (Injective pfB) y'
  Injective (IPFI pfA) -> runPFGet (Injective $ pfInvert pfA) x

runPFSet
  :: (
     )
  => PartialFunction mtype ReadWrite domain range
  -> domain
  -> Maybe range
  -> StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO ()
runPFSet pf x y = case pf of
  Normal (PFN manifest) -> runSet manifest x y
  Injective (PFI manifest) -> runSet manifest x y
  Injective (IPFI pf') -> runPFSet (Injective $ pfInvert pf') x y
  -- Other cases ruled out by Access type.

data M' t where
  MPure :: t -> M' t
  MAt
    :: (
       )
    => PartialFunction mtype access domain range
    -> domain
    -> (Maybe range -> t)
    -> M' t
  MAssign
    :: (
       )
    => PartialFunction mtype ReadWrite domain range
    -> domain
    -> Maybe range
    -> t
    -> M' t

instance Functor M' where
  fmap f m' = case m' of
    MPure x -> MPure $ f x
    MAt manifest x g -> MAt manifest x (fmap f g)
    MAssign manifest x y next -> MAssign manifest x y (f next)

type M = Free M'

at :: PartialFunction mtype access domain range -> domain -> M (Maybe range)
at pf x = liftF (MAt pf x id)

-- | Convenient for feeding results of `at`s to other `at`s; no need to
--   pattern match on the Maybe; we do it for you.
at_ :: PartialFunction mtype access domain range -> Maybe domain -> M (Maybe range)
at_ pf x = case x of
  Just x' -> at pf x'
  Nothing -> return Nothing

assign :: PartialFunction mtype ReadWrite domain range -> domain -> Maybe range -> M ()
assign pf x y = liftF (MAssign pf x y ())

infixr 1 .:=

(.:=) (pf, x) y = assign pf x y

runM :: M a -> StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO a
runM term = iterM run term >>= finalize
  where

    finalize x = do
      dmap <- get
      liftIO $ releaseAll dmap
      return x

    run :: M' (StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO a)
        -> StateT (DM.DependentMap DResourceMap DResourceKey Resource) IO a
    run m' = case m' of
      MPure action -> action
      MAt pf x nextAction -> runPFGet pf x >>= nextAction
      MAssign pf x y next -> runPFSet pf x y >> next

-- Moving forward...
--
-- Given that we would like to use backends like Redis and MongoDB, backends
-- which do not have the usual ACID transaction mechanisms that we love, it's
-- necessary to do some Haskell-side magic. The plan is to compile M terms
-- in such a way that all writes happen at the end of execution, immediately
-- before the resource is closed (with no reads happening after any write).
-- Reads, on the other hand, will be either
--   1. cached by read, in case the value has already been read
--   2. cached by write, in case the point being read was written in this M
--      term.
--   3. immediately fetched, in case the value is not cached.
-- Note that any M term can be rewritten to have no cached reads or writes, but
-- maintain the same meaning.
--
-- Hm, this may be a lost cause, since the points might not have Eq or Ord
-- instances... ah but it would be reasonable to demand that the underlying
-- transport type (ByteString or Text probably) does! So we can test on those:
-- the same resource descriptor at the same transport value means the same
-- output value.
--
-- Another option, besides caching, might be to rewrite the M terms so that
-- they are in a kind of M-normal form, in which there are no reads which
-- could be cached, and all writes appear at the end. This wouldn't be so
-- straightforward, given that we can have long sequences of composed
-- PartialFunction values... or maybe it would be easy?
--
--   - First, "desugar" all reads into a sequence of "atomic reads" on
--     actual manifests (mget calls).
--   - Now analyze the desugared term. If it's an `assign` then move it to the
--     end of the term, remembering what was assigned to the resource at the
--     given point.
--   - If it's a read, check whether it's been assigned in this term.
--     If it has, replace it with the value which was assigned.
--     Otherwise, actually get the value.
--
-- And hey while we're at it, we can introduce concurrency, no? 
-- Is it possible to introduce concurrency? We don't know, given a M term, what
-- the next term is until we run it! We'll have to change the M term so that
-- everything is there statically... but we can't. We would have to introduce
-- a symbolic representation of actual Haskell... or just do the "compilation"
-- phase on-line! No, that still won't allow it; by the very definition of M,
-- we need to run each mget and mset in sequence! Ah, not quite true; the
-- mset terms are statically known, so we can indeed grab them and defer
-- them. But could we define M in such a way that concurrency is recovered
-- easily? 
-- Perhaps the best option is to just give an explicit Applicative instance
-- and wait for ApplicativeDo.
--
-- Or, what if instead of
--   Maybe range -> t
-- we have
--   Concurrently (Maybe range) -> Concurrently t
-- and then whenever we actually use the thing we `wait` for it.
-- Yeah, could work! Must replace the domain value in `MAt` with an
-- Async, though, am I right? Then when we runM, we wait for the
-- domain, use it to produce the range Async, and then pass it in order to
-- get the next one!
-- So the M terms would deal exclusively with Async things.
--   MPure wraps the value in an Async
--   MAt takes an domain and a continuation with an
--     Async (Maybe range) domain type and Async t range type
--   MAssign takes domain, Maybe Range, t
-- We only force the Async's ... when?
--
--
--

data M2' f t where
  M2Pure :: t -> M2' f t
  M2At
    :: (
       )
    => PartialFunction mtype access domain range
    -> domain
    -> (f (Maybe range) -> f t)
    -> M2' f t
  M2Assign
    :: (
       )
    => PartialFunction mtype ReadWrite domain range
    -> domain
    -> Maybe range
    -> t
    -> M2' f t

instance Functor f => Functor (M2' f) where
  fmap f m' = case m' of
    M2Pure x -> M2Pure $ f x
    M2At manifest x g -> M2At manifest x ((fmap . fmap) f g)
    M2Assign manifest x y next -> M2Assign manifest x y (f next)

type M2 f = Free (M2' f)

at2 :: Functor f => PartialFunction mtype access domain range -> domain -> M2 f (Maybe range)
at2 pf x = liftF (M2At pf x id)

-- | Convenient for feeding results of `at`s to other `at`s; no need to
--   pattern match on the Maybe; we do it for you.
at_2 :: Functor f => PartialFunction mtype access domain range -> Maybe domain -> M2 f (Maybe range)
at_2 pf x = case x of
  Just x' -> at2 pf x'
  Nothing -> return Nothing

assign2 :: Functor f => PartialFunction mtype ReadWrite domain range -> domain -> Maybe range -> M2 f ()
assign2 pf x y = liftF (M2Assign pf x y ())

-- Example case:
--
--   do x <- func1 `at` 0
--      y <- func2 `at` 1
--      return $ (+) <$> x <*> y
--
-- the two `at`s should run concurrently. How?
-- This is the actual structure:
--
--   M2At func1 0 (\x -> M2At func2 1 (\y -> M2Pure $ (+) <$> x <*> y))
--
-- Since we don't actually use x or y until the last term, we should not
-- demand them until we reach that point (but we should be sure to evaluate
-- them in full when we do reach that point, to avoid any lazy I/O).
--
-- Three cases in which we must force a result:
--   1. It's used as input to a get
--   2. It's used as input to a set (domain or range)
--   3. It's used in a pure function.
-- How could we possibly do this in a transparent way?
--
--

runConc :: M2' Concurrently (IO a) -> IO a
runConc m = case m of
    M2Pure x -> x
    M2At pf x next -> let rangeConcurrent = _
                      in  next rangeConcurrent
    M2Assign pf x y next -> next >> runPFSet' pf x y

runPFSet' = undefined

-- | The "target language" of M terms.
--   Describes, independently, a list of reads and writes which must be
--   performed.
--   Hm, maybe this hurts more than it helps. This is just a desugaring
--   of PartialFunctions into a bunch of manifest reads and writes, but
--   still must have the complexity of a monad (dependencies between terms
--   remain).
{-
data CoreM a = 

runM2 :: Functor f => M2 f a -> f (CoreM a)
runM2 term = iterM run term

  where

    run
      :: ( Functor f
         )
      => M2' f (CoreM f a)
      -> CoreM f a
    run m = case m of
      M2Pure corem -> corem
      M2At pf x next -> coreAt x 
      M2Assign pf x y next -> fmap (coreAssign x y) next

coreAssign = undefined

coreAt = undefined
-}
--
-- Should we allow arbitrary IO in our M terms? Should M be MonadIO???? Nah,
-- let's leave that out for now. The Manifest IO should be all you need.
--
-- Also: a non-terminating M term is useless; could we somehow rule this out?
-- Not in Haskell I think.
--
