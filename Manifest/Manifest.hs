{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Manifest.Manifest (

    read
  , write
  , manifest

  , ManifestRead
  , ManifestWrite
  , ManifestFailure(..)

  , Manifest(..)

  -- TODO should be able to pull these from some other package
  , Nat(..)
  , Vect(..)
  , vectToList
  , listToVect

  , ManifestKey(..)
  , ManifestValue(..)
  , Manifestible
  , ManifestibleKey
  , ManifestibleValue
  , manifestibleKey
  , manifestibleValue
  , manifestibleFactorization

  ) where

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Control.RichConditional
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Proxy

-- TODO move these guys; can't find an implementation on Hackage.
-- Perhaps ask the IRC channel?
data Nat = Z | S Nat
data Vect :: * -> Nat -> * where
  VNil :: Vect a Z
  VCons :: a -> Vect a n -> Vect a (S n)

showVect :: Show a => Vect a l -> String
showVect VNil = "VNil"
showVect (VCons x xs) = show x ++ " , " ++ showVect xs

vectToList :: Vect a n -> [a]
vectToList v = case v of
  VNil -> []
  VCons x xs -> x : vectToList xs

{-
class ListToVect n where
  listToVect' :: [a] -> Maybe (Vect a n)

instance ListToVect Z where
  listToVect' [] = Just VNil
  listToVect' _ = Nothing

instance ListToVect n => ListToVect (S n) where
  listToVect' [] = Nothing
  listToVect' (x : xs) = VCons x <$> listToVect' xs 

listToVect :: ListToVect l => [a] -> u l -> Maybe (Vect a l)
listToVect xs _ = listToVect' xs
--listToVect :: [a] -> u (l :: Nat) -> Maybe (Vect a l)
--listToVect xs proxy = listToVect' xs

data ListToVect2 :: Nat -> * where
  MkListToVect2 :: ([b] -> Maybe (Vect b n)) -> ListToVect2 n

listToVectNil :: ListToVect2 Z
listToVectNil = MkListToVect2 $ \x -> case x of
  [] -> Just VNil
  _ -> Nothing

listToVectCons :: ListToVect2 n -> ListToVect2 (S n)
listToVectCons (MkListToVect2 mk) = MkListToVect2 $ \x -> case x of
  [] -> Nothing
  (y : ys) -> VCons y <$> mk ys

data Vect2 :: Nat -> * -> * where
  VNil2 :: Vect2 Z a
  VCons2 :: a -> Vect2 n a -> Vect2 (S n) a

class ListTo v a where
  listTo :: [a] -> Maybe (v a)

instance ListTo (Vect2 Z) a where
  listTo x = case x of
    [] -> Just VNil2
    _ -> Nothing

instance ListTo (Vect2 n) a => ListTo (Vect2 (S n)) a where
  listTo xs = case xs of
    [] -> Nothing
    (y : ys) -> VCons2 y <$> (listTo ys :: Maybe (Vect2 n a))
-}

-- Focus: here's what we really want.
-- Somehow we have to switch based on the type. Can't the GADT above help
-- us? No I think we just need typeclasses... I don't know of any other
-- way to have the type direct the values in Haskell.
--list2Vect :: [a] -> u (n :: Nat) -> Maybe (Vect2 n a)
--list2Vect = undefined

-- Idea: recursion principle for Nat on the types.
-- Then maybe we can do
--
--   type MaybeVect a n = Maybe (Vect a n)
--
--   listToVect :: [a] -> MaybeVect a n
--   listToVect = induction (Just VNil) (
--
class IsNat (n :: Nat) where
  induction :: (a Z) -> (forall m . a m -> a (S m)) -> a n

instance IsNat Z where
  induction ifZ _ = ifZ

instance IsNat n => IsNat (S n) where
  induction ifZ ifS = ifS (induction ifZ ifS)
-- So there we have it, but how can we use it?

newtype MaybeVect a n = MV {
    unMV :: Maybe (Vect a n, [a])
  }

listToVect :: IsNat n => [a] -> u n -> Maybe (Vect a n)
listToVect xs _ = fst <$> unMV (listToVect' xs)

listToVect' :: IsNat n => [a] -> MaybeVect a n
listToVect' xs = induction (MV $ Just (VNil, xs)) inductive
  where
    inductive :: MaybeVect a m -> MaybeVect a (S m)
    inductive (MV Nothing) = MV Nothing
    inductive (MV (Just (v, xs))) = case xs of
      [] -> MV Nothing
      (y : ys) -> MV (Just (VCons y v, ys))

-- AMAZING! It works :D Best thing I ever did at 3:30 in the morning.
-- Ok, should remember exactly WHY this was needed. Had to explain to GHC
-- that any Nat value was good for a vectToList for vectors of that length.
--
-- Problem: we don't detect lists which are too long! How to remedy this?
-- Every inductive case has to be the same, so we can't detect it. Better
-- to work top-down: detect at base case.

listToVect'' :: IsNat n => [a] -> MaybeVect a n
listToVect'' xs = induction base inductive
  where
    base :: MaybeVect a Z
    base 

read
  :: forall a k manifest u l .
     ( Manifest manifest
     , Monad (ManifestMonad manifest)
     , Manifestible a
     , k ~ ManifestibleKey a
     , ManifestKey k
     , ManifestValue (ManifestibleValue a)
     , l ~ ManifestValueLength (ManifestibleValue a)
     , IsNat l
     )
  => k
  -> u (manifest a)
  -- ^ Need a proxy to fix the manifest and value types.
  -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
read key _ = do
    maybeBytestrings <- lift $ manifestRead (Proxy :: Proxy manifest) (Proxy :: Proxy l) bkey
    inCase maybeBytestrings found notFound

  where

    bkey = manifestibleKeyDump key

    readBytestrings :: Vect BS.ByteString l -> Maybe a
    readBytestrings bss = manifestiblePull (Proxy :: Proxy a) (bkey, bss)

    found :: Vect BS.ByteString l -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
    found x =
      let maybeRead = readBytestrings x
      in  inCase maybeRead readOK readNotOK

    notFound :: ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
    notFound = return NotFound

    readOK :: a -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
    readOK x = return (Found x)

    readNotOK :: ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
    readNotOK = throwE ReadFailure

write
  :: forall a manifest u l .
     ( Manifest manifest
     , Monad (ManifestMonad manifest)
     , Manifestible a
     , ManifestKey (ManifestibleKey a)
     , ManifestValue (ManifestibleValue a)
     , l ~ ManifestValueLength (ManifestibleValue a)
     , IsNat l
     )
  => a
  -> u manifest
  -- ^ Need a proxy to fix the manifest type.
  -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestWrite (manifest a) a)
write x _ = do
    () <- lift $ manifestWrite (Proxy :: Proxy manifest) (Proxy :: Proxy l) bkey bvalue
    return $ Written x

  where

    bkey = manifestibleKeyDump (manifestibleKey x)
    bvalue = manifestibleValueDump (manifestibleValue x)

manifest
  :: ( Manifest manifest
     , Monad (ManifestMonad manifest)
     )
  => manifest a
  -> ExceptT GeneralManifestFailure (ManifestMonad manifest) t
  -> IO (Either (ManifestFailure manifest) t, manifest a)
manifest m term = do
    (outcome, m') <- manifestRun m (runExceptT term)
    case outcome of
      Left peculiarFailure -> return (Left (PeculiarFailure peculiarFailure), m')
      Right outcome' -> case outcome' of
        Left generalFailure -> return (Left (GeneralFailure generalFailure), m')
        Right x -> return (Right x, m')

  {-
    ifElse outcome (ifPeculiarFailure m') (ifNoPeculiarFailure m')

  where

    ifGeneralFailure :: GeneralManifestFailure -> IO (Either (ManifestFailure manifest) (a, manifest a))
    --ifGeneralFailure = GeneralFailure
    ifGeneralFailure = undefined

    ifNoGeneralFailure :: Either (PeculiarManifestFailure manifest) (a, manifest a) -> IO (Either (ManifestFailure manifest) (a, manifest a))
    ifNoGeneralFailure = undefined
  -}

-- | Witness that some value was read from some Manifest.
data ManifestRead manifest a = Found a | NotFound
  deriving (Show)

-- | Witness that some value was written to some Manifest.
data ManifestWrite manifest a = Written a
  deriving (Show)

data GeneralManifestFailure = ReadFailure
  deriving (Show)

data ManifestFailure manifest
  = GeneralFailure GeneralManifestFailure
  -- ^ Indicates that the data found for some ManifestKey was not
  --   parsed back into the expected value. It's a failure that EVERY
  --   Manifest may encounter.
  | PeculiarFailure (PeculiarManifestFailure manifest)

instance Show (ManifestFailure manifest) where
  show failure = case failure of
    GeneralFailure _ -> "General failure"
    PeculiarFailure _ -> "Peculiar failure"

-- | Indicates that some type can be used as a manifest.
class Manifest manifest where

  type ManifestMonad manifest :: * -> *
  type PeculiarManifestFailure manifest :: *

  manifestRead
    :: ( IsNat n
       )
    => u manifest
    -> u' n
    -> BS.ByteString
    -> ManifestMonad manifest (Maybe (Vect BS.ByteString n))
  -- ^ Try to read from a Manifest. Nothing indicates not found.
  --   Failure can be expressed by a suitable ManifestMonad.

  manifestWrite
    :: ( IsNat n
       )
    => u manifest
    -> u' n
    -> BS.ByteString
    -> Vect BS.ByteString n
    -> ManifestMonad manifest ()
  -- ^ Try to write to a Manifest.
  --   Failure can be expressed by a suitable ManifestMonad.

  manifestRun
    :: manifest a
    -> ManifestMonad manifest t
    -> IO (Either (PeculiarManifestFailure manifest) t, manifest a)
  -- ^ Run a manifest computation under a given manifest. This is the point
  --   at which a particular Manifest value comes into play and gives
  --   meaning to the Manifest term (read and write call).
  --   Must be exception safe! If a Right is given, then everything went
  --   OK.

-- | Indicate that values of some type can be used as keys into a Manifest.
--   This is just serialization and deserialization to and from ByteString.
class ManifestKey k where
  manifestibleKeyDump :: k -> BS.ByteString
  manifestibleKeyPull :: BS.ByteString -> Maybe k
  -- ^ Must have that
  --
  --     manifestibleKeyPull . manifestibleKeyDump = Just

-- | Indicate that values of some type can be used as values in a Manifest.
--   This is just serialization and deserialization to and from list of
--   ByteStrings.
class ManifestValue v where
  type ManifestValueLength v :: Nat
  manifestibleValueDump :: v -> Vect BS.ByteString (ManifestValueLength v)
  manifestibleValuePull :: Vect BS.ByteString (ManifestValueLength v) -> Maybe v
  -- ^ Must have that
  --
  --     manifestibleValuePull . manifestibleValueDump = Just

instance ManifestKey BS.ByteString where
  manifestibleKeyDump = id
  manifestibleKeyPull = Just

instance ManifestValue BS.ByteString where
  type ManifestValueLength BS.ByteString = S Z
  manifestibleValueDump bs = VCons bs VNil
  manifestibleValuePull bss = case bss of
    VCons bs VNil -> Just bs
    _ -> Nothing

-- | A value which can be used with a Manifest.
--   It must factor into a key and value, as determined by the type functions
--     ManifestibleKey
--     ManifestibleValue
--   An instance declares that any value of type ManifestibleKey a, which is
--   itself a ManifestKey instance, can be used to read and write values of
--   type a into and out of some Manifest.
--
--   TBD seems right to demand hat a have an Eq and that
--
--     x :: a == y :: a <=> manifestibleKey x = manifestibleKey y
class Manifestible a where

  type ManifestibleKey a :: *
  type ManifestibleValue a :: *

  manifestibleKey :: a -> ManifestibleKey a
  manifestibleValue :: a -> ManifestibleValue a
  manifestibleFactorization :: ManifestibleKey a -> ManifestibleValue a -> a
  -- ^ Show that the type a really does factor into the two types given.
  --   Must have that
  --
  --     manifestibleFactorization (manifestibleKey x) (manifestibleValue x) = x

  manifestiblePull
    :: ( ManifestKey (ManifestibleKey a)
       , ManifestValue (ManifestibleValue a)
       )
    => u a
    -> (BS.ByteString, Vect BS.ByteString (ManifestValueLength (ManifestibleValue a)))
    -> Maybe a
  manifestiblePull proxy (k, v) =
        manifestibleFactorization
    <$> manifestibleKeyPull k
    <*> manifestibleValuePull v

  manifestibleDump
    :: ( ManifestKey (ManifestibleKey a)
       , ManifestValue (ManifestibleValue a)
       )
    => a
    -> (BS.ByteString, Vect BS.ByteString (ManifestValueLength (ManifestibleValue a)))
  manifestibleDump x =
    let mkey = manifestibleKey x
        mval = manifestibleValue x
    in  ( manifestibleKeyDump mkey
        , manifestibleValueDump mval
        )

  -- Theorem:
  --   manifestiblePull . manifestibleDump = Just
  --
  -- Proof:
  --     manifestiblePull (manifestibleDump x)
  --   = manifestiblePull (manifestibleKeyDump Proxy (manifestibleKey x), manifestibleValueDump Proxy (manifestibleValue x))
  --   = manifestibleFactorization <$> Just (manifestibleKey x) <*> Just (manifestibleValue x)
  --   = x
  --
  -- Obviously, this relies on a correct implementation of the class methods,
  -- which obeys the asserted laws.
