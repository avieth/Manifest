{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Manifest.Manifest (

    mget
  , mput
  , mdel
  , manifest

  , ManifestRead
  , ManifestWrite
  , ManifestDelete
  , ManifestFailure(..)

  , Manifest(..)

  , ManifestKey(..)
  , ManifestValue(..)
  , Manifestible
  , ManifestibleKey
  , ManifestibleValue
  , manifestibleKey
  , manifestibleValue
  , manifestibleFactorization

  ) where

import qualified Data.ByteString as BS
import Control.RichConditional
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Proxy
import Data.TypeNat.Vect

mget
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
mget key _ = do
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

mput
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
mput x _ = do
    () <- lift $ manifestWrite (Proxy :: Proxy manifest) (Proxy :: Proxy l) bkey bvalue
    return $ Written x

  where

    bkey = manifestibleKeyDump (manifestibleKey x)
    bvalue = manifestibleValueDump (manifestibleValue x)

mdel
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
  -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestDelete (manifest a) a)
mdel key _ = do
    maybeBytestrings <- lift $ manifestDelete (Proxy :: Proxy manifest) (Proxy :: Proxy l) bkey
    inCase maybeBytestrings found notFound

  where

    bkey = manifestibleKeyDump key

    readBytestrings :: Vect BS.ByteString l -> Maybe a
    readBytestrings bss = manifestiblePull (Proxy :: Proxy a) (bkey, bss)

    found :: Vect BS.ByteString l -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestDelete  (manifest a) a)
    found x =
      let maybeRead = readBytestrings x
      in  inCase maybeRead readOK readNotOK

    notFound :: ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestDelete (manifest a) a)
    notFound = return NothingToDelete

    readOK :: a -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestDelete (manifest a) a)
    readOK x = return (Deleted x)

    readNotOK :: ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestDelete (manifest a) a)
    readNotOK = throwE ReadFailure


-- | Run a manifest term (such as mput, mget, or any sequencing of these).
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

-- | Witness that some value was read from some Manifest, or not found.
data ManifestRead manifest a = Found a | NotFound
  deriving (Show)

instance PartialIf (ManifestRead manifest a) a where
  indicate mr = case mr of
    Found x -> Just x
    NotFound -> Nothing

-- | Witness that some value was written to some Manifest.
data ManifestWrite manifest a = Written a
  deriving (Show)

-- | Witness that some value was deleted from a Manifest, or not found.
data ManifestDelete manifest a = Deleted a | NothingToDelete
  deriving (Show)

instance PartialIf (ManifestDelete manifest a) a where
  indicate mr = case mr of
    Deleted x -> Just x
    NothingToDelete -> Nothing

-- | Failure reasons which are relevant to any Manifest.
--   There's just one: could not read the value, meaning its Manifestible
--   instance is wrong.
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

  manifestDelete
    :: ( IsNat n
       )
    => u manifest
    -> u' n
    -> BS.ByteString
    -> ManifestMonad manifest (Maybe (Vect BS.ByteString n))
  -- ^ If a thing is deleted successfully, must return the value corresponding
  --   to the key; otherwise, Nothing (meaning no problems, just that nothing
  --   was deleted).

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
