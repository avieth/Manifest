{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Manifest.Manifest (

    read
  , write
  , manifest

  , ManifestRead
  , ManifestWrite
  , ManifestFailure
  , PeculiarManifestFailure(..)

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

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Control.RichConditional
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Proxy

read
  :: forall a k manifest u .
     ( Manifest manifest
     , Monad (ManifestMonad manifest)
     , Manifestible a
     , k ~ ManifestibleKey a
     , ManifestKey k
     , ManifestValue (ManifestibleValue a)
     )
  => k
  -> u (manifest a)
  -- ^ Need a proxy to fix the manifest and value types.
  -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
read key _ = do
    maybeBytestrings <- lift $ manifestRead (Proxy :: Proxy manifest) bkey
    inCase maybeBytestrings found notFound

  where

    bkey = manifestibleKeyDump key

    readBytestrings :: [BS.ByteString] -> Maybe a
    readBytestrings bss = manifestiblePull (Proxy :: Proxy a) (bkey, bss)

    found :: [BS.ByteString] -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestRead (manifest a) a)
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
  :: forall a manifest u .
     ( Manifest manifest
     , Monad (ManifestMonad manifest)
     , Manifestible a
     , ManifestKey (ManifestibleKey a)
     , ManifestValue (ManifestibleValue a)
     )
  => a
  -> u manifest
  -- ^ Need a proxy to fix the manifest type.
  -> ExceptT GeneralManifestFailure (ManifestMonad manifest) (ManifestWrite (manifest a) a)
write x _ = do
    () <- lift $ manifestWrite (Proxy :: Proxy manifest) bkey bvalue
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

data PeculiarManifestFailure manifest
  = PeculiarReadFailure (ManifestReadFailure manifest)
  -- ^ A read failure peculiar to the given Manifest.
  | PeculiarWriteFailure (ManifestWriteFailure manifest)
  -- ^ A write failure peculiar to the given Manifest.

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
  type ManifestReadFailure manifest :: *
  type ManifestWriteFailure manifest :: *

  manifestRead
    :: u manifest
    -> BS.ByteString
    -> ManifestMonad manifest (Maybe [BS.ByteString])
  -- ^ Try to read from a Manifest. Nothing indicates not found.
  --   Failure can be expressed by a suitable ManifestMonad.

  manifestWrite
    :: u manifest
    -> BS.ByteString
    -> [BS.ByteString]
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
  manifestibleValueDump :: v -> [BS.ByteString]
  manifestibleValuePull :: [BS.ByteString] -> Maybe v
  -- ^ Must have that
  --
  --     manifestibleValuePull . manifestibleValueDump = Just

instance ManifestKey BS.ByteString where
  manifestibleKeyDump = id
  manifestibleKeyPull = Just

instance ManifestValue BS.ByteString where
  manifestibleValueDump = pure
  manifestibleValuePull bss = case bss of
    [bs] -> Just bs
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
    -> (BS.ByteString, [BS.ByteString])
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
    -> (BS.ByteString, [BS.ByteString])
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
