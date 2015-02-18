{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Manifest.Manifest (

    read
  , write

  , ManifestRead
  , ManifestWrite

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
import Control.Applicative
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
  -> ManifestMonad manifest (Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a))
read key _ = do
    eitherFailureSuccess <- manifestRead (Proxy :: Proxy manifest) bkey
    return $ ifElse eitherFailureSuccess ifFailure ifSuccess

  where

    bkey = manifestibleKeyDump key

    ifFailure :: ManifestReadFailure manifest -> Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a)
    ifFailure = Left

    ifSuccess :: Maybe [BS.ByteString] -> Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a)
    ifSuccess maybeRead = inCase maybeRead ifRead ifNoRead

    ifRead :: [BS.ByteString] -> Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a)
    ifRead bs = inCase (manifestiblePull (Proxy :: Proxy a) (bkey, bs)) ifPulled ifNotPulled

    ifNoRead :: Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a)
    ifNoRead = Right NotFound

    ifPulled :: a -> Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a)
    ifPulled = Right . Found

    ifNotPulled :: Either (ManifestReadFailure manifest) (ManifestRead (manifest a) a)
    ifNotPulled = Right ReadError

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
  -> ManifestMonad manifest (Either (ManifestWriteFailure manifest) (ManifestWrite (manifest a) a))
write x _ = do
    maybeFailure <- manifestWrite (Proxy :: Proxy manifest) bkey bvalue
    return $ inCase maybeFailure ifFailure ifSuccess

  where

    bkey = manifestibleKeyDump (manifestibleKey x)
    bvalue = manifestibleValueDump (manifestibleValue x)

    ifFailure :: ManifestWriteFailure manifest -> Either (ManifestWriteFailure manifest) (ManifestWrite (manifest a) a)
    ifFailure = Left

    ifSuccess :: Either (ManifestWriteFailure manifest) (ManifestWrite (manifest a) a)
    ifSuccess = Right $ Written x

-- | Witness that some value was read from some Manifest.
data ManifestRead manifest a = Found a | NotFound | ReadError
  deriving (Show)

-- | Witness that some value was written to some Manifest.
data ManifestWrite manifest a = Written a
  deriving (Show)

data ManifestFailure manifest
  = ReadFailure (ManifestReadFailure manifest)
  | WriteFailure (ManifestWriteFailure manifest)

-- | Indicates that some type can be used as a manifest.
class Manifest manifest where

  type ManifestMonad manifest :: * -> *
  type ManifestReadFailure manifest :: *
  type ManifestWriteFailure manifest :: *

  manifestRead
    :: u manifest
    -> BS.ByteString
    -> ManifestMonad manifest (Either (ManifestReadFailure manifest) (Maybe [BS.ByteString]))
  -- ^ Try to read from a Manifest. If successful, Just indicates that a value
  --   was found, and Nothing indicates that no value was found.

  manifestWrite
    :: u manifest
    -> BS.ByteString
    -> [BS.ByteString]
    -> ManifestMonad manifest (Maybe (ManifestWriteFailure manifest))
  -- ^ Try to write to a Manifest.

  manifestRun
    :: manifest a
    -> ManifestMonad manifest a
    -> IO (Either (ManifestFailure manifest) (a, manifest a))
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
