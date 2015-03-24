{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Manifest.ManifestException (

    ManifestException
  , manifestExceptionToException
  , manifestExceptionFromException

  ) where

import Data.Typeable
import Control.Exception

-- | Container for any exception raised by a Manifest.
--   Particular Manifest instances should define particular exceptions, and
--   make them "subexceptions" of this one in the canonical way: an Exception
--   instance with
--     toException = manifestExceptionToException
--     fromException = manifestExceptionFromException
data ManifestException where
  ManifestException :: Exception e => e -> ManifestException

deriving instance Typeable ManifestException
instance Show ManifestException where
  show (ManifestException x) = show x
instance Exception ManifestException

manifestExceptionToException :: Exception e => e -> SomeException
manifestExceptionToException = toException . ManifestException

manifestExceptionFromException :: Exception e => SomeException -> Maybe e
manifestExceptionFromException x = do
    ManifestException a <- fromException x
    cast a
