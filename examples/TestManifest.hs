{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.IORef
import Manifest.Manifest

data User = User BS.ByteString
  deriving (Show)

type Digest = BS.ByteString
type Salt = BS.ByteString

data WithSaltedDigest a = WithSaltedDigest a Digest Salt
  deriving (Show)

type Email = BS.ByteString

data WithEmail a = WithEmail a Email
  deriving (Show)

instance ManifestKey User where
  manifestibleKeyDump (User b) = b
  manifestibleKeyPull = Just . User

instance ManifestValue (Digest, Salt) where
  manifestibleValueDump (digest, salt) = [digest, salt]
  manifestibleValuePull bs = case bs of
    [digest, salt] -> Just (digest, salt)
    _ -> Nothing

instance Manifestible (WithSaltedDigest a) where
  type ManifestibleKey (WithSaltedDigest a) = a
  type ManifestibleValue (WithSaltedDigest a) = (Digest, Salt)
  manifestibleKey (WithSaltedDigest x _ _) = x
  manifestibleValue (WithSaltedDigest _ digest salt) = (digest, salt)
  manifestibleFactorization key (digest, salt) = WithSaltedDigest key digest salt

instance Manifestible (WithEmail a) where
  type ManifestibleKey (WithEmail a) = a
  type ManifestibleValue (WithEmail a) = Email
  manifestibleKey (WithEmail x _) = x
  manifestibleValue (WithEmail _ x) = x
  manifestibleFactorization = WithEmail

data TestManifest a = TM (IORef [(BS.ByteString, [BS.ByteString])])

emptyTM :: IO (TestManifest a)
emptyTM = do
  ref <- newIORef []
  return $ TM ref

data TestManifestReadFailure
data TestManifestWriteFailure = AlreadyPresent

instance Manifest TestManifest where
  type ManifestReadFailure TestManifest = TestManifestReadFailure
  type ManifestWriteFailure TestManifest = TestManifestWriteFailure
  manifestRead (TM ref) bs = do
      list <- readIORef ref 
      return $ case find ((==) bs . fst) list of
        Nothing -> Right Nothing
        Just (_, bss) -> Right (Just bss)
  manifestWrite (TM ref) bs bss = do
      list <- readIORef ref
      case find ((==) bs . fst) list of
        Just _ -> return $ Just AlreadyPresent
        Nothing -> do
          writeIORef ref ((bs, bss) : list)
          return Nothing

alex = User "alex"
john = User "john"
richard = User "richard"
alex' = WithSaltedDigest alex "mypassword" "123"
john' = WithSaltedDigest john "hispassword" "321"
alexEmail = WithEmail alex "alex@vieth.com"
richardEmail = WithEmail richard "richard@carstone.com"

main = do
  tmSaltedDigest <- emptyTM
  tmEmail <- emptyTM
  write alex' tmSaltedDigest
  write richardEmail tmEmail
  -- At this point we restrict tm to the type
  --   TestManifest (WithSaltedDigest User)
  -- so that uncommenting the next line will make the program ill-typed.
  write alexEmail tmEmail
  write john' tmSaltedDigest
  Right x <- read alex tmSaltedDigest
  Right y <- read john tmSaltedDigest
  Right z <- read richard tmSaltedDigest
  Right z' <- read richard tmEmail
  print x
  print y
  print z
  print z'
