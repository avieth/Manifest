{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Data.List (find)
import Control.Monad.Trans.State
import Data.Proxy
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

data TestManifest a = TM [(BS.ByteString, [BS.ByteString])]

emptyTM :: TestManifest a
emptyTM = TM []

data TestManifestReadFailure
data TestManifestWriteFailure = AlreadyPresent
  deriving (Show)

instance Manifest TestManifest where
  type ManifestMonad TestManifest = State [(BS.ByteString, [BS.ByteString])]
  -- ^ Problem: the phantom type in the Manifest means we can't say
  -- the state in our monad is a TestManifest a.
  type ManifestReadFailure TestManifest = TestManifestReadFailure
  type ManifestWriteFailure TestManifest = TestManifestWriteFailure
  manifestRead proxy bs = do
      list <- get
      return $ case find ((==) bs . fst) list of
        Nothing -> Right Nothing
        Just (_, bss) -> Right (Just bss)
  manifestWrite proxy bs bss = do
      list <- get
      case find ((==) bs . fst) list of
        Just _ -> return $ Just AlreadyPresent
        Nothing -> do
          put ((bs, bss) : list)
          return Nothing
  manifestRun (TM list) action =
      let (x, s) = runState action list
      in  return $ Right (x, TM s)

alex = User "alex"
john = User "john"
richard = User "richard"
alex' = WithSaltedDigest alex "mypassword" "123"
john' = WithSaltedDigest john "hispassword" "321"
alexEmail = WithEmail alex "alex@vieth.com"
richardEmail = WithEmail richard "richard@carstone.com"

main = do
  let tmSaltedDigest = emptyTM
  let tmEmail = emptyTM
  let action = do {
      write alex' (Proxy :: Proxy TestManifest);
      write john' (Proxy :: Proxy TestManifest);
      read alex (Proxy :: Proxy (TestManifest (WithSaltedDigest User)))
    }
  Right (x, _) <- manifestRun tmSaltedDigest action
  print x
{-
  Right (x, _) <- manifestRun tmEmail $ do
    write alex'
    --write john'
    --read alex
  print x

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
-}
