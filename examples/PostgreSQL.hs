{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Data.TypeNat.Vect
import Data.Proxy
import Manifest.Manifest
import Manifest.PostgreSQL

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
  type ManifestValueLength (Digest, Salt) = S (S Z)
  manifestibleValueDump (digest, salt) = VCons digest (VCons salt VNil)
  manifestibleValuePull bs = case bs of
    VCons digest (VCons salt VNil) -> Just (digest, salt)
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

connectInfo = ConnectInfo "localhost" (fromIntegral 5432) "" "" "alex"

myPostgres :: PostgreSQL (WithSaltedDigest User)
myPostgres = postgresql connectInfo "test"

alex = User "alex"
john = User "john"
richard = User "richard"
alex' = WithSaltedDigest alex "mypassword" "123"
john' = WithSaltedDigest john "hispassword" "321"
richard' = WithSaltedDigest richard "supersecret" "231"
alexEmail = WithEmail alex "alex@vieth.com"
richardEmail = WithEmail richard "richard@carstone.com"

main = do
  let action = do {
      --write alex' (Proxy :: Proxy PostgreSQL);
      --write john' (Proxy :: Proxy PostgreSQL);
      x <- read alex (Proxy :: Proxy (PostgreSQL (WithSaltedDigest User)));
      y <- read richard (Proxy :: Proxy (PostgreSQL (WithSaltedDigest User)));
      return (x, y)
    }
  let action2 = read john (Proxy :: Proxy (PostgreSQL (WithSaltedDigest User)))
  let action3 = write richard' (Proxy :: Proxy PostgreSQL)
  (Right x, _) <- manifest myPostgres action
  --(Right y, _) <- manifest myPostgres action2
  --(Right z, _) <- manifest myPostgres action3
  print x
  --print y
  --print z
{-
  (Right x, _) <- manifest tmEmail $ do
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
