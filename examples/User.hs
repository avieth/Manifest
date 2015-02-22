{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.State
import Data.Proxy
import Data.TypeNat.Vect
import Manifest.Manifest
import Manifest.PureManifest

data User = User BS.ByteString
  deriving (Show)

type Email = BS.ByteString

data WithEmail a = WithEmail a Email
  deriving (Show)

email :: WithEmail a -> Email
email (WithEmail _ x) = x

instance ManifestKey User where
  manifestibleKeyDump (User b) = b
  manifestibleKeyPull = Just . User

instance Manifestible (WithEmail a) where
  type ManifestibleKey (WithEmail a) = a
  type ManifestibleValue (WithEmail a) = Email
  manifestibleKey (WithEmail x _) = x
  manifestibleValue (WithEmail _ x) = x
  manifestibleFactorization = WithEmail

ada = User "ada"
richard = User "richard"
adaEmail = WithEmail ada "ada@clare.com"
richardEmail = WithEmail richard "richard@carstone.com"

main = do
  let emailManifest = emptyPM
  let writeem = do {
      mput adaEmail (Proxy :: Proxy PureManifest);
      mput richardEmail  (Proxy :: Proxy PureManifest);
    }
  let readem = do {
      adasEmail <- mget ada (Proxy :: Proxy (PureManifest (WithEmail User)));
      richardsEmail <- mget richard (Proxy :: Proxy (PureManifest (WithEmail User)));
      return (email <$> adasEmail, email <$> richardsEmail)
    }
  (Right x, emailManifest) <- manifest emailManifest writeem
  (Right y, _) <- manifest emailManifest readem
  print y
