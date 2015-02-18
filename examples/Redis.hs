{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import qualified Data.ByteString as BS
import Control.Monad.Trans.Reader
import Database.Redis
import Manifest.Manifest

data RedisManifest a = RedisManifest {
    connectInfo :: ConnectInfo
  , keyspace :: BS.ByteString
  }

{-
-- Problem: I don't think this guy is really a Monad or Applicative.
newtype CustomRedisTxMonad a = CRTM {
    unCRTM :: RedisTx (Queued a)
  } deriving (Functor)

instance Applicative CustomRedisTxMonad where
  pure = CRTM . pure . pure
  f <*> x = undefined

instance Monad CustomRedisTxMonad where
  return = CRTM . return . return
  x >>= k = undefined
-}

newtype CustomRedisMonad a = CRM {
    unCRM :: Redis (Either Reply a)
  } deriving (Functor)

instance Applicative CustomRedisMonad where
  pure = CRM . pure . pure
  f <*> x = CRM $ (<*>) <$> (unCRM f) <*> (unCRM x)

instance Monad CustomRedisMonad where
  return = CRM . return . return
  x >>= k = CRM $ do
    x' <- unCRM x
    case x' of
      Left y -> return $ Left y
      Right x'' -> unCRM $ k x''

data RedisManifestFailure
  = RedisNoConnection
  | RedisUnauthorized
  | RedisTransactionFailed
  | RedisSomeException

instance Manifest RedisManifest where

  --type ManifestMonad RedisManifest = ReaderT BS.ByteString CustomRedisTxMonad
  type ManifestMonad RedisManifest = ReaderT BS.ByteString CustomRedisMonad
  type PeculiarManifestFailure RedisManifest = RedisManifestFailure

  manifestRead redisManifest key = do
    keyspace <- ask
    let fullkey = BS.append keyspace key
    keyIsDefined <- exists fullkey
    case keyIsDefined of
      -- TODO not sure what this means.
      Left _ -> Nothing
      Right x -> if not x then return Nothing else smembers fullkey
    -- If we had a transactional guarantee, I'd feel a lot better about this
    -- call. Key may have been deleted by now.

  manifestWrite = undefined

  manifestRun r@(RedisManifest connectInfo keyspace) action = do
      let redisAction = unCRM (runReaderT action keyspace)
      -- TODO must catch exceptions.
      conn <- connect connectInfo
      outcome <- runRedis conn redisAction
      case outcome of
        Left _ -> return (Left RedisSomeException, r)
        Right x -> return (Right x, r)
      {-
      case outcome of
        TxAborted -> return (Left RedisTransactionFailed, r)
        TxError _ -> return (Left RedisSomeException, r)
        TxSuccess x -> return (Right x, r)
      -}
