{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Manifest.SQLite (

    SQLiteManifest
  , sqlite

  , TextSerializable(..)

  ) where

import qualified Data.DependentMap as DM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.String
import Data.Typeable
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Database.SQLite.Simple

import Manifest.Manifest
import Manifest.Resource
import Manifest.FType

-- | A database file name describes the SQLite manifest.
--   Empty string or ":memory:" gives an in-memory database which is lost
--   once the associated resource is closed. You probably don't want that.
data SQLiteDescriptor = SQLD String
  deriving (Eq, Ord, Typeable)

instance ResourceDescriptor SQLiteDescriptor where
  type ResourceType SQLiteDescriptor = Connection
  acquireResource (SQLD str) = do
      conn <- liftIO $ open str
      liftIO $ execute_ conn "BEGIN TRANSACTION"
      return $ Resource conn commit rollback release
    where
      rollback conn = do
        liftIO $ execute_ conn "ROLLBACK TRANSACTION"
      commit conn = do
        liftIO $ execute_ conn "COMMIT TRANSACTION"
      release conn = do
        liftIO $ close conn

-- | An SQLiteManifest. Assumes the provided SQLiteDescriptor picks out a
--   database which has a table of the specified name, with two columns as
--   specified. First column must be unique, and in case the FType if
--   FInjective, the other must be unique as well.
data SQLiteManifest :: FType -> Access -> * -> * -> * where
  SQLiteManifest
    :: SQLiteDescriptor
    -> B8.ByteString
    -- ^ Name of the table to use.
    -> B8.ByteString
    -- ^ Column name for domain
    -> B8.ByteString
    -- ^ Column name for range
    -> SQLiteManifest ftype access domain range

sqlite :: String -> B8.ByteString -> B8.ByteString -> B8.ByteString -> SQLiteManifest ftype access domain range
sqlite file = SQLiteManifest (SQLD file)

class TextSerializable a where
  textSerialize :: a -> T.Text
  textDeserialize :: T.Text -> Maybe a

instance TextSerializable T.Text where
  textSerialize = id
  textDeserialize = Just

instance Manifest SQLiteManifest where
  type ManifestResourceDescriptor SQLiteManifest ftype access domain range = SQLiteDescriptor
  resourceDescriptor (SQLiteManifest sqld _ _ _) = sqld
  type ManifestDomainType SQLiteManifest domain range = T.Text
  type ManifestRangeType SQLiteManifest domain range = T.Text
  type ManifestDomainConstraint SQLiteManifest domain range = TextSerializable domain
  type ManifestRangeConstraint SQLiteManifest domain range = TextSerializable range
  mdomainDump _ = textSerialize
  mrangePull _ = textDeserialize

instance ManifestRead SQLiteManifest where
  mget (SQLiteManifest _ tableName domainName rangeName) conn key = do
    let statement = [ "SELECT \""
                    , rangeName
                    , "\" FROM "
                    , tableName
                    , " WHERE \""
                    , domainName
                    , "\"=?"
                    ]
    let queryString = fromString . B8.unpack . BS.concat $ statement
    -- ^ SQLite simple doesn't allow query substitution for table name and
    --   where clause simultaneously :(
    y <- liftIO $ query conn queryString (Only key)
    return $ case y :: [Only T.Text] of
      [] -> Nothing
      (y' : _) -> Just (fromOnly y')
      -- ^ TBD should we warn in case more than one row is found?

instance ManifestWrite SQLiteManifest where
  mrangeDump _ = textSerialize
  mset (SQLiteManifest _ tableName domainName rangeName) conn key value = case value of
    Nothing -> do
        let statement = [ "DELETE FROM "
                        , tableName
                        , " WHERE \""
                        , domainName
                        , "\"=?"
                        ]
        let queryString = fromString . B8.unpack . BS.concat $ statement
        liftIO $ execute conn queryString (Only key)
    Just value -> do
        let statement = [ "INSERT OR REPLACE INTO "
                        , tableName
                        , "(\""
                        , domainName
                        , "\", \""
                        , rangeName
                        , "\") VALUES (?, ?)"
                        ]
        let queryString = fromString . B8.unpack . BS.concat $ statement
        liftIO $ execute conn queryString (key, value)

instance ManifestInjective SQLiteManifest where
  minvert (SQLiteManifest sqld tableName domainName rangeName) =
    SQLiteManifest sqld tableName rangeName domainName
