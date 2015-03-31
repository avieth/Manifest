{-|
Module      : Manifest.SQLite
Description : An SQLite-backed Manifest.
copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Manifest.SQLite (

    SQLiteManifestSingle
  , sqliteSingle

  , SQLiteManifestMultiple
  , sqliteMultiple

  , TextSerializable(..)

  ) where

import qualified Data.DependentMap as DM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.String
import Data.Typeable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Exception
import Database.SQLite.Simple
import Manifest.Manifest
import Manifest.ManifestException
import Manifest.Resource

-- | A database file name describes the SQLite manifest.
--   Empty string or ":memory:" gives an in-memory database which is lost
--   once the associated resource is closed. You probably don't want that.
data SQLiteDescriptor = SQLD String
  deriving (Eq, Ord, Typeable)

instance ResourceDescriptor SQLiteDescriptor where
  type ResourceType SQLiteDescriptor = Connection
  acquireResource (SQLD str) = do
      conn <- open str
      beginTransaction conn
      return $ Resource conn commit rollback release
    where
      beginTransaction conn = execute_ conn "BEGIN TRANSACTION"
      rollback conn = execute_ conn "ROLLBACK TRANSACTION"
      commit conn = execute_ conn "COMMIT TRANSACTION"
      release = close

-- | An SQLiteManifest. Assumes the provided SQLiteDescriptor picks out a
--   database which has a table of the specified name, with two columns as
--   specified.
data SQLiteManifestSingle :: Access -> * -> * -> * where
  SQLiteManifestSingle
    :: SQLiteDescriptor
    -> B8.ByteString
    -- ^ Name of the table to use.
    -> B8.ByteString
    -- ^ Column name for domain
    -> B8.ByteString
    -- ^ Column name for range
    -> SQLiteManifestSingle access domain range

data SQLiteManifestMultiple :: Access -> * -> * -> * where
  SQLiteManifestMultiple
    :: SQLiteDescriptor
    -> B8.ByteString
    -> B8.ByteString
    -> B8.ByteString
    -> SQLiteManifestMultiple access domain range

sqliteSingle :: String -> B8.ByteString -> B8.ByteString -> B8.ByteString -> SQLiteManifestSingle access domain range
sqliteSingle file = SQLiteManifestSingle (SQLD file)

sqliteMultiple :: String -> B8.ByteString -> B8.ByteString -> B8.ByteString -> SQLiteManifestMultiple access domain range
sqliteMultiple file = SQLiteManifestMultiple (SQLD file)

class TextSerializable a where
  textSerialize :: a -> T.Text
  textDeserialize :: T.Text -> Maybe a

instance TextSerializable () where
  textSerialize = const ""
  textDeserialize = const (Just ())

instance TextSerializable Int where
  textSerialize = T.pack . show
  textDeserialize t = case reads (T.unpack t) :: [(Int, [Char])] of
      [(i, [])] -> Just i
      _ -> Nothing

instance TextSerializable T.Text where
  textSerialize = id
  textDeserialize = Just

instance Manifest SQLiteManifestSingle where
  type ManifestResourceDescriptor SQLiteManifestSingle access domain range = SQLiteDescriptor
  resourceDescriptor (SQLiteManifestSingle sqld _ _ _) = sqld
  type ManifestDomainType SQLiteManifestSingle domain range = T.Text
  type ManifestRangeType SQLiteManifestSingle domain range = T.Text
  type ManifestDomainConstraint SQLiteManifestSingle domain range = TextSerializable domain
  type ManifestRangeConstraint SQLiteManifestSingle domain range = TextSerializable range
  type ManifestFunctor SQLiteManifestSingle domain range = Maybe
  mdomainDump _ = textSerialize
  mrangePull _ = textDeserialize

instance ManifestRead SQLiteManifestSingle where
  mget (SQLiteManifestSingle _ tableName domainName rangeName) conn key = do
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
      y <- query conn queryString (Only key)
      return $ case y :: [Only T.Text] of
        [] -> Nothing
        (y' : _) -> Just (fromOnly y')
        -- ^ TBD should we warn in case more than one row is found?

instance ManifestWrite SQLiteManifestSingle where
  mrangeDump _ = textSerialize
  mset (SQLiteManifestSingle _ tableName domainName rangeName) conn key value = case value of
      Nothing -> do
          let statement = [ "DELETE FROM "
                          , tableName
                          , " WHERE \""
                          , domainName
                          , "\"=?"
                          ]
          let queryString = fromString . B8.unpack . BS.concat $ statement
          execute conn queryString (Only key)
      Just value -> do
          let statement = [ "INSERT OR REPLACE INTO "
                          , tableName
                          , " (\""
                          , domainName
                          , "\", \""
                          , rangeName
                          , "\") VALUES (?, ?)"
                          ]
          let queryString = fromString . B8.unpack . BS.concat $ statement
          liftIO $ execute conn queryString (key, value)

instance Manifest SQLiteManifestMultiple where
  type ManifestResourceDescriptor SQLiteManifestMultiple access domain range = SQLiteDescriptor
  resourceDescriptor (SQLiteManifestMultiple sqld _ _ _) = sqld
  type ManifestDomainType SQLiteManifestMultiple domain range = T.Text
  type ManifestRangeType SQLiteManifestMultiple domain range = T.Text
  type ManifestDomainConstraint SQLiteManifestMultiple domain range = TextSerializable domain
  type ManifestRangeConstraint SQLiteManifestMultiple domain range = TextSerializable range
  type ManifestFunctor SQLiteManifestMultiple domain range = []
  mdomainDump _ = textSerialize
  mrangePull _ x = case textDeserialize x of
      Nothing -> []
      Just x' -> [x']

instance ManifestRead SQLiteManifestMultiple where
  mget (SQLiteManifestMultiple _ tableName domainName rangeName) conn key = do
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
      y <- query conn queryString (Only key)
      return $ case y :: [Only T.Text] of
        ys -> fromOnly <$> ys

instance ManifestWrite SQLiteManifestMultiple where
  mrangeDump _ = textSerialize
  mset (SQLiteManifestMultiple _ tableName domainName rangeName) conn key value = do
      -- Delete every record with the key.
      let deleteStatement = [ "DELETE FROM "
                            , tableName
                            , " WHERE \""
                            , domainName
                            , "\"=?"
                            ]
      let queryString = fromString . B8.unpack . BS.concat $ deleteStatement
      execute conn queryString (Only key)
      -- Now insert everything in the value list.
      let insertStatement = [ "INSERT INTO "
                            , tableName
                            , " (\""
                            , domainName
                            , "\", \""
                            , rangeName
                            , "\") VALUES (?, ?)"
                            ]
      let queryString = fromString . B8.unpack . BS.concat $ insertStatement
      forM_ value (\v -> liftIO $ execute conn queryString (key, v))
