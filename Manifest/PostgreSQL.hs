{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Manifest.PostgreSQL (

    PostgreSQL

  , postgresql
  , ConnectInfo(..)

  , PostgreSQLManifestFailure

  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.String (fromString)
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Database.PostgreSQL.Simple
import Data.TypeNat.Vect
import Data.Proxy
import Manifest.Manifest

type TableName = BS.ByteString

-- | TODO state assumptions on the db form.
data PostgreSQL a = PostgreSQL {
    postgresInfo :: ConnectInfo
  , tableName :: TableName
  }

postgresql :: ConnectInfo -> TableName -> PostgreSQL a
postgresql = PostgreSQL

data PostgreSQLManifestFailure
  = PostgreSQLManifestNoConnection
  | PostgreSQLManifestBadQuery
  | PostgreSQLManifestSomeException
  deriving (Show)

-- | We just need this guy for the vectSQLSelectQuery function, which uses
--   natRecursion from IsNat, and so must work on some datatype which carries
--   a Nat index.
newtype BSSWithNatProxy (n :: Nat) = BSSWNP {
    unBSSWNP :: [BS.ByteString]
  }

-- | Dump a BSSWithNatProxy to a string which can be used as a select clause
--   in an SQL query.
exitBSSWNP :: BSSWithNatProxy n -> BS.ByteString
exitBSSWNP = BS.intercalate "," . unBSSWNP

vectSQLSelectQuery :: IsNat n => u n -> TableName -> Query
vectSQLSelectQuery proxyN tableName = fromString . B8.unpack . BS.concat $
    ["SELECT ", exitBSSWNP (vectSQLSelectQuery' proxyN), " FROM ", tableName, " WHERE key=?"]

  where

    vectSQLSelectQuery' :: forall u n . IsNat n => u n -> BSSWithNatProxy n
    vectSQLSelectQuery' _ = natRecursion inductive base incr 1

      where

        incr :: Int -> Int
        incr = (+) 1

        base _ = BSSWNP []

        -- Careful to surround those numbers in quotes! It's essential for
        -- correct interpretation by PostgreSQL.
        inductive n (BSSWNP bss) = BSSWNP $
          BS.concat ["\"", (B8.pack (show n)), "\""] : bss

vectSQLInsertQuery :: Vect a n -> TableName -> Query
vectSQLInsertQuery v tableName = fromString . B8.unpack . BS.concat $
    ["INSERT INTO ", tableName, " VALUES (", vectSQLInsertQuery' v, ")"]

  where

    vectSQLInsertQuery' :: Vect a n -> BS.ByteString
    vectSQLInsertQuery' vect = case vect of
      VNil -> ""
      VCons _ VNil -> "?"
      VCons _ v -> BS.append "?," (vectSQLInsertQuery' v)

instance Manifest PostgreSQL where

  type ManifestMonad PostgreSQL = ReaderT (Connection, TableName) IO
  type PeculiarManifestFailure PostgreSQL = PostgreSQLManifestFailure

  manifestRead proxy (proxy' :: u n) key = do
      (conn, tableName) <- ask
      let queryString = vectSQLSelectQuery proxy' tableName
      let binaryKey = Binary key
      -- Query won't indicate a failure to read via a Maybe, it will just
      -- throw an exception :(
      rows <- lift $
          catch
          (fmap Just (query conn queryString [binaryKey] :: IO [Vect BS.ByteString n]))
          (catchQueryError)
      case rows of
        Just [] -> lift $ putStrLn "No rows" >> return Nothing
        -- TODO should use ExceptT to report when a query fails, so as to
        -- distinguish it from a "not found".
        Nothing -> lift $ putStrLn "Problem" >> return Nothing
        -- We know xs = [] by primary key constraint... but we should still
        -- make our checks here, and give an error in case we detect the
        -- uniqueness constraint has failed.
        Just (x : xs) -> return $ Just x
        -- Also TODO must eliminate the key column; could do that by popping
        -- of the head I suppose.

    where

      catchQueryError :: SomeException -> IO (Maybe a)
      catchQueryError e = print e >> return Nothing

  manifestWrite proxy proxy' key valueVect = do
      (conn, tableName) <- ask
      -- This is admittedly flimsy, making the query string like this.
      let keyAndValue = VCons key valueVect
      let queryString = vectSQLInsertQuery keyAndValue tableName
      let binaryKeyAndValue = vectMap Binary keyAndValue
      -- TODO catch IO exceptions and rethrow in ExceptT
      result <- lift $ execute conn queryString binaryKeyAndValue
      return ()

  manifestRun p@(PostgreSQL connInfo tableName) action = do
      -- TODO EXCEPTION safety.
      -- TODO Transactional semantics.
      conn <- connect connInfo
      outcome <- runReaderT action (conn, tableName)
      return (Right outcome, p)
