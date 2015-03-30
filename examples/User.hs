{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Applicative
import Manifest.Manifest
import Manifest.M
import Manifest.Function
import Manifest.SmartStrategy
import Manifest.Pure
import Manifest.SQLite

data User = User T.Text
  deriving (Show, Eq, Ord)

type Email = T.Text

ada = User "ada"
richard = User "richard"
esther = User "esther"
john = User "john"
adasEmail = "ada@clare.com"
richardsEmail = "richard@carstone.com"
esthersEmail = "esther@summerson.com"
johnsEmail = "john@jarndynce.com"

userEmails :: Function ReadOnly User (Maybe Email)
userEmails = pureFunction f
  where
    f user = case user of
      User "ada" -> Just adasEmail
      User "richard" -> Just richardsEmail
      _ -> Nothing

example1 :: PFStrategy f => M f (f (Maybe Email))
example1 = do
    x <- userEmails `at_` ada
    y <- userEmails `at_` richard
    return $ appendThroughMaybe <$> x <*> y
  where
    appendThroughMaybe mx my = (T.append) <$> mx <*> my

sqliteManifest = sqlite "test.db" "test" "domain" "range"

instance TextSerializable User where
  textSerialize (User t) = t
  textDeserialize = Just . User

userEmailsS :: Function ReadWrite User (Maybe Email)
userEmailsS = function sqliteManifest

example2 :: PFStrategy f => M f (f (Maybe Email))
example2 = do
    x <- userEmailsS `at_` ada
    y <- userEmailsS `at_` richard
    return $ appendThroughMaybe <$> x <*> y
  where
    appendThroughMaybe mx my = (T.append) <$> mx <*> my

example3 :: PFStrategy f => Function ReadWrite User (Maybe Email) -> M f (f ())
example3 f = do
    (f, ada) .:= Just adasEmail
    (f, richard) .:= Just richardsEmail
