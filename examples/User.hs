{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Applicative
import Manifest.Manifest
import Manifest.FType
import Manifest.M
import Manifest.PartialFunction
import Manifest.StupidStrategy
import Manifest.Pure
import Manifest.Volatile

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

userEmails :: PureManifest FInjective ReadOnly User Email
userEmails = pureInjection forward backward
  where
    forward user = case user of
      User "ada" -> Just adasEmail
      User "richard" -> Just richardsEmail
      _ -> Nothing
    backward email = case email of
      "ada@clare.com" -> Just ada
      "richard@carstone.com" -> Just richard

example1 :: PFStrategy f => M f (f (Maybe Email))
example1 = do
    x <- injection userEmails `at_` ada
    y <- injection userEmails `at_` richard
    return $ appendThroughMaybe <$> x <*> y
  where
    appendThroughMaybe mx my = (T.append) <$> mx <*> my

example2 :: PFStrategy f => VolatileManifest FInjective ReadWrite User Email -> M f (f (Maybe Email))
example2 userEmailsVolatile = do
    (function userEmailsVolatile, esther) .:= Just esthersEmail
    (function userEmailsVolatile, john) .:= Just esthersEmail
    function userEmailsVolatile `at_` esther

runExample2 = do
  v <- volatileManifestInjective
  let x = runM (example2 v) :: StupidStrategy (Maybe Email)
  runStupidStrategy x
