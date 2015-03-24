
module Manifest.StupidStrategy (

    StupidStrategy
  , runStupidStrategy

  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Manifest.Manifest
import Manifest.Resource
import Manifest.PartialFunction

newtype StupidStrategy a = StupidStrategy {
    runStupidStrategy :: IO a
  }

instance Functor StupidStrategy where
  fmap f (StupidStrategy ix) = StupidStrategy $ fmap f ix

instance Applicative StupidStrategy where
  pure x = StupidStrategy $ pure x
  f <*> x = StupidStrategy $ runStupidStrategy f <*> runStupidStrategy x

instance Monad StupidStrategy where
  return x = StupidStrategy $ return x
  x >>= k = StupidStrategy $ runStupidStrategy x >>= runStupidStrategy . k

instance PFStrategy StupidStrategy where

  runGet m (StupidStrategy ix) = StupidStrategy $ do
      let rd = resourceDescriptor m
      resourceWrapper <- runExceptT (acquireResource rd)
      case resourceWrapper of
        Left _ -> error "No resource"
        Right r -> do
            x <- ix
            let domainValue = mdomainDump m <$> x
            case domainValue of
              Nothing -> return Nothing
              Just x' -> do
                    y <- runExceptT (mget m (resource r) x')
                    case y of
                      Left _ -> error "WTF?"
                      Right y' -> case mrangePull m <$> y' of
                        Nothing -> return Nothing
                        Just y'' -> return y''

  runSet m (StupidStrategy ix) (StupidStrategy iy) = StupidStrategy $ do
      let rd = resourceDescriptor m
      resourceWrapper <- runExceptT (acquireResource rd)
      case resourceWrapper of
        Left _ -> error "No resource"
        Right r -> do
            x <- ix
            y <- iy
            let domainValue = mdomainDump m <$> x
            let rangeValue = mrangeDump m <$> y
            case domainValue of
              Nothing -> return ()
              Just x' -> do
                  outcome <- runExceptT (mset m (resource r) x' rangeValue)
                  case outcome of
                    Left _ -> error "WTF?"
                    Right () -> return ()
