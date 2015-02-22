{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Manifest.PureManifest (

    PureManifest
  , PureManifestFailure
  , emptyPM

  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Monad.Trans.State
import Data.Proxy
import Data.TypeNat.Vect
import Manifest.Manifest

-- | A pure Manifest that lives in memory.
data PureManifest a = PM (M.Map BS.ByteString [BS.ByteString])

emptyPM :: PureManifest a
emptyPM = PM M.empty

-- | Our manfiest is pure, so it's not surprising that nothing can go
--   wrong.
data PureManifestFailure

deriving instance Show PureManifestFailure

instance Manifest PureManifest where

  type ManifestMonad PureManifest = State (M.Map BS.ByteString [BS.ByteString])
  type PeculiarManifestFailure PureManifest = PureManifestFailure

  manifestRead proxy lengthProxy bs = do
      map <- get
      case M.lookup bs map of
        Nothing -> return $ Right Nothing
        Just bss -> case listToVect bss of
          Nothing -> return $ Left ()
          Just vect -> return $ Right (Just vect)

  manifestWrite proxy proxy' bs vect = do
      map <- get
      let bss = vectToList vect
      put $ M.insert bs bss map
      return ()

  manifestDelete proxy proxy' bs = do
      map <- get
      let maybeValue = M.lookup bs map
      put $ M.delete bs map
      case maybeValue of
        Nothing -> return $ Right Nothing
        Just bss -> case listToVect bss of
          Nothing -> return $ Left ()
          Just vect -> return $ Right (Just vect)

  manifestRun (PM list) action =
      let (x, s) = runState action list
      in  return $ (Right x, PM s)
