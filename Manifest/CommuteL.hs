{-|
Module      : Manifest.CommuteL
Description : Class for left-commuting type constructors.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Manifest.CommuteL (

    CommuteL(..)

  ) where

import Control.Applicative
import Data.Traversable

class CommuteL (f :: * -> *) (g :: * -> *) where
  commuteL :: f (g a) -> g (f a)

instance CommuteL f f where
  commuteL = id

instance (Applicative f, Traversable g) => CommuteL g f where
  commuteL = sequenceA
