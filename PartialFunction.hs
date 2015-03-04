{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad.Free

-- Just for an example
import qualified Data.Map as M

--data ManifestN a b
--data ManifestI a b
type ManifestN a b = a -> Maybe b

data PartialFunctionN :: * -> * -> * where
  PFN :: ManifestN a b -> PartialFunctionN a b
  CPFN :: PartialFunctionN a b -> PartialFunctionN b c -> PartialFunctionN a c

data PartialFunctionI :: * -> * -> * where
  --PFI :: ManifestI a b -> PartialFunctionI a b
  PFI :: ManifestN a b -> ManifestN b a -> PartialFunctionI a b
  CPFI :: PartialFunctionI a b -> PartialFunctionI b c -> PartialFunctionI a c
  IPFI :: PartialFunctionI a b -> PartialFunctionI b a

data PartialFunction :: * -> * -> * where
  Normal :: PartialFunctionN a b -> PartialFunction a b
  Injective :: PartialFunctionI a b -> PartialFunction a b

-- | Make a normal partial function from an injective one, by dropping the
--   proof of injectivity.
makeN :: PartialFunctionI a b -> PartialFunctionN a b
makeN pfi = case pfi of
  PFI f _ -> PFN f
  CPFI pff pfg -> CPFN (makeN pff) (makeN pfg)
  IPFI pf -> makeN (invertPF pf)

invertPF :: PartialFunctionI a b -> PartialFunctionI b a
invertPF pfi = case pfi of
  PFI f g -> PFI g f
  CPFI pff pfg -> CPFI (invertPF pfg) (invertPF pff)
  IPFI pf -> pf

runPF :: PartialFunction a b -> a -> Maybe b
runPF pf x = case pf of
  Normal pfn -> atN pfn x
  Injective pfi -> atI pfi x

atN :: PartialFunctionN a b -> a -> Maybe b
atN pfn x = case pfn of
  PFN f -> f x
  CPFN pf pg -> case atN pf x of
    Nothing -> Nothing
    Just y -> atN pg y

atI :: PartialFunctionI a b -> a -> Maybe b
atI pfi x = case pfi of
  PFI f _ -> f x
  CPFI pf pg -> case atI pf x of
    Nothing -> Nothing
    Just y -> atI pg y
  IPFI pf -> atI (invertPF pf) x

(~>) :: PartialFunction a b -> PartialFunction b c -> PartialFunction a c
(~>) pf pg = case pf of
  Normal pfnf -> case pg of
    Normal pfng -> Normal $ CPFN pfnf pfng
    Injective pfig -> Normal $ CPFN pfnf (makeN pfig)
  Injective pfif -> case pg of
    Injective pfig -> Injective $ CPFI pfif pfig
    Normal pfng -> Normal $ CPFN (makeN pfif) pfng

fromPure :: (a -> b) -> PartialFunction a b
fromPure f = Normal (PFN (fmap Just f))

fromPureInjective :: (a -> b) -> (b -> a) -> PartialFunction a b
fromPureInjective f g = Injective (PFI (fmap Just f) (fmap Just g))

fromMap :: Ord a => M.Map a b -> PartialFunction a b
fromMap map = Normal (PFN (\x -> M.lookup x map))

data M' :: * -> * where
  At :: PartialFunction domain range -> domain -> (Maybe range -> t) -> M' t
  MPure :: t -> M' t

instance Functor M' where
  fmap f x = case x of
    At a b g -> At a b (fmap f g)
    MPure a -> MPure (f a)

-- Our monad for writing queries and assignments.
type M = Free M'

runM :: M a -> IO a
runM = iterM run
  where
    run x = case x of
      MPure m -> m
      At p x f -> f (runPF p x)

at :: PartialFunction domain range -> domain -> M (Maybe range)
at m x = liftF $ At m x id

example = do
  x <- (fromPure not) `at` False
  y <- (fromPure not) `at` True
  z <- (fromMap (M.fromList [("A",False), ("B",False), ("C",True)])) `at` "B"
  return $ (&&) <$> x <*> z

