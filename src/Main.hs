{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Map.Strict
import Data.Proxy

import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Classes

newtype Mapnad k v = Mapnad { runMapnad :: Map k v }
  deriving newtype (Show, Eq, Arbitrary, Functor)

instance (Ord k, Monoid k) => Applicative (Mapnad k)
  where
  pure = return
  (<*>) = ap

joinMapnad :: (Ord k, Monoid k) => Mapnad k (Mapnad k v) -> Mapnad k v
joinMapnad (Mapnad (fmap runMapnad -> mm)) = Mapnad $ fromList $ fmap (\(k1, (k2, v)) -> (k1 <> k2, v)) $ (>>= sequenceA) $ toList $ fmap toList $ mm

instance (Ord k, Monoid k) => Monad (Mapnad k)
  where
  return = Mapnad . singleton mempty
  ma >>= amb = joinMapnad $ fmap amb ma

main :: IO ()
main = lawsCheck $ monadLaws $ Proxy @(Mapnad String)
