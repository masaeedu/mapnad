{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Map.Strict
import Data.Monoid
import Data.Functor

import Control.Monad

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Hedgehog.Classes

newtype Mapnad k v = Mapnad { runMapnad :: Map k v }
  deriving newtype (Show, Eq, Functor)

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

aGoodSize :: Range Int
aGoodSize = R.linear 0 10

genMap :: (Ord k, Monoid k) => Gen k -> Gen a -> Gen (Mapnad k a)
genMap k g = Mapnad <$> G.map aGoodSize ((,) <$> k <*> g)

sumgen :: Gen (Sum Int)
sumgen = Sum <$> G.int aGoodSize

mulgen :: Gen (Product Int)
mulgen = Product <$> G.int aGoodSize

strgen :: Gen String
strgen = G.string aGoodSize G.alpha

main :: IO ()
main = void $ lawsCheck $ monadLaws $ genMap strgen
