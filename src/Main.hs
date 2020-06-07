{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Map.Strict
import Data.Monoid

import Control.Monad

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Hedgehog.Classes

newtype Mapnad k v = Mapnad { runMapnad :: Map k v }
  deriving newtype (Show, Eq, Functor)

fromList' :: Ord k => [(k, v)] -> Mapnad k v
fromList' = Mapnad . fromList

toList' :: Mapnad k v -> [(k,  v)]
toList' = toList . runMapnad

instance (Ord k, Monoid k) => Applicative (Mapnad k)
  where
  pure = return
  (<*>) = ap

joinMapnad :: (Ord k, Monoid k) => Mapnad k (Mapnad k v) -> Mapnad k v
joinMapnad = fromList' . fmap join . (>>= sequenceA) . toList' . fmap toList'

instance (Ord k, Monoid k) => Monad (Mapnad k)
  where
  return = Mapnad . singleton mempty
  ma >>= amb = joinMapnad $ fmap amb ma

aGoodSize :: Range Int
aGoodSize = R.linear 0 10

genMap :: (Ord k, Monoid k) => Gen k -> Gen a -> Gen (Mapnad k a)
genMap k g = Mapnad <$> G.map aGoodSize ((,) <$> k <*> g)

sumgen :: Gen (Sum Int)
sumgen = Sum <$> G.int (R.linear (-100) 100)

strgen :: Gen String
strgen = G.string aGoodSize G.alpha

main :: IO Bool
main = do
  lawsCheck $ monadLaws $ genMap strgen
  lawsCheck $ monadLaws $ genMap sumgen
