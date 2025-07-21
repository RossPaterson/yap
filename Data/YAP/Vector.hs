{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Vector
-- Copyright   :  (c) Ross Paterson 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- An example instance of the new classes: vectors.
--
-----------------------------------------------------------------------------

module Data.YAP.Vector (Vector(..), dot, norm) where

import Prelude.YAP
import Data.YAP.Algebra

-- | Simple vector type.
newtype Vector a = Vector [a]
    deriving (Eq, Show)

instance Functor Vector where
    fmap f (Vector as) = Vector (fmap f as)

instance (AbelianGroup a) => AbelianGroup (Vector a) where
    zero = Vector (repeat zero)
    Vector as + Vector bs = Vector (zipWith (+) as bs)
    Vector as - Vector bs = Vector (zipWith (-) as bs)
    negate (Vector as) = Vector (map negate as)

-- | Dot product of two vectors.
dot :: (Ring a) => Vector a -> Vector a -> a
dot (Vector as) (Vector bs) = sum' (zipWith (*) as bs)

-- | Norm of a vector.
norm :: (Ring a) => Vector a -> a
norm v = v `dot` v

-- sum, generalized to monoids.
sum' :: (AbelianGroup a) => [a] -> a
sum' = foldr (+) zero
