{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Matrix
-- Copyright   :  (c) Ross Paterson 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- An example instance of the new classes: arbitrary-sized matrices,
-- based on a haskell-cafe posting by Udo Stenzel on 22 Jun 2006.
--
-- Beware that the identity matrix is infinite.
--
-----------------------------------------------------------------------------

module Data.YAP.Matrix (Matrix(..), apply) where

import Data.List (transpose)
import Prelude.YAP
import Data.YAP.Algebra
import Data.YAP.Vector

newtype Matrix a = Matrix [[a]]	-- ^ list of rows
    deriving (Eq, Show)

instance Functor Matrix where
    fmap f (Matrix as) = Matrix (fmap (fmap f) as)

instance (AbelianGroup a) => AbelianGroup (Matrix a) where
    zero = Matrix (repeat (repeat zero))
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    negate (Matrix as) = Matrix (map (map negate) as)

instance Ring a => Ring (Matrix a) where
    Matrix as * Matrix bs =
        Matrix [[sum' $ zipWith (*) a b | b <- transpose bs] | a <- as]
    fromInteger x = diagonal (fromInteger x)

diagonal :: (AbelianGroup a) => a -> Matrix a
diagonal x = Matrix (iterate (zero:) (x : repeat zero))

-- | Multiply a matrix by a vector.
apply :: (Ring a) => Matrix a -> Vector a -> Vector a
apply (Matrix as) (Vector b) = Vector [sum' (zipWith (*) a b) | a <- as]

-- sum, generalized to monoids.
sum' :: (AbelianGroup a) => [a] -> a
sum' = foldr (+) zero
