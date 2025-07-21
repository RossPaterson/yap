{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Polynomial
-- Copyright   :  (c) Ross Paterson 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- An example instance of the new classes: polynomials.
-- Some of these functions work with infinite polynomials,
-- i.e. formal power series.
--
-----------------------------------------------------------------------------

module Data.YAP.Polynomial (
    -- * Polynomials
    Polynomial,
    polynomial,
    -- * Queries
    -- ** Finite polynomials
    coefficients,
    degree,
    evaluate,
    pretty,
    -- ** Formal power series
    approximations,
    -- * Operations
    compose,
    differentiate,
    integrate
  ) where

import Prelude.YAP
import Data.YAP.Algebra

-- Coefficients, least significant first.
-- There may be trailing zeroes, but the interface hides them.
newtype Polynomial a = P [a]

instance Functor Polynomial where
    fmap f (P as) = P (map f as)

instance (Eq a, AbelianGroup a) => Eq (Polynomial a) where
    P as == P bs = eq as bs
      where eq [] ys = allZero ys
            eq xs [] = allZero xs
            eq (x:xs) (y:ys) = x == y && eq xs ys

allZero :: (Eq a, AbelianGroup a) => [a] -> Bool
allZero = all (== zero)

instance (Eq a, Show a, AbelianGroup a) => Show (Polynomial a) where
    showsPrec p x =
        showParen (p > 10) $ showString "polynomial " . shows (coefficients x)

instance (AbelianGroup a) => AbelianGroup (Polynomial a) where
    zero            = P []
    P xs + P ys     = P (add xs ys)
    negate (P xs)   = P (map negate xs)

add :: (AbelianGroup a) => [a] -> [a] -> [a]
add [] ys = ys
add xs [] = xs
add (x:xs) (y:ys) = x+y : add xs ys

instance (Ring a) => Ring (Polynomial a) where
    P xs * P ys     = P (mul xs ys)
    fromInteger i   = P [fromInteger i]

mul :: (Ring a) => [a] -> [a] -> [a]
mul [] _ys = []
mul _xs [] = []
mul (x:xs) ys = add (map (x*) ys) (zero:mul xs ys)

-- | If @b@ is non-zero, @'mod' a b@ has a smaller degree than @b@.
-- If @a@ is non-zero, @'associate' a@ has a leading coefficient of @1@.
instance (Eq a, Field a) => EuclideanDomain (Polynomial a) where
    divMod a b
      | null bs = error "division by zero"
      | otherwise = (P (reverse d), P (reverse m))
      where (d, m) = divModAux n as bs
            as = rev_coefficients a
            bs = rev_coefficients b
            n = length as - length bs

    associate (P as)
      | allZero as  = P []
      | otherwise   = P (map (/ last as) (init as) ++ [1])
    unit (P as)
      | allZero as  = P [1]
      | otherwise   = P [last as]

divModAux :: (Eq a, Field a) => Int -> [a] -> [a] -> ([a],[a])
divModAux n as _
  | n < 0 = ([], as)
divModAux n (0:as) bs = (0:d, m)
  where (d, m) = divModAux (n-1) as bs
divModAux n (a:as) bs@(b:bs') = (c:d, m)
  where c = a/b
        (d, m) = divModAux (n-1) (sub as (map (c*) bs')) bs

sub :: (AbelianGroup a) => [a] -> [a] -> [a]
sub xs [] = xs
sub (x:xs) (y:ys) = (x-y) : sub xs ys
sub [] _ = error "can't happen"

-- | Construct a polynomial from a list of coefficients,
-- least significant first.
polynomial :: [a] -> Polynomial a
polynomial = P

-- | The coefficients of a finite polynomial, least significant first
-- and with no trailing zeroes.
coefficients :: (Eq a, AbelianGroup a) => Polynomial a -> [a]
coefficients (P as) = reverse (dropWhile (== zero) (reverse as))

-- | The coefficients of a finite polynomial, starting with the most
-- significant non-zero coefficient.
rev_coefficients :: (Eq a, AbelianGroup a) => Polynomial a -> [a]
rev_coefficients (P as) = dropWhile (== zero) (reverse as)

-- | The degree of a finite polynomial.
--
-- @'degree' p = length (coefficients p)@
degree :: (Eq a, AbelianGroup a) => Polynomial a -> Int
degree a = length (rev_coefficients a)

-- | Evaluate a polynomial for a given value of @x@.
--
-- @'evaluate' a x = 'zipWith' (*) ('coefficients' a) ('iterate' (*x) 1)@
--
-- (The implementation uses Horner's rule.)
evaluate :: (Ring a) => Polynomial a -> a -> a
evaluate (P as) x = foldr (\ a v -> a + x*v) zero as

-- | The infinite list of evaluations of truncations of the polynomial
-- or power series.
approximations :: (Ring a) => Polynomial a -> a -> [a]
approximations (P as) x = scanr1 (+) (zipWith (*) as (iterate (*x) 1))

-- other functions

-- | Pretty-print a polynomial, e.g.
--
-- @pretty (polynomial [3, 4, 0, 1, 5]) \"x\" = \"5x^4 + x^3 + 4x + 3\"@
pretty :: (Ord a, Show a, Ring a) => Polynomial a -> String -> String
pretty (P as) x = case dropWhile ((== zero) . fst) (reverse (zip as terms)) of
    [] -> "0"
    (a,t):ats -> showFirst a ++ t ++ showRest ats
  where terms = "" : x : [x ++ "^" ++ show n | n <- [2..]::[Int]]
        showFirst a
          | a < 0 = '-':show (negate a)
          | a == 1 = ""
          | otherwise = show a
        showRest [] = ""
        showRest [(a,t)]
          | a < 0 = " - " ++ show (negate a) ++ t
          | a > 0 = " + " ++ show a ++ t
          | otherwise = ""
        showRest ((a,t):ats)
          | a < 0 = " - " ++ show (negate a) ++ t ++ showRest ats
          | a == 1 = " + " ++ t  ++ showRest ats
          | a > 0 = " + " ++ show a ++ t ++ showRest ats
          | otherwise = showRest ats

-- | Composition of polynomials:
--
-- @'evaluate' ('compose' a b) = 'evaluate' a . 'evaluate' b@
compose :: (Ring a) => Polynomial a -> Polynomial a -> Polynomial a
compose (P as) q = evaluate (P (map constant as)) q

constant :: a -> Polynomial a
constant a = P [a]

-- | Symbolic differentiation of polynomials.
differentiate :: (Ring a) => Polynomial a -> Polynomial a
differentiate (P as) = P (zipWith (*) (tail as) (map fromInteger [1..]))

-- | Symbolic integration of polynomials.
integrate :: (Field a) => Polynomial a -> Polynomial a
integrate (P as) = P (zero : zipWith (/) as (map fromInteger [1..]))
