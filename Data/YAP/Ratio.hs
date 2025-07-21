{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Ratio
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Standard functions on rational numbers.
--
-- This is a replacement for "Data.Ratio", using the same 'Ratio'
-- type, but with components generalized from 'Integral' to
-- 'Prelude.YAP.StandardAssociate' + 'Prelude.YAP.Euclidean'.
--
-- Using the same type means we have the old instances.
-- The 'Ord' and 'Read' instances could be relaxed to:
--
-- @
-- instance (Ord a, Semiring a) => Ord (Ratio a)
-- instance (Eq a, StandardAssociate a, Euclidean a, Read a) => Read (Ratio a)
-- @
--
-----------------------------------------------------------------------------

module Data.YAP.Ratio
    ( Ratio
    , Rational
    , (%)
    , numerator
    , denominator
    , approxRational

  ) where

import Prelude.YAP
import Data.YAP.Algebra (ToRational(..))
import Data.YAP.Algebra.Internal (Ratio(..), (%), numerator, denominator)

-- -----------------------------------------------------------------------------
-- approxRational (unchanged from standard Data.Ratio)

-- | 'approxRational', applied to two real fractional numbers @x@ and @epsilon@,
-- returns the simplest rational number within @epsilon@ of @x@.
-- A rational number @y@ is said to be /simpler/ than another @y'@ if
--
-- * @'abs' ('numerator' y) <= 'abs' ('numerator' y')@, and
--
-- * @'denominator' y <= 'denominator' y'@.
--
-- Any real interval contains a unique simplest rational;
-- in particular, note that @0\/1@ is the simplest rational of all.

-- Implementation details: Here, for simplicity, we assume a closed rational
-- interval.  If such an interval includes at least one whole number, then
-- the simplest rational is the absolutely least whole number.  Otherwise,
-- the bounds are of the form q%1 + r%d and q%1 + r'%d', where abs r < d
-- and abs r' < d', and the simplest rational is q%1 + the reciprocal of
-- the simplest rational between d'%r' and d%r.

approxRational :: (ToRational a) => a -> a -> Rational
approxRational rat eps =
    -- We convert rat and eps to rational *before* subtracting/adding since
    -- otherwise we may overflow. This was the cause of #14425.
    simplest (toRational rat - toRational eps) (toRational rat + toRational eps)
  where
    simplest x y
      | y < x      =  simplest y x
      | x == y     =  xr
      | x > 0      =  simplest' n d n' d'
      | y < 0      =  - simplest' (-n') d' (-n) d
      | otherwise  =  0 :% 1
      where xr  = toRational x
            n   = numerator xr
            d   = denominator xr
            nd' = toRational y
            n'  = numerator nd'
            d'  = denominator nd'

    simplest' n d n' d'       -- assumes 0 < n%d < n'%d'
      | r == 0     =  q :% 1
      | q /= q'    =  (q+1) :% 1
      | otherwise  =  (q*n''+d'') :% n''
      where (q,r)      =  quotRem n d
            (q',r')    =  quotRem n' d'
            nd''       =  simplest' d' r' d r
            n''        =  numerator nd''
            d''        =  denominator nd''
