{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude.YAP
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- A replacement for the standard Prelude, aiming to preserve
-- compatibility for clients as far as possible.  When importing this
-- module, you'll also need
--
-- @
-- {-# LANGUAGE RebindableSyntax #-}
-- @
--
-- This turns off the implicit import of the standard @Prelude@ and
-- rebinds numeric literals and unary minus to the definitions of
-- 'fromInteger', 'fromRational' and 'negate' given here, which have
-- more general types than their standard counterparts.
--
-- This module provides generalized definitions of 'sum' and 'product',
-- so any import of "Data.List" or "Data.Foldable" will have to hide
-- those two.
-- (Ideally the members of 'Data.Foldable.Foldable' would be generalized.)
--
-- For backwards compatibility, this module hides the names of the extra
-- classes from "Data.YAP.Algebra" and their new methods.
-- To use these names, e.g. to define instances, you'll also need to
-- import "Data.YAP.Algebra".
--
-----------------------------------------------------------------------------

module Prelude.YAP (
    -- * Numeric classes
    -- ** Compatibility classes
    -- | Most of the content of these classes is now in new superclasses
    -- defined in "Data.YAP.Algebra".
    Num(..), Real, Fractional, Integral(..),
    -- ** Generalized methods
    -- | These are now methods of more general classes defined in
    -- "Data.YAP.Algebra".
    (+), (-), negate, (*), fromInteger,
    toRational, (/), recip,
    div, mod, divMod, toInteger,
    -- ** Unchanged classes
    RealFrac(..), Floating(..), RealFloat(..),
    -- * Numeric functions
    subtract, gcd, lcm,
    even, odd, (^), (^^), fromIntegral, realToFrac,
    -- * Special folds
    sum, product,
    -- * Rest of the standard Haskell prelude (unchanged)
    -- | This is the standard prelude, minus the names redefined above.
    module Prelude,
    -- * Extra glue
    ifThenElse,
  ) where

import Prelude hiding (
    -- Numeric classes
    Num(..), Real(..), Integral(..), Fractional(..),
    Floating(..), RealFrac(..), RealFloat(..),
    -- Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac,
    -- Special folds
    sum, product)
import Data.YAP.Algebra.Internal

-- | The same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract :: (AbelianGroup a) => a -> a -> a
subtract x y = y - x

-- | The sum of the numbers of a structure.
sum :: (Foldable f, AdditiveMonoid a) => f a -> a
sum = foldr (+) zero

-- | The product of the numbers of a structure.
product :: (Foldable f, Semiring a) => f a -> a
product = foldr (*) one
