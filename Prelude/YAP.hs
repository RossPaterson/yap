{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude.YAP
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- A replacement for the standard Prelude, aiming to preserve
-- compatibility for clients as far as possible.
-- To use this module, you'll need to turn on @RebindableSyntax@, which
-- also turns off the implicit import of the standard @Prelude@.
--
-- For greater backwards compatibility, this module hides the names of
-- the classes @AbelianGroup@, @Ring@, @Field@ and @EuclideanDomain@,
-- and their new methods @zero@, @unit@ and @associate@.  To use
-- those names, e.g. to define instances, you'll also need to import
-- "Data.YAP.Algebra".
--
-----------------------------------------------------------------------------

module Prelude.YAP (
    -- * Standard Haskell prelude
    module Prelude,
    -- ** Compatibility classes
    module Data.YAP.Algebra,
    Num(..), Real(..), Fractional(..), Integral(..),
    -- ** Unchanged classes
    RealFrac(..), Floating(..), RealFloat(..),
    -- ** Numeric functions
    even, odd, (^), (^^), fromIntegral, realToFrac,
  ) where

import Prelude hiding (
    Num(..), Real(..), Integral(..), Fractional(..),
    Floating(..), RealFrac(..), RealFloat(..),
    subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac)
import Data.YAP.Algebra hiding (
    AbelianGroup(zero), Ring, EuclideanDomain(associate, unit), Field)
import Prelude.YAP.Internal
