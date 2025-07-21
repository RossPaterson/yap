{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Algebra
-- Copyright   :  (c) Ross Paterson 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- Classes corresponding to common structures from abstract algebra.
--
-----------------------------------------------------------------------------

module Data.YAP.Algebra (
    -- * Classes
    AbelianGroup(..), Ring(..), EuclideanDomain(..), Field(..),
    -- * Utility functions
    subtract, gcd, lcm
  ) where

import Data.YAP.Algebra.Internal
import Prelude.YAP.Internal
