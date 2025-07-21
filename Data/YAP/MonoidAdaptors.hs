{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.MonoidAdaptors
-- Copyright   :  (c) Ross Paterson 2024
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- Generalized versions of the monoid adaptor types from "Data.Semigroup".
--
-----------------------------------------------------------------------------

module Data.YAP.MonoidAdaptors (
    Sum(..),
    Product(..),
  ) where

import Data.YAP.Algebra
import Prelude.YAP

import Data.Data (Data)
import GHC.Generics (Generic, Generic1)

-- | A redefinition of the @Sum@ adaptor from "Data.Semigroup" with
-- more general instances.
newtype Sum a = Sum a
    deriving (Bounded, Eq, Ord, Read, Show,
        AdditiveMonoid, AbelianGroup, Semiring, ToRational, Ring,
        Num,
        Data, Generic, Generic1, Functor, Foldable, Traversable)
 
instance (AdditiveMonoid a) => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance (AdditiveMonoid a) => Monoid (Sum a) where
    mempty = Sum zero

instance Applicative Sum where
    pure x = Sum x
    Sum f <*> Sum x = Sum (f x)

instance Monad Sum where
    Sum x >>= f = f x

-- | A redefinition of the @Product@ adaptor from "Data.Semigroup" with
-- more general instances.
newtype Product a = Product a
    deriving (Bounded, Eq, Ord, Read, Show,
        AdditiveMonoid, AbelianGroup, Semiring, ToRational, Ring,
        Num,
        Data, Generic, Generic1, Functor, Foldable, Traversable)
 
instance (Semiring a) => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance (Semiring a) => Monoid (Product a) where
    mempty = Product one

instance Applicative Product where
    pure x = Product x
    Product f <*> Product x = Product (f x)

instance Monad Product where
    Product x >>= f = f x
