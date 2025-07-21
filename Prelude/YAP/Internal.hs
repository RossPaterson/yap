{-# LANGUAGE RebindableSyntax #-}

module Prelude.YAP.Internal (
    module Prelude.YAP.Internal
  ) where

import Prelude hiding (
    Num(..), Real(..), Integral(..), Fractional(..),
    Floating(..), RealFrac(..), RealFloat(..),
    subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac)
import qualified Prelude
import Data.YAP.Algebra.Internal

infixr 8  ^, ^^, **
infixl 7  `quot`, `rem`

-- Numeric classes

-------------------------------------------------------------------------
-- refactored Haskell 98 classes
-------------------------------------------------------------------------

-- all builtin numeric types, plus Ratio and Complex
-- | Haskell 98 compatibility class
class  (Ring a) => Num a  where
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a

-- | unchanged from Haskell 98
class  (Num a, Ord a) => Real a  where
    -- | The rational equivalent of its real argument with full precision
    toRational       ::  a -> Rational

-- | Integral numbers, supporting integer division.
--   
-- Minimal complete definition: 'toInteger'.
class  (Real a, Enum a, EuclideanDomain a) => Integral a  where
    -- | Integer division truncated toward zero
    quot                :: a -> a -> a
    -- | Integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    rem                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    quotRem          :: a -> a -> (a,a)
    -- | Conversion to 'Integer'
    toInteger        :: a -> Integer

        -- Minimal complete definition:
        --      toInteger
    n `quot` d       =  q  where (q,_) = quotRem n d
    n `rem` d        =  r  where (_,r) = quotRem n d
    quotRem n d      =  if signum r == - signum d then (q+1, r-d) else qr
                        where qr@(q,r) = divMod n d

-- | Haskell 98 compatibility class
class  (Num a, Field a) => Fractional a  where
    -- | Convert from 'Rational'
    --
    -- A floating point numeric literal represents the application of
    -- the function 'fromRational' to the appropriate value of type
    -- 'Rational', so such literals have type @('Field' a) => a@.
    fromRational    :: Rational -> a

    fromRational x   =  fromInteger (numerator x) /
                        fromInteger (denominator x)

-------------------------------------------------------------------------
-- unchanged Haskell 98 classes
-------------------------------------------------------------------------

-- | unchanged from Haskell 98
class  (Fractional a) => Floating a  where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

        -- Minimal complete definition:
        --      pi, exp, log, sin, cos, sinh, cosh
        --      asin, acos, atan
        --      asinh, acosh, atanh
    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    sqrt x           =  x ** 0.5
    tan  x           =  sin  x / cos  x
    tanh x           =  sinh x / cosh x

-- | unchanged from Haskell 98
class  (Real a, Fractional a) => RealFrac a  where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

        -- Minimal complete definition:
        --      properFraction
    truncate x       =  m  where (m,_) = properFraction x
    
    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m
    
    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x
    
    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x

-- | unchanged from Haskell 98
class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

        -- Minimal complete definition:
        --      All except exponent, significand, 
        --                 scaleFloat, atan2
    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

    atan2 y x
      | x>0           =  atan (y/x)
      | x==0 && y>0   =  pi/2
      | x<0  && y>0   =  pi + atan (y/x) 
      |(x<=0 && y<0)  ||
       (x<0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                      = -atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      =  pi    -- must be after the previous test on zero y
      | x==0 && y==0  =  y     -- must be after the other double zero tests
      | otherwise     =  x + y -- x or y is a NaN, return a NaN (via +)

-- Numeric functions

even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

-- | raise a number to a non-negative integral power
(^)              :: (Ring a, Integral b) => a -> b -> a
_ ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n  where
                                    g x n | even n  = g (x*x) (n `quot` 2)
                                          | otherwise = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"

-- | raise a number to an integral power
(^^)             :: (Field a, Integral b) => a -> b -> a
x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))

-- | General conversion from integral types, via the 'Integer' type.
fromIntegral     :: (Integral a, Ring b) => a -> b
fromIntegral     =  fromInteger . toInteger

-- | General conversion to fields, via the 'Rational' type.
realToFrac      :: (Real a, Fractional b) => a -> b
realToFrac      =  fromRational . toRational

-------------------------------------------------------------------------
-- instances for Prelude numeric types
-------------------------------------------------------------------------

instance  Num Int  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

instance  Real Int  where
    toRational x    =  toInteger x :% 1

instance  Integral Int  where
    toInteger       =  Prelude.toInteger

instance  Num Integer  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

instance  Real Integer  where
    toRational x    =  x :% 1

instance  Integral Integer  where
    toInteger       =  id

instance  Num Float  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

instance  Real Float  where
    toRational x    =  (m%1)*(b%1)^^n
                       where (m,n) = Prelude.decodeFloat x
                             b     = Prelude.floatRadix  x

instance  Fractional Float  where
    fromRational    =  Prelude.fromRational

instance  Floating Float  where
    pi              =  Prelude.pi
    exp             =  Prelude.exp
    log             =  Prelude.log
    sin             =  Prelude.sin
    cos             =  Prelude.cos
    sinh            =  Prelude.sinh
    cosh            =  Prelude.cosh
    asin            =  Prelude.asin
    acos            =  Prelude.acos
    atan            =  Prelude.atan
    asinh           =  Prelude.asinh
    acosh           =  Prelude.acosh
    atanh           =  Prelude.atanh

instance  RealFrac Float  where
    properFraction x =  (fromInteger n, r)
                        where (n,r) = Prelude.properFraction x

instance  RealFloat Float  where
    floatRadix      =  Prelude.floatRadix
    floatDigits     =  Prelude.floatDigits
    floatRange      =  Prelude.floatRange
    decodeFloat     =  Prelude.decodeFloat
    encodeFloat     =  Prelude.encodeFloat
    isNaN           =  Prelude.isNaN
    isInfinite      =  Prelude.isInfinite
    isDenormalized  =  Prelude.isDenormalized
    isNegativeZero  =  Prelude.isNegativeZero
    isIEEE          =  Prelude.isIEEE

instance  Num Double  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

instance  Real Double  where
    toRational x    =  (m%1)*(b%1)^^n
                       where (m,n) = Prelude.decodeFloat x
                             b     = Prelude.floatRadix  x

instance  Fractional Double  where
    fromRational    =  Prelude.fromRational

instance  Floating Double  where
    pi              =  Prelude.pi
    exp             =  Prelude.exp
    log             =  Prelude.log
    sin             =  Prelude.sin
    cos             =  Prelude.cos
    sinh            =  Prelude.sinh
    cosh            =  Prelude.cosh
    asin            =  Prelude.asin
    acos            =  Prelude.acos
    atan            =  Prelude.atan
    asinh           =  Prelude.asinh
    acosh           =  Prelude.acosh
    atanh           =  Prelude.atanh

instance  RealFrac Double  where
    properFraction x =  (fromInteger n, r)
                        where (n,r) = Prelude.properFraction x

instance  RealFloat Double  where
    floatRadix      =  Prelude.floatRadix
    floatDigits     =  Prelude.floatDigits
    floatRange      =  Prelude.floatRange
    decodeFloat     =  Prelude.decodeFloat
    encodeFloat     =  Prelude.encodeFloat
    isNaN           =  Prelude.isNaN
    isInfinite      =  Prelude.isInfinite
    isDenormalized  =  Prelude.isDenormalized
    isNegativeZero  =  Prelude.isNegativeZero
    isIEEE          =  Prelude.isIEEE

-- Prelude.Integral a is required here to get Show (Ratio a)

instance  (Integral a, Prelude.Integral a)  => Num (Ratio a)  where
    abs (x:%y)          =  abs x :% y
    signum (x:%_)       =  signum x :% 1

instance  (Integral a, Prelude.Integral a)  => Real (Ratio a)  where
    toRational (x:%y)   =  toInteger x :% toInteger y

instance  (Integral a, Prelude.Integral a)  => Fractional (Ratio a)  where
    fromRational x      =  fromInteger (numerator x) :%
                           fromInteger (denominator x)

instance  (Integral a, Prelude.Integral a)  => RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
                            where (q,r) = quotRem x y
