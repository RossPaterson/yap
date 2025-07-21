{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Complex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A version of "Data.Complex", using the same type, but with less
-- constrained operations.  In particular this version permits Gaussian
-- integers.
--
-----------------------------------------------------------------------------

module Data.YAP.Complex
        (
        -- * Rectangular form
          Complex((:+))

        , realPart      -- :: Complex a -> a
        , imagPart      -- :: Complex a -> a
        -- * Polar form
        , mkPolar       -- :: (RealFloat a) => a -> a -> Complex a
        , cis           -- :: (RealFloat a) => a -> Complex a
        , polar         -- :: (RealFloat a) => Complex a -> (a,a)
        , magnitude     -- :: (RealFloat a) => Complex a -> a
        , phase         -- :: (RealFloat a) => Complex a -> a
        -- * Conjugate
        , conjugate     -- :: (AbelianGroup a) => Complex a -> Complex a

        -- Complex instances:
        --
        --  (Eq a)        => Eq         (Complex a)
        --  (Read a)      => Read       (Complex a)
        --  (Show a)      => Show       (Complex a)
        --  (AbelianGroup a) => AbelianGroup (Complex a)
        --  (Ring a)      => Ring       (Complex a)
        --  (Integral a)  => EuclideanDomain (Complex a)
        --  (RealFloat a) => Field      (Complex a)
        --  (RealFloat a) => Num        (Complex a)
        --  (RealFloat a) => Fractional (Complex a)
        --  (RealFloat a) => Floating   (Complex a)

        )  where

import Prelude.YAP
import Data.YAP.Algebra

import Data.Complex (Complex((:+)))

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The conjugate of a complex number.
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: (AbelianGroup a) => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

-- | Form a complex number from polar components of magnitude and phase.
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: (Floating a) => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: (Floating a) => a -> Complex a
cis theta        =  cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
{-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
polar            :: (RealFloat a) => Complex a -> (a,a)
polar z          =  (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
--
-- 'RealFloat' is used to do scaling to reduce the incidence of overflow.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
                     (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
--
-- 'RealFloat' is need for 'atan2'.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0)   = 0            -- SLPJ July 97 from John Peterson
phase (x:+y)     = atan2 y x


-- -----------------------------------------------------------------------------
-- Instances of Complex

instance  (AbelianGroup a) => AbelianGroup (Complex a)  where
    {-# SPECIALISE instance AbelianGroup (Complex Float) #-}
    {-# SPECIALISE instance AbelianGroup (Complex Double) #-}
    zero                =  zero :+ zero
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    negate (x:+y)       =  negate x :+ negate y

instance  (Ring a) => Ring (Complex a)  where
    {-# SPECIALISE instance Ring (Complex Float) #-}
    {-# SPECIALISE instance Ring (Complex Double) #-}
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    fromInteger n       =  fromInteger n :+ 0

-- | Gaussian integers:
-- if @b@ is non-zero, the norm (squared magnitude) of @'mod' a b@
-- is at most half that of @b@.
-- Standard associates lie in the positive quadrant.
instance  (Integral a) => EuclideanDomain (Complex a)  where
    {-# SPECIALISE instance EuclideanDomain (Complex Int) #-}
    {-# SPECIALISE instance EuclideanDomain (Complex Integer) #-}
    (x:+y) `div` (x':+y')
                        =  round_div (x*x' + y*y') :+ round_div (x'*y - x*y')
                           where round_div i = (2*i + n) `div` (2*n)
                                 n = x'*x' + y'*y'
    a `mod` b           =  a - b*(a `div` b)
    divMod a b          =  (q, a - b*q)
                           where q = a `div` b
    unit (x :+ y)
      | y > 0 && x <= 0 =  0 :+ 1
      | x < 0 && y <= 0 =  -1
      | y < 0 && x >= 0 =  0 :+ (-1)
      | otherwise       =  1

-- | 'RealFloat' is used to do scaling to reduce the incidence of overflow.
instance  (RealFloat a) => Field (Complex a)  where
    {-# SPECIALISE instance Field (Complex Float) #-}
    {-# SPECIALISE instance Field (Complex Double) #-}
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where x'' = scaleFloat k x'
                                 y'' = scaleFloat k y'
                                 k   = - max (exponent x') (exponent y')
                                 d   = x'*x'' + y'*y''

instance  (RealFloat a) => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    abs z               =  magnitude z :+ 0
    signum (0:+0)       =  0
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z

instance  (RealFloat a) => Fractional (Complex a)  where
    fromRational a      =  fromRational a :+ 0

instance  (RealFloat a) => Floating (Complex a) where
    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    sqrt (0:+0)    =  0
    sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
                      where sinx  = sin x
                            cosx  = cos x
                            sinhy = sinh y
                            coshy = cosh y

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z         =  y'':+(-x'')
                      where (x'':+y'') = log (z + ((-y'):+x'))
                            (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y':+(-x')
                      where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  0.5 * log ((1.0+z) / (1.0-z))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
