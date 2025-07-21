{-# LANGUAGE RebindableSyntax #-}

module Data.YAP.Algebra.Internal (
    module Data.YAP.Algebra.Internal,
    Ratio(..)
  ) where

import Prelude hiding (
    (+), (-), negate, (*), fromInteger,
    div, mod, divMod, (/), recip,
    subtract, gcd, lcm)
import qualified Prelude
import GHC.Real (Ratio(..))
-- import Numeric (readDec)

infixl 7  *, /, `div`, `mod`
infixl 6  +, -

infixl 7  %

-- | An Abelian group has an commutative associative binary operation
-- with an identity and inverses.
--
-- Minimal complete definition: 'zero', @('+')@ and (@('-')@ or 'negate').
class  AbelianGroup a  where
    -- | The identity of @('+')@.
    zero             :: a
    -- | A commutative associative operation with identity @zero@.
    (+), (-)         :: a -> a -> a
    -- | Inverse for @('+')@ (unary negation).
    negate           :: a -> a

    x - y            =  x + negate y
    negate x         =  zero - x

-- | A ring: addition forms an Abelian group, and multiplication defines
-- a monoid and distributes over addition.
-- Multiplication is not guaranteed to be commutative.
--
-- Minimal complete definition: @('*')@ and 'fromInteger'.
class  (AbelianGroup a) => Ring a where
    -- | An associative operation with identity @'fromInteger' 1@,
    -- distributing over @('+')@ and 'zero'.
    (*)              :: a -> a -> a

    -- | Conversion from 'Integer', the initial ring:
    -- 'fromInteger' is the unique function preserving 'zero', @('+')@,
    -- @('-')@ and @('*')@, and for which @'fromInteger' 1@ is the
    -- identity of @('*')@.
    --
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Ring' a) => a@.
    fromInteger      :: Integer -> a

-- | A integral domain (a non-trivial commutative 'Ring' with no zero
-- divisors) on which the Euclid's algorithm for 'gcd' works.
--
-- Minimal complete definition:
--  ('divMod' or ('div' and 'mod')) and 'unit'.
class  (Eq a, Ring a) => EuclideanDomain a  where
    div, mod         :: a -> a -> a
    -- ^ Division with remainder: for any @d /= 0@,
    --
    -- * @n == 'div' n d * d + 'mod' n d@
    --
    -- * @'mod' (n + a*d) d == 'mod' n d@
    --
    -- * @'mod' n d@ is smaller than @d@ in some well-founded order.
    --
    -- For integral types, @'mod' n d@ is a non-negative integer smaller
    -- than the absolute value of @d@.
    divMod           :: a -> a -> (a,a)
    -- ^ @'divMod' n d == ('div' n d, 'mod' n d)@

    associate, unit  :: a -> a
    -- ^ For each @x@ there is a decomposition @x == 'associate' x * 'unit' x@
    -- such that @'unit' x@ has a multiplicative inverse and
    --
    -- * if @x@ and @y@ are factors of each other, then @'associate' x == 'associate' y@
    --
    -- * @'associate' 1 == 1@
    --
    -- For integral types, @'associate' x@ is a non-negative integer and
    -- @'unit' x@ is @-1@ or @1@.

    n `divMod` d     =  (n `div` d, n `mod` d)
    n `div` d        =  q  where (q,_) = divMod n d
    n `mod` d        =  r  where (_,r) = divMod n d

    associate x      =  x `div` unit x

-- | A commutative 'Ring' in which all non-zero elements have multiplicative
-- inverses.
--
-- Minimal complete definition: 'recip' or @('/')@.
class  (Ring a) => Field a  where
    (/)              :: a -> a -> a
    -- | Multiplicative inverse.
    recip            :: a -> a

    recip x          =  1 / x
    x / y            =  x * recip y

-------------------------------------------------------------------------
-- instances for Prelude numeric types
-------------------------------------------------------------------------

instance  AbelianGroup Int  where
    zero            =  0
    (+)             =  (Prelude.+)
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate

instance  Ring Int  where
    (*)             =  (Prelude.*)
    fromInteger     =  Prelude.fromInteger

instance  EuclideanDomain Int  where
    div             =  Prelude.div
    mod             =  Prelude.mod
    associate x     =  abs x
    unit x          =  if x < 0 then -1 else 1

instance  AbelianGroup Integer  where
    zero            =  0
    (+)             =  (Prelude.+)
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate

instance  Ring Integer  where
    (*)             =  (Prelude.*)
    fromInteger     =  id

instance  EuclideanDomain Integer  where
    div             =  Prelude.div
    mod             =  Prelude.mod
    associate x     =  abs x
    unit x          =  if x < 0 then -1 else 1

instance  AbelianGroup Float  where
    zero            =  0
    (+)             =  (Prelude.+)
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate

instance  Ring Float  where
    (*)             =  (Prelude.*)
    fromInteger     =  Prelude.fromInteger

instance  Field Float  where
    (/)             =  (Prelude./)

instance  AbelianGroup Double  where
    zero            =  0
    (+)             =  (Prelude.+)
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate

instance  Ring Double  where
    (*)             =  (Prelude.*)
    fromInteger     =  Prelude.fromInteger

instance  Field Double  where
    (/)             =  (Prelude./)

-- Numeric functions

-- | The same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract         :: (AbelianGroup a) => a -> a -> a
subtract         =  flip (-)

-- | @'gcd' x y@ is a common factor of @x@ and @y@ such that
--
-- * @'associate' ('gcd' x y) == 'gcd' x y@, and
--
-- * any common factor of @x@ and @y@ is a factor of @'gcd' x y@.
gcd              :: (EuclideanDomain a) => a -> a -> a
gcd x 0          =  associate x
gcd x y          =  gcd y (x `mod` y)

-- | @'lcm' x y@ is a common multiple of @x@ and @y@ such that
--
-- * @'associate' ('lcm' x y) == 'lcm' x y@, and
--
-- * any common multiple of @x@ and @y@ is a multiple of @'lcm' x y@.
lcm              :: (EuclideanDomain a) => a -> a -> a
lcm _ 0          =  0
lcm 0 _          =  0
lcm x y          =  associate ((x `div` (gcd x y)) * y)

-- Other types

-- | Forms the ratio of two values in a Euclidean domain (e.g. 'Integer').
{-# SPECIALISE (%) :: Integer -> Integer -> Rational #-}
(%)                     :: (EuclideanDomain a) => a -> a -> Ratio a

-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator               :: (EuclideanDomain a) => Ratio a -> a

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator             :: (EuclideanDomain a) => Ratio a -> a

-- y /= 0 && x % y = n :% d ==>
--      associate d = d &&
--      x*d = y*n
--      x*d' = y*n' ==> exists a. d' = a*d && n' = a*n
x % y                   =  (x `div` (d * unit y')) :% associate y'
                           where y' = y `div` d
                                 d = gcd x y

numerator (x :% _)      =  x

denominator (_ :% y)    =  y

{-
instance  (EuclideanDomain a, Ord a) => Ord (Ratio a)  where
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y
-}

instance  (EuclideanDomain a) => AbelianGroup (Ratio a)  where
    zero                =  zero :% 1
    (x:%y) + (x':%y')   =  (x*y' + x'*y) % (y*y')
    negate (x:%y)       =  (-x) :% y

instance  (EuclideanDomain a) => Ring (Ratio a)  where
    (x:%y) * (x':%y')   =  (x * x') % (y * y')
    fromInteger x       =  fromInteger x :% 1

instance  (EuclideanDomain a) => Field (Ratio a)  where
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (x:%y)        =  y % x

{-
ratPrec = 7 :: Int

instance  (EuclideanDomain a, Read a) => Read (Ratio a)  where
    readsPrec p  =  readParen (p > ratPrec)
                              (\r -> [(x%y,u) | (x,s)   <- readsPrec (ratPrec+1) r,
                                                ("%",t) <- lex s,
                                                (y,u)   <- readsPrec (ratPrec+1) t ])

instance  (EuclideanDomain a, Show a)  => Show (Ratio a)  where
    showsPrec p (x:%y)  =  showParen (p > ratPrec)
                               (showsPrec (ratPrec+1) x . 
                                showString " % " . 
                                showsPrec (ratPrec+1) y)
-}

-- | Direct product
instance  (AbelianGroup a, AbelianGroup b) => AbelianGroup (a,b)  where
    zero            =  (zero, zero)
    (x,y) + (x',y') =  (x+x', y+y')
    (x,y) - (x',y') =  (x-x', y-y')
    negate (x,y)    =  (negate x, negate y)

-- | Direct product
instance  (Ring a, Ring b) => Ring (a,b)  where
    (x,y) * (x',y') =  (x*x', y*y')
    fromInteger n   =  (fromInteger n, fromInteger n)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
