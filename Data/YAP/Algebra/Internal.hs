{-# LANGUAGE RebindableSyntax #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.YAP.Algebra.Internal
-- Copyright   :  (c) Ross Paterson 2011
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- Internal definitions.
--
-----------------------------------------------------------------------------

module Data.YAP.Algebra.Internal (
    module Data.YAP.Algebra.Internal,
    Ratio(..)
  ) where

import Prelude hiding (
    Num(..), Real(..), Integral(..), Fractional(..),
    Floating(..), RealFrac(..), RealFloat(..),
    even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac)
import qualified Prelude
import GHC.Real (Ratio(..))
import Data.Complex (Complex((:+)))
import Numeric.Natural (Natural)
import GHC.Num.Integer (integerToNatural)

infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

infixl 7  %

-------------------------------------------------------------------------
-- Algebraic classes
-------------------------------------------------------------------------

-- | A commutative associative binary operation with an identity.
--
-- /Rationale:/
--
-- * This is the common superclass of 'AbelianGroup' and 'Semiring'.
--   General monoids are also useful, but it is a common expectation that
--   an operation denoted by '+' is commutative.
--
-- * 'zero' is required because this class is insufficient for integer
--   literals.
--
-- * Ideally, @0@ could be defined as equivalent to 'zero', with other
--   integer literals handled by 'fromNatural'.
class  AdditiveMonoid a  where
    -- | An associative operation.
    (+)              :: a -> a -> a

    -- | The identity of @('+')@.
    zero             :: a

    -- | Sum of @n@ copies of @x@. @n@ should be non-negative.
    atimes           :: (ToInteger b) => b -> a -> a
    atimes n x
      | n <= zero    = zero
      | even n       = y+y
      | otherwise    = y+y+x
      where
        y = atimes (n `div` two) x
        two = one + one

-- | An Abelian group has a commutative associative binary operation
-- with an identity and inverses.
--
-- /Rationale:/
--
-- * The 'Prelude.abs' and 'Prelude.signum' operations lack sensible
--   definitions for many useful instances, such as complex numbers,
--   polynomials, matrices, etc.
--
-- * Types that have subtraction but not multiplication include vectors
--   and dimensioned quantities.
class  (AdditiveMonoid a) => AbelianGroup a  where
    -- | Subtraction operator.
    (-)              :: a -> a -> a

    -- | Inverse for @('+')@ (unary negation).
    negate           :: a -> a

    -- | Sum of @n@ copies of @x@.
    gtimes           :: (AbelianGroup b, ToInteger b) => b -> a -> a
    gtimes n x
      | n < zero     = negate (atimes (negate n) x)
      | otherwise    = atimes (negate n) x

    x - y            =  x + negate y
    negate x         =  zero - x

    {-# MINIMAL (-) | negate #-}

-- | A semiring: addition defines a commutative monoid, and multiplication
-- defines a monoid and distributes over addition and zero.
-- Multiplication is not guaranteed to be commutative.
--
-- /Rationale:/
--
-- * 'Natural' is the key example of a type with multiplication but not
--   subtraction, but there are many more.
--
-- * 'one' is required because this class is insufficient for integer
--   literals.
--
-- * In an ideal world, an integer literal @i@ would be treated as
--   equivalent to @'fromNatural' i@ and have type @('Semiring' a) => a@.
--   (The lexical syntax already permits only non-negative numbers.)
--
-- * 'rescale' is available here with a trivial default definition so
--   that some operations on complex numbers, whose direct definitions
--   would often overflow on 'Float' or 'Double' components, can be
--   defined for other types as well.
class  (AdditiveMonoid a) => Semiring a where
    -- | An associative operation that distributes over '(+)'.
    (*)              :: a -> a -> a

    -- | The identity of @('*')@.
    one              :: a
    one              =  fromNatural (Prelude.fromInteger 1)

    -- | Conversion from 'Natural', the initial semiring:
    -- 'fromNatural' is the unique function preserving 'zero', 'one',
    -- @('+')@ and @('*')@.
    fromNatural      :: Natural -> a
    fromNatural n    =  atimes n one

    -- | @'rescale' x y = (x', y', s)@ where @s@ is a linear function such
    -- that @s x' = x@ and @s y' = y@, chosen so that multiplications
    -- by @x'@ and @y'@ are less likely to overflow.  In the default
    -- definition, @s@ is 'id'.
    rescale          :: a -> a -> (a, a, a -> a)
    rescale x y      =  (x, y, id)

    {-# MINIMAL (*), (one | fromNatural) #-}

-- | A ring: addition forms an Abelian group, and multiplication defines
-- a monoid and distributes over addition.
-- Multiplication is not guaranteed to be commutative.
--
-- /Rationale:/
--
-- * This is sufficient to define 'fromInteger', but often a much more
--   efficient definition is available.
class  (AbelianGroup a, Semiring a) => Ring a where
    -- | Conversion from 'Integer', the initial ring:
    -- 'fromInteger' is the unique function preserving 'zero', 'one',
    -- @('+')@, @('-')@ and @('*')@.
    --
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Ring' a) => a@.
    -- (Ideally, they would represent an application of 'fromNatural',
    -- and have type @('Semiring' a) => a@.)
    fromInteger      :: Integer -> a
    fromInteger n    =  gtimes n one

-- | A cancellative commutative semiring with a designated canonical
-- factoring of each value as the product of a unit (invertible value)
-- and an associate.
--
-- /Rationale:/
--
-- * This normalization is required so that the results of operations
--   such as 'gcd' and 'lcm' can be uniquely defined.  In the original
--   Prelude, 'Prelude.abs' and 'Prelude.signum' were used for this,
--   but they lack coherent definitions on many useful instances.
--
-- * This class is usually used together with 'Euclidean', and
--   generally has matching instances.  Merging the two classes would
--   be a possibility.  That would allow a default definition of
--   'stdAssociate'.
class  (Semiring a) => StandardAssociate a  where
    -- | A representative associate:
    --
    -- * if @x@ and @y@ are factors of each other,
    --   then @'stdAssociate' x = 'stdAssociate' y@
    --
    -- * @'stdAssociate' ('stdAssociate' x) = 'stdAssociate' x@
    --
    -- * @'stdAssociate' 'zero' = 'zero'@
    --
    -- * @'stdAssociate' 'one' = 'one'@
    --
    -- For integral types, @'stdAssociate' x@ is a non-negative integer.
    stdAssociate     :: a -> a

    -- | @'stdUnit' x@ has a multiplicative inverse and satisfies
    --
    -- * @'stdAssociate' x * 'stdUnit' x = x@
    --
    -- * @'stdUnit' 'zero' = 'one'@
    --
    -- * @'stdUnit' 'one' = 'one'@
    --
    -- For integral types, @'stdUnit' x@ is @1@ or @-1@.
    stdUnit          :: a -> a

    -- | multiplicative inverse of @'stdUnit' x@
    stdRecip         :: a -> a

    stdAssociate x   =  x * stdRecip x

-- | A Euclidean semiring: a commutative semiring with Euclidean division,
-- yielding a quotient and a remainder that is smaller than the divisor in
-- a well-founded ordering.  This is sufficient to implement Euclid's
-- algorithm for the greatest common divisor.
--
-- /Rationale:/
--
-- * This class, together with 'StandardAssociate', is sufficient
--   to define 'gcd', and thus to define arithmetic operations on
--   @'Data.Ratio.Ratio' a@.
--
-- * The uniformity condition is required to make modular arithmetic work.
--   Non-integer examples include @'Data.Complex.Complex' 'Integer'@
--   (Gaussian integers) and polynomials.
--
-- * The usual definition of a Euclidean domain assumes a ring, but
--   division with remainder can be defined in the absence of negation,
--   e.g. for 'Natural'.
class  (Semiring a) => Euclidean a  where
    -- | Division with remainder: for any @d@ that is not 'zero',
    --
    -- * @n = 'div' n d * d + 'mod' n d@
    div              :: a -> a -> a

    -- | Remainder of division: for any @d@ that is not 'zero',
    --
    -- * @n = 'div' n d * d + 'mod' n d@
    --
    -- * @'mod' (n + a*d) d = 'mod' n d@
    --
    -- * @'div' 'zero' d = 'zero'@
    --
    -- * either @'mod' n d@ is 'zero' or
    --   @'euclideanNorm' ('mod' n d) < 'euclideanNorm' d@.
    --
    -- For integral types, @'mod' n d@ is a non-negative integer smaller
    -- than the absolute value of @d@.
    mod              :: a -> a -> a

    -- | @'divMod' n d = ('div' n d, 'mod' n d)@
    divMod           :: a -> a -> (a,a)

    n `divMod` d     =  (n `div` d, n `mod` d)
    n `div` d        =  q  where (q,_) = divMod n d
    n `mod` d        =  r  where (_,r) = divMod n d

    -- | A measure of the size of a non-zero value.
    -- This may be undefined on 'zero'.
    -- If the argument is non-zero, the value is positive.
    euclideanNorm :: a -> Natural

    {-# MINIMAL ((divMod | (div, mod)), euclideanNorm) #-}

-- | Types that can be faithfully embedded in 'Rational'.
--
-- /Rationale:/
--
-- * This is essentially equivalent to the old 'Prelude.Real' class,
--   but with the 'Prelude.Num' superclass reduced to 'Semiring'
--   (so it does not assume negative values).
class  (Ord a, Semiring a) => ToRational a where
    -- | The rational equivalent of its argument with full precision
    toRational       ::  a -> Rational

-- | Types representing a contiguous set of integers, including 0, 1 and 2.
--
-- /Rationale:/
--
-- * This is similar to the old 'Prelude.Integral' class, but does not
--   require subtraction, which does not work for 'Natural'.
class  (StandardAssociate a, Euclidean a, ToRational a) => ToInteger a where
    -- | Conversion to 'Integer', satisfying
    --
    -- @'fromInteger' ('toInteger' x) = x@
    toInteger        :: a -> Integer

-- | A 'Semiring' in which all non-zero elements have multiplicative inverses.
--
-- /Rationale:/
--
-- * Quaternions have multiplicative inverses, but do not form a field,
--   because multiplication is not commutative.
--
-- * Some semirings, such as the tropical semiring and its dual, support
--   division but not subtraction.
class  (Semiring a) => DivisionSemiring a  where
    -- | Multiplicative inverse of any value but 'zero'.
    recip            :: a -> a

-- | A 'Ring' in which all non-zero elements have multiplicative inverses.
--
-- /Rationale:/
--
-- * Quaternions have multiplicative inverses, but multiplication is not
--   commutative, so they do not have a well-defined division operation,
--   and do not form a field.
class  (Ring a, DivisionSemiring a) => DivisionRing a

-- | A commutative 'Semiring' in which all non-zero elements have
-- multiplicative inverses, so that division is uniquely defined.
--
-- /Rationale:/
--
-- * Some semirings, such as the tropical semiring and its dual, support
--   division but not subtraction.
class  (DivisionSemiring a) => Semifield a  where
    -- | Division operator.
    (/)              :: a -> a -> a
    x / y            =  x * recip y

    {-# MINIMAL #-}

-- | A commutative 'Ring' in which all non-zero elements have
-- multiplicative inverses.
--
-- /Rationale:/
--
-- * 'fromRational' needs to be in an independent class, because 'Rational'
--   cannot be embedded in finite fields.
--
-- * While fields trivially support Euclidean division and standard
--   associates, they are kept apart for backward compatibility.
class  (DivisionRing a, Semifield a) => Field a

-- | Rings extending 'Rational'.
-- Such a ring should have characteristic 0, i.e. no sum @1 + ... + 1@
-- should equal zero.
--
-- /Rationale:/
--
-- * For 'fromRational' to be injective, its domain must be infinite,
--   which naturally excludes finite fields.
--   Indeed 'Rational' is initial in the category of infinite fields.
--   (It's true that 'Float' and 'Double' aren't infinite, but they aren't
--   even additive monoids.)
--
-- * Conversely, many rings that are not fields support embedding of
--   rationals, e.g. polynomials and matrices.
class  (Ring a) => FromRational a  where
    -- | Conversion from a 'Rational' (that is, @'Ratio' 'Integer'@)
    -- preserving 'zero', 'one', @('+')@, @('-')@ and @('*')@.
    -- Under the assumption that the ring has characteristic 0, this
    -- implies that 'fromRational' is injective.
    --
    -- For an infinite field, 'fromRational' is the unique function
    -- preserving 'zero', 'one', @('+')@, @('-')@, @('*')@ and @('/')@.
    --
    -- A floating literal stands for an application of 'fromRational'
    -- to a value of type 'Rational', so such literals have type
    -- @('FromRational' a) => a@.
    fromRational    :: Rational -> a

-------------------------------------------------------------------------
-- instances for Prelude integral types
-------------------------------------------------------------------------

instance  AdditiveMonoid Int  where
    (+)             =  (Prelude.+)
    zero            =  0

instance  AbelianGroup Int  where
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate
    gtimes n m      =  fromIntegral n * m

instance  Semiring Int  where
    (*)             =  (Prelude.*)
    one             =  1

instance  Ring Int  where
    fromInteger     =  Prelude.fromInteger

-- | Units have absolute value 1. Standard associates are non-negative.
instance  StandardAssociate Int  where
    stdAssociate x  =  if x < 0 then -x else x
    stdUnit x       =  if x < 0 then -1 else 1
    stdRecip        =  stdUnit

-- | `mod` is non-negative
instance  Euclidean Int  where
    div             =  Prelude.div
    mod             =  Prelude.mod
    euclideanNorm   =  integerToNatural . toInteger

instance  ToRational Int  where
    toRational x    =  toInteger x :% 1

instance  ToInteger Int  where
    toInteger       =  Prelude.toInteger

instance  AdditiveMonoid Word  where
    (+)             =  (Prelude.+)
    zero            =  0

instance  AbelianGroup Word  where
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate
    gtimes n m      =  fromIntegral n * m

instance  Semiring Word  where
    (*)             =  (Prelude.*)
    one             =  1

instance  Ring Word  where
    fromInteger     =  Prelude.fromInteger

-- | The only unit is 1. 'stdAssociate' is the identity.
instance  StandardAssociate Word  where
    stdAssociate x  =  x
    stdUnit _       =  1
    stdRecip _      =  1

instance  Euclidean Word  where
    div             =  Prelude.div
    mod             =  Prelude.mod
    euclideanNorm   =  integerToNatural . toInteger

instance  ToRational Word  where
    toRational x    =  toInteger x :% 1

instance  ToInteger Word  where
    toInteger       =  Prelude.toInteger

instance  AdditiveMonoid Natural  where
    (+)             =  (Prelude.+)
    zero            =  Prelude.fromInteger 0

instance  Semiring Natural  where
    (*)             =  (Prelude.*)
    one             =  Prelude.fromInteger 1
    fromNatural     =  id

-- | The only unit is 1. 'stdAssociate' is the identity.
instance  StandardAssociate Natural  where
    stdAssociate x  =  x
    stdUnit _       =  one
    stdRecip _      =  one

instance  Euclidean Natural  where
    div             =  Prelude.div
    mod             =  Prelude.mod
    euclideanNorm   =  id

instance  ToRational Natural  where
    toRational x    =  toInteger x :% 1

instance  ToInteger Natural  where
    toInteger       =  Prelude.toInteger

instance  AdditiveMonoid Integer  where
    (+)             =  (Prelude.+)
    zero            =  0

instance  AbelianGroup Integer  where
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate
    gtimes n m      =  toInteger n * m

instance  Semiring Integer  where
    (*)             =  (Prelude.*)
    one             =  1

instance  Ring Integer  where
    fromInteger     =  id

-- | Units have absolute value 1. Standard associates are non-negative.
instance  StandardAssociate Integer  where
    stdAssociate x  =  if x < 0 then -x else x
    stdUnit x       =  if x < 0 then -1 else 1
    stdRecip        =  stdUnit

-- | `mod` is non-negative
instance  Euclidean Integer  where
    div             =  Prelude.div
    mod             =  Prelude.mod
    euclideanNorm   =  integerToNatural

instance  ToRational Integer  where
    toRational x    =  x :% 1

instance  ToInteger Integer  where
    toInteger       =  id

-------------------------------------------------------------------------
-- instances for Prelude floating point types
-------------------------------------------------------------------------

instance  AdditiveMonoid Float  where
    (+)             =  (Prelude.+)
    zero            =  0

instance  AbelianGroup Float  where
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate
    gtimes n m      =  fromIntegral n * m

instance  Semiring Float  where
    (*)             =  (Prelude.*)
    one             =  1
    rescale         =  rescaleRealFloat

instance  Ring Float  where
    fromInteger     =  Prelude.fromInteger

instance  DivisionSemiring Float  where
    recip           =  (Prelude.recip)

instance  Semifield Float  where
    (/)             =  (Prelude./)

instance  DivisionRing Float

instance  Field Float

instance  FromRational Float  where
    fromRational    =  Prelude.fromRational

instance  ToRational Float  where
    toRational x    =  (m%1)*(b%1)^^n
                       where (m,n) = Prelude.decodeFloat x
                             b     = Prelude.floatRadix  x

instance  AdditiveMonoid Double  where
    (+)             =  (Prelude.+)
    zero            =  0

instance  AbelianGroup Double  where
    (-)             =  (Prelude.-)
    negate          =  Prelude.negate
    gtimes n m      =  fromIntegral n * m

instance  Semiring Double  where
    (*)             =  (Prelude.*)
    one             =  1
    rescale         =  rescaleRealFloat

instance  Ring Double  where
    fromInteger     =  Prelude.fromInteger

instance  DivisionSemiring Double  where
    recip           =  (Prelude.recip)

instance  Semifield Double  where
    (/)             =  (Prelude./)

instance  DivisionRing Double

instance  Field Double

instance  FromRational Double  where
    fromRational    =  Prelude.fromRational

instance  ToRational Double  where
    toRational x    =  (m%1)*(b%1)^^n
                       where (m,n) = Prelude.decodeFloat x
                             b     = Prelude.floatRadix  x

-- rescaling for IEEE floats
rescaleRealFloat :: (RealFloat a) => a -> a -> (a, a, a -> a)
{-# SPECIALISE rescaleRealFloat :: Float -> Float -> (Float, Float, Float -> Float) #-}
{-# SPECIALISE rescaleRealFloat :: Double -> Double -> (Double, Double, Double -> Double) #-}
rescaleRealFloat x y = (scaleFloat mk x, scaleFloat mk y, scaleFloat k)
  where
    k  = max (exponent x) (exponent y)
    mk = - k

-- Numeric functions

-- | Euclid's algorithm, yielding a maximal common divisor of @x@ and @y@,
-- but not necessarily a canonical associate.
euclid           :: (Eq a, Euclidean a) => a -> a -> a
euclid x y
  | y == zero    =  x
  | otherwise    =  euclid y (x `mod` y)

-- | @'gcd' x y@ is a common factor of @x@ and @y@ such that
--
-- * @'stdAssociate' ('gcd' x y) = 'gcd' x y@, and
--
-- * any common factor of @x@ and @y@ is a factor of @'gcd' x y@.
gcd              :: (Eq a, StandardAssociate a, Euclidean a) => a -> a -> a
gcd x y          =  stdAssociate (euclid x y)

-- | @'lcm' x y@ is a common multiple of @x@ and @y@ such that
--
-- * @'stdAssociate' ('lcm' x y) = 'lcm' x y@, and
--
-- * any common multiple of @x@ and @y@ is a multiple of @'lcm' x y@.
lcm              :: (Eq a, StandardAssociate a, Euclidean a) => a -> a -> a
lcm x y
  | y == zero    =  zero
  | x == zero    =  zero
  | otherwise    =  stdAssociate ((x `div` euclid x y) * y)

-------------------------------------------------------------------------
-- instances for other Prelude types
-------------------------------------------------------------------------

-- | Direct product
instance  (AdditiveMonoid a, AdditiveMonoid b) => AdditiveMonoid (a,b)  where
    (x,y) + (x',y') =  (x+x', y+y')
    zero            =  (zero, zero)
    atimes n (x,y)  =  (atimes n x, atimes n y)

-- | Direct product
instance  (AbelianGroup a, AbelianGroup b) => AbelianGroup (a,b)  where
    (x,y) - (x',y') =  (x-x', y-y')
    negate (x,y)    =  (negate x, negate y)
    gtimes n (x,y)  =  (gtimes n x, gtimes n y)

-- | Direct product
instance  (Semiring a, Semiring b) => Semiring (a,b)  where
    (x,y) * (x',y') =  (x*x', y*y')
    one             =  (one, one)
    fromNatural n   =  (fromNatural n, fromNatural n)

-- | Direct product
instance  (Ring a, Ring b) => Ring (a,b)  where
    fromInteger n   =  (fromInteger n, fromInteger n)

-- | Direct product
instance  (FromRational a, FromRational b) => FromRational (a,b)  where
    fromRational n  =  (fromRational n, fromRational n)

-- -----------------------------------------------------------------------------
-- Functions on Ratio

-- | Forms the ratio of two values in a Euclidean domain (e.g. 'Integer').
{-# SPECIALISE (%) :: Integer -> Integer -> Rational #-}
(%)                     :: (Eq a, StandardAssociate a, Euclidean a) =>
                           a -> a -> Ratio a
-- y /= 0 && x % y = n :% d ==>
--      stdAssociate d = d &&
--      x*d = y*n
--      x*d' = y*n' ==> exists a. d' = a*d && n' = a*n
x % y                   =  ((x `div` d) * stdRecip y') :% stdAssociate y'
                           where y' = y `div` d
                                 d = euclid x y

-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator               :: Ratio a -> a
numerator (x :% _)      =  x

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator             :: Ratio a -> a
denominator (_ :% y)    =  y

-- -----------------------------------------------------------------------------
-- Instances of the type Ratio (defined in the standard module Data.Ratio)
-- need to be defined here so that they're not orphans.

instance  (Eq a, StandardAssociate a, Euclidean a) => AdditiveMonoid (Ratio a)  where
    (x:%y) + (x':%y')   =  (x*y' + x'*y) % (y*y')
    zero                =  zero :% one
    atimes n (x:%y)     =  atimes n x % y

instance  (Eq a, StandardAssociate a, Euclidean a, Ring a) => AbelianGroup (Ratio a)  where
    negate (x:%y)       =  (-x) :% y
    gtimes n (x:%y)     =  gtimes n x % y

instance  (Eq a, StandardAssociate a, Euclidean a) => Semiring (Ratio a)  where
    (x:%y) * (x':%y')   =  (x * x') % (y * y')
    one                 =  one :% one
    fromNatural x       =  fromNatural x :% one

instance  (Eq a, StandardAssociate a, Euclidean a, Ring a) => Ring (Ratio a)  where
    fromInteger x       =  fromInteger x :% one

instance  (Eq a, StandardAssociate a, Euclidean a) => DivisionSemiring (Ratio a)  where
    recip (x:%y)        =  y % x

instance  (Eq a, StandardAssociate a, Euclidean a) => Semifield (Ratio a)  where
    (x:%y) / (x':%y')   =  (x*y') % (y*x')

instance  (Eq a, StandardAssociate a, Euclidean a, Ring a) => DivisionRing (Ratio a)

instance  (Eq a, StandardAssociate a, Euclidean a, Ring a) => Field (Ratio a)

instance  (Integral a) => FromRational (Ratio a)  where
    fromRational x      =  fromInteger (numerator x) %
                           fromInteger (denominator x)

-- | The original version of 'Integral' is required here by the old instance
-- @'Ord' ('Ratio' a)@.  Ideally this would be replaced with @'Ord' a@.
instance  (ToInteger a, Prelude.Integral a) => ToRational (Ratio a)  where
    toRational (x:%y)   =  toInteger x :% toInteger y

{- instances defined in Data.Ratio: derived Eq plus

instance  (Ord a, Semiring a) => Ord (Ratio a)  where
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y

ratPrec = 7 :: Int

instance  (Eq a, StandardAssociate a, Euclidean a, Read a) => Read (Ratio a)  where
    readsPrec p  =  readParen (p > ratPrec)
                              (\r -> [(x%y,u) | (x,s)   <- readsPrec (ratPrec+1) r,
                                                ("%",t) <- lex s,
                                                (y,u)   <- readsPrec (ratPrec+1) t ])

instance  (Show a)  => Show (Ratio a)  where
    showsPrec p (x:%y)  =  showParen (p > ratPrec)
                               (showsPrec (ratPrec+1) x . 
                                showString " % " . 
                                showsPrec (ratPrec+1) y)
-}

-- -----------------------------------------------------------------------------
-- Instances of the type Complex (defined in the standard module Data.Complex)
-- need to be defined here so that they're not orphans.

instance  (AdditiveMonoid a) => AdditiveMonoid (Complex a)  where
    {-# SPECIALISE instance AdditiveMonoid (Complex Float) #-}
    {-# SPECIALISE instance AdditiveMonoid (Complex Double) #-}
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    {-# SPECIALISE instance AdditiveMonoid (Complex Float) #-}
    {-# SPECIALISE instance AdditiveMonoid (Complex Double) #-}
    zero                =  zero :+ zero

instance  (AbelianGroup a) => AbelianGroup (Complex a)  where
    {-# SPECIALISE instance AbelianGroup (Complex Float) #-}
    {-# SPECIALISE instance AbelianGroup (Complex Double) #-}
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    negate (x:+y)       =  negate x :+ negate y
    gtimes n (x:+y)     =  gtimes n x :+ gtimes n y

instance  (Ring a) => Semiring (Complex a)  where
    {-# SPECIALISE instance Semiring (Complex Float) #-}
    {-# SPECIALISE instance Semiring (Complex Double) #-}
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    {-# SPECIALISE instance Semiring (Complex Float) #-}
    {-# SPECIALISE instance Semiring (Complex Double) #-}
    one                 =  one :+ zero
    fromNatural n       =  fromNatural n :+ zero

instance  (Ring a) => Ring (Complex a)  where
    {-# SPECIALISE instance Ring (Complex Float) #-}
    {-# SPECIALISE instance Ring (Complex Double) #-}
    fromInteger n       =  fromInteger n :+ zero

-- | Gaussian integers:
-- units have magnitude 1;
-- standard associates are natural numbers or in the positive quadrant.
instance  (Ring a, ToInteger a) => StandardAssociate (Complex a)  where
    {-# SPECIALISE instance StandardAssociate (Complex Int) #-}
    {-# SPECIALISE instance StandardAssociate (Complex Integer) #-}
    stdUnit (x :+ y)
      | y > 0 && x <= 0 =  0 :+ 1
      | x < 0 && y <= 0 =  -1
      | y < 0 && x >= 0 =  0 :+ (-1)
      | otherwise       =  1
    stdRecip (x :+ y)
      | y > 0 && x <= 0 =  0 :+ (-1)
      | x < 0 && y <= 0 =  -1
      | y < 0 && x >= 0 =  0 :+ 1
      | otherwise       =  1

-- | Gaussian integers:
-- if @b@ is non-zero, the norm (squared magnitude) of @'mod' a b@
-- is at most half that of @b@.
instance  (Ring a, ToInteger a) => Euclidean (Complex a)  where
    {-# SPECIALISE instance Euclidean (Complex Int) #-}
    {-# SPECIALISE instance Euclidean (Complex Integer) #-}
    (x:+y) `div` (x':+y')
                        =  round_div (x*x' + y*y') :+ round_div (x'*y - x*y')
                           where round_div i = (2*i + n) `div` (2*n)
                                 n = x'*x' + y'*y'
    a `mod` b           =  a - b*(a `div` b)
    divMod a b          =  (q, a - b*q)
                           where q = a `div` b

    euclideanNorm (x:+y)
                        = euclideanNorm (toInteger (x*x)) + 
                          euclideanNorm (toInteger (y*y))

instance  (FromRational a) => FromRational (Complex a)  where
    fromRational a      =  fromRational a :+ 0

instance  (Field a) => DivisionSemiring (Complex a)  where
    {-# SPECIALISE instance DivisionSemiring (Complex Float) #-}
    {-# SPECIALISE instance DivisionSemiring (Complex Double) #-}
    recip (x:+y)        =  x' / d :+ (-y') / d
                           where (x', y', _) = rescale x y
                                 d   = x*x' + y*y'

instance  (Field a) => Semifield (Complex a)  where
    {-# SPECIALISE instance Semifield (Complex Float) #-}
    {-# SPECIALISE instance Semifield (Complex Double) #-}
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where (x'', y'', _) = rescale x' y'
                                 d   = x'*x'' + y'*y''

{-
instance  (Field a) => Field (Complex a)  where
    (x:+y) / (x':+y')   =  (x*x'+y*y') / d :+ (y*x'-x*y') / d
                           where d   = x'*x' + y'*y'
-}

instance  (Field a) => DivisionRing (Complex a)

instance  (Field a) => Field (Complex a)

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

-- | Haskell 98 compatibility class
class  (Num a, ToRational a) => Real a  where

-- | Subsets of 'Integer', supporting integer division.
class  (Enum a, Real a, ToInteger a) => Integral a  where
    -- | Integer division truncated toward zero
    quot                :: a -> a -> a
    -- | Integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    rem                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    quotRem          :: a -> a -> (a,a)

    n `quot` d       =  q  where (q,_) = quotRem n d
    n `rem` d        =  r  where (_,r) = quotRem n d
    quotRem n d      =  if signum r == - signum d then (q+1, r-d) else qr
                        where qr@(q,r) = divMod n d

-- | Haskell 98 compatibility class
class  (Num a, FromRational a, Field a) => Fractional a  where

-------------------------------------------------------------------------
-- unchanged Haskell 98 classes
-------------------------------------------------------------------------

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
                                _  -> error "round default defn: Bad value"
    
    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x
    
    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x

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

-------------------------------------------------------------------------
-- Numeric functions
-------------------------------------------------------------------------

-- | The input value divides evenly by 2.
even             :: (ToInteger a) => a -> Bool
even n           =  n `mod` two == zero
                    where two = one + one

-- | The input value does not divide evenly by 2.
odd              :: (ToInteger a) => a -> Bool
odd              =  not . even

-- | raise a number to a non-negative integral power
(^)              :: (Semiring a, ToInteger b) => a -> b -> a
x ^ n
  | n >= zero    =  natPower x n one
  | otherwise    =  error "Prelude.^: negative exponent"

natPower :: (Semiring a, ToInteger b) => a -> b -> a -> a
natPower x n y
  | n == zero = y
  | even n    = natPower (x*x) (n `div` two) y
  | otherwise = natPower (x*x) (n `div` two) (x*y)
  where
    two = one + one

-- | raise a number to an integral power
(^^)             :: (DivisionSemiring a, AbelianGroup b, ToInteger b) => a -> b -> a
x ^^ n           =  if n >= zero then x^n else recip (x^(-n))

-- | General conversion from integral types, via the 'Integer' type.
fromIntegral     :: (ToInteger a, Ring b) => a -> b
fromIntegral     =  fromInteger . toInteger

-- | General conversion to fractional types, via the 'Rational' type.
realToFrac       :: (ToRational a, FromRational b) => a -> b
realToFrac       =  fromRational . toRational

-------------------------------------------------------------------------
-- instances for Prelude numeric types
-------------------------------------------------------------------------

-- | As in "Prelude".
instance  Num Int  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

-- | As in "Prelude".
instance  Real Int  where

-- | As in "Prelude".
instance  Integral Int  where

-- | As in "Prelude".
instance  Num Word  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

-- | As in "Prelude".
instance  Real Word  where

-- | As in "Prelude".
instance  Integral Word  where

-- | As in "Prelude".
instance  Num Integer  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

-- | As in "Prelude".
instance  Real Integer  where

-- | As in "Prelude".
instance  Integral Integer  where

-- | As in "Prelude".
instance  Num Float  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

-- | As in "Prelude".
instance  Real Float  where

-- | As in "Prelude".
instance  Fractional Float  where

-- | As in "Prelude".
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

-- | As in "Prelude".
instance  RealFrac Float  where
    properFraction x =  (fromInteger n, r)
                        where (n,r) = Prelude.properFraction x

-- | As in "Prelude".
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

-- | As in "Prelude".
instance  Num Double  where
    abs             =  Prelude.abs
    signum          =  Prelude.signum

-- | As in "Prelude".
instance  Real Double  where

-- | As in "Prelude".
instance  Fractional Double  where

-- | As in "Prelude".
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

-- | As in "Prelude".
instance  RealFrac Double  where
    properFraction x =  (fromInteger n, r)
                        where (n,r) = Prelude.properFraction x

-- | As in "Prelude".
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

-- | As in "Data.Ratio".
instance  (Integral a)  => Num (Ratio a)  where
    abs (x:%y)          =  abs x :% y
    signum (x:%_)       =  signum x :% 1

-- -----------------------------------------------------------------------------
-- Instances of Ratio

-- | As in "Data.Ratio".  The original version of 'Integral' is required
-- here by the old instance @'Ord' ('Ratio' a)@.  Ideally there would
-- be only one.
instance  (Integral a, Prelude.Integral a)  => Real (Ratio a)  where

-- | As in "Data.Ratio".
instance  (Integral a)  => Fractional (Ratio a)  where

-- | As in "Data.Ratio".  The original version of 'Integral' is required
-- here by the old instance @'Ord' ('Ratio' a)@.  Ideally there would
-- be only one.
instance  (Integral a, Prelude.Integral a)  => RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
                            where (q,r) = quotRem x y

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | The nonnegative magnitude of a complex number.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (Floating a) => Complex a -> a
magnitude (x:+y) =  scale (sqrt (sqr x' + sqr y'))
                    where (x', y', scale) = rescale x y
                          sqr z = z * z

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
--
-- 'RealFloat' is needed for 'atan2'.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0)   = 0            -- SLPJ July 97 from John Peterson
phase (x:+y)     = atan2 y x

-- -----------------------------------------------------------------------------
-- Instances of Complex (largely broken)

-- | As in "Data.Complex".
instance  (RealFloat a) => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    abs z               =  magnitude z :+ 0
    signum (0:+0)       =  0
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z

-- | As in "Data.Complex".
instance  (RealFloat a) => Fractional (Complex a)  where

-- sqrt uses (<) and abs
-- log uses phase, which uses atan2
-- inverse trig and hyp functions use sqrt and log

-- | As in "Data.Complex".
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

    sinh (x:+y)    =  cos y * sinh x :+ sin y * cosh x
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

-- | Standard implementation of if-then-else, needed for @RebindableSyntax@.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
