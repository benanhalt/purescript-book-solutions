module Test.MySolutions where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Array (nubByEq, length)
import Data.Hashable

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) =
    show real
    <> (if imaginary < 0.0 then "" else "+")
    <> (show imaginary)
    <> "i"

derive instance eqComplex :: Eq Complex

instance semiringComplex :: Semiring Complex where
  zero = Complex {real: 0.0, imaginary: 0.0}

  add (Complex z1) (Complex z2) =
    Complex { real: z1.real + z2.real,
              imaginary: z1.imaginary + z2.imaginary
            }

  mul (Complex z1) (Complex z2) =
    Complex { real: z1.real * z2.real - z1.imaginary * z2.imaginary
            , imaginary: z1.real * z2.imaginary + z1.imaginary * z2.real
            }

  one = Complex {real: 1.0, imaginary: 0.0}

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

derive instance eqNonEmpty :: Eq a => Eq (NonEmpty a)
derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show = genericShow

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a as) (NonEmpty b bs) = NonEmpty a (as <> [b] <> bs)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a as) = NonEmpty (f a) (f <$> as)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f b (NonEmpty a as) = foldl f (f b a) as
  foldr f b (NonEmpty a as) = f a $ foldr f b as
  foldMap f as = foldl (<>) mempty $ f <$> as

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

data OneMore f a = OneMore a (f a)

instance functorOneMore :: Functor f => Functor (OneMore f) where
  map func (OneMore a as) = OneMore (func a) (func <$> as)

instance foldableOneMore :: (Functor f, Foldable f) => Foldable (OneMore f) where
  foldl f b (OneMore a as) = foldl f (f b a) as
  foldr f b (OneMore a as) = f a $ foldr f b as
  foldMap f as = foldl (<>) mempty $ f <$> as


unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum ns = case maximum ns of
  Just n -> n


class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance eqMultiply :: Eq Multiply
derive newtype instance showMultiply :: Show Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) m = n * m

-- instance actionMultiplyInt :: Action Multiply Int where
--   act (Multiply n) m = m / n

-- instance actionMultiplyInt :: Action Multiply Int where
--   act (Multiply n) m = m `pow` n

instance actionMuliplyString :: Action Multiply String where
  act (Multiply n) s = s `power` n

instance actionOverArray :: Action m a => Action m (Array a) where
  act m as = act m <$> as


newtype Self m = Self m

derive newtype instance eqSelf :: Eq m => Eq (Self m)
derive newtype instance showSelf :: Show m => Show (Self m)

instance actionOnSelf :: Monoid m => Action m (Self m) where
  act m (Self m') = Self $ m <> m'

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates as = length as /= length distinct
  where
    distinct = nubByEq (\a b -> hashEqual a b && (a == b)) as

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour n) = hash $ mod n 12
