module Exercise2 where

import Prelude
import Data.Array (cons)

newtype Complex = Complex
 { real :: Number
 , imaginary :: Number
 }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = (show real) <> " + " <> (show imaginary) <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex o1) (Complex o2) = o1.real == o2.real
                                 && o1.imaginary == o2.imaginary

data NonEmpty a = NonEmpty a (Array a)

exNonEmpty :: NonEmpty Int
exNonEmpty = NonEmpty 1 []

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = "(NonEmpty " <> (show x) <> " " <> (show xs) <> ")"

instance semiNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x1 xs1) (NonEmpty x2 xs2) =
    NonEmpty x1 (append xs1 (cons x2 xs2))

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)
