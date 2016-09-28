module Data.Monoid.Conj where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Monad (class Monad)

import Data.HeytingAlgebra (class HeytingAlgebra, conj, disj, ff, tt)
import Data.Bounded (class Bounded, top, bottom)
import Data.Eq (class Eq, (==))
import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord, compare)
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring)
import Data.Show (class Show, show)

-- | Monoid under conjuntion.
-- |
-- | ``` purescript
-- | Conj x <> Conj y == Conj (x && y)
-- | mempty :: Conj _ == Conj top
-- | ```
newtype Conj a = Conj a

runConj :: forall a. Conj a -> a
runConj (Conj x) = x

instance eqConj :: Eq a => Eq (Conj a) where
  eq (Conj x) (Conj y) = x == y

instance ordConj :: Ord a => Ord (Conj a) where
  compare (Conj x) (Conj y) = compare x y

instance boundedConj :: Bounded a => Bounded (Conj a) where
  top = Conj top
  bottom = Conj bottom

instance functorConj :: Functor Conj where
  map f (Conj x) = Conj (f x)

instance invariantConj :: Invariant Conj where
  imap f _ (Conj x) = Conj (f x)

instance applyConj :: Apply Conj where
  apply (Conj f) (Conj x) = Conj (f x)

instance applicativeConj :: Applicative Conj where
  pure = Conj

instance bindConj :: Bind Conj where
  bind (Conj x) f = f x

instance monadConj :: Monad Conj

instance extendConj :: Extend Conj where
  extend f x = Conj (f x)

instance comonadConj :: Comonad Conj where
  extract = runConj

instance showConj :: (Show a) => Show (Conj a) where
  show (Conj a) = "(Conj " <> show a <> ")"

instance semigroupConj :: HeytingAlgebra a => Semigroup (Conj a) where
  append (Conj a) (Conj b) = Conj (conj a b)

instance monoidConj :: HeytingAlgebra a => Monoid (Conj a) where
  mempty = Conj tt

instance semiringConj :: HeytingAlgebra a => Semiring (Conj a) where
  zero = Conj tt
  one = Conj ff
  add (Conj a) (Conj b) = Conj (conj a b)
  mul (Conj a) (Conj b) = Conj (disj a b)
